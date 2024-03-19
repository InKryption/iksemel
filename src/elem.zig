/// Expects that the last value returned by `tokenizer.nextType(.non_markup)`
/// to have been `.angle_bracket_left`.
pub inline fn sliceScanner(tokenizer: *Tokenizer) Scanner(null) {
    return .{
        .tokenizer = tokenizer,
        .mbr = .{
            .reader = {},
            .read_buffer = {},
        },
    };
}

/// Expects that the last value returned by `tokenizer.nextType(.non_markup)`
/// to have been `.angle_bracket_left`. `reader` and `read_buf` will be used
/// as the source to feed the tokenizer.
pub inline fn readerScanner(
    tokenizer: *Tokenizer,
    reader: anytype,
    read_buf: []u8,
) Scanner(@TypeOf(reader)) {
    return .{
        .tokenizer = tokenizer,
        .mbr = .{
            .reader = reader,
            .read_buffer = read_buf,
        },
    };
}

pub fn Scanner(comptime MaybeReader: ?type) type {
    return struct {
        tokenizer: *Tokenizer,
        mbr: parse_helper.MaybeBufferedReader(MaybeReader),
        state: State = .@"<",
        segment_iter: parse_helper.TokenSrcIter(MaybeReader) = .{},
        const Self = @This();

        pub const Segment = if (MaybeReader != null)
            []const u8
        else
            Tokenizer.Range;

        pub inline fn nextMarker(scanner: *Self) !ScanMarker {
            return nextMarkerOrSegmentImpl(MaybeReader, scanner, .marker);
        }

        /// Returns segments of a string, either as a slice or a range,
        /// based upon whether this is backed by a streaming or
        /// non-streaming Tokenizer. A full string is terminated by
        /// a null sentinel value.
        /// Not all segments are guaranteed to be consecutive
        /// in the source, except for when the context does not
        /// allow for the possibility of comments ('<!-- -->'), ie
        /// an attribute value.
        pub fn nextSegment(scanner: *Self) !?Segment {
            return nextMarkerOrSegmentImpl(MaybeReader, scanner, .segment);
        }
    };
}

pub const ScanError = error{
    UnexpectedToken,
    UnexpectedEof,
};

pub const ScanMarker = enum {
    eof,

    /// `nextSegment` will return the segments of the PI.
    pi,

    /// `nextSegment` will return the segments of the text.
    text,
    //// `nextSegment` will return the segments of the reference name.
    reference,
    //// `nextSegment` will return the segments of the character reference number string.
    char_reference,

    /// First, `nextSegment` will return the segments of the element name.
    /// Secondly, `nextMarker` will return one of:
    /// * `.attribute_start`
    /// * `.element_open_end`
    /// * `.element_open_end_inline_close`
    /// If it's `.attribute_start`, follow retrieve the attribute information,
    /// and then repeat the second step here.
    element_open_start,

    /// First, `nextSegment` will return the segments of the attribute name.
    /// Secondly, `nextMarker` will return one of:
    /// * `.text`
    /// * `.reference`
    /// * `.attribute_end`
    /// If it's anything other than `.attribute_end` (`.text` or `.reference`),
    /// repeat the second step here.
    attribute_start,
    attribute_end,

    /// `nextMarker` will return one of:
    /// * `.text`
    /// * `.reference`
    /// * `.element_open_start`
    element_open_end,
    /// Same as `.element_open_end`, except a new scope isn't opened.
    element_open_end_inline_close,

    /// `nextSegment` will return the segments of the element name.
    element_close,
};

const State = union(enum) {
    non_markup: ?Tokenizer.TokenType,
    cdata,

    pi,

    @"<",
    @"</",
    element_tag,
    attr_value_sq,
    attr_value_dq,

    @"sq,&",
    @"dq,&",
    @"&",
};

fn nextMarkerOrSegmentImpl(
    comptime MaybeReader: ?type,
    scanner: *Scanner(MaybeReader),
    comptime ret_type: enum { marker, segment },
) !switch (ret_type) {
    .marker => ScanMarker,
    .segment => ?Scanner(MaybeReader).Segment,
} {
    const tokenizer: *Tokenizer = scanner.tokenizer;
    const mbr = scanner.mbr;
    return mainloop: while (true) break switch (scanner.state) {
        .non_markup => |*cached_tt| switch (ret_type) {
            .marker => switch (blk: {
                defer cached_tt.* = null;
                break :blk cached_tt.* orelse try parse_helper.nextTokenType(tokenizer, .non_markup, MaybeReader, mbr);
            }) {
                .eof => break .eof,

                .comment_start => switch (try parse_helper.handleCommentSkip(tokenizer, MaybeReader, mbr)) {
                    .normal_end => continue,
                    .invalid_end_triple_dash => return ScanError.UnexpectedToken,
                    .invalid_dash_dash => return ScanError.UnexpectedToken,
                    .eof => return ScanError.UnexpectedEof,
                },

                .angle_bracket_left => {
                    scanner.state = .@"<";
                    continue;
                },

                .ampersand => switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
                    .tag_token => {
                        scanner.segment_iter = .{};
                        scanner.state = .@"&";
                        return .reference;
                    },
                    .hashtag => {
                        switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
                            else => return ScanError.UnexpectedToken,
                            .tag_token => {},
                        }
                        scanner.segment_iter = .{};
                        scanner.state = .@"&";
                        return .char_reference;
                    },
                    else => return ScanError.UnexpectedToken,
                },

                .pi_start => {
                    switch (try parse_helper.nextTokenType(tokenizer, .pi, MaybeReader, mbr)) {
                        .eof => return ScanError.UnexpectedEof,
                        .pi_end => return ScanError.UnexpectedToken,
                        .text_data => {},
                        else => unreachable,
                    }
                    scanner.state = .pi;
                    scanner.segment_iter = .{};
                    return .pi;
                },
                .cdata_start => {
                    switch (try parse_helper.nextTokenType(tokenizer, .cdata, MaybeReader, mbr)) {
                        else => return ScanError.UnexpectedToken,
                        .eof => return ScanError.UnexpectedEof,
                        .cdata_end => continue,
                        .text_data => {},
                    }
                    scanner.state = .cdata;
                    scanner.segment_iter = .{};
                    return .text;
                },
                .text_data => {
                    scanner.segment_iter = .{};
                    return .text;
                },
                else => return ScanError.UnexpectedToken,
            },

            .segment => {
                if (try scanner.segment_iter.next(tokenizer, .non_markup, mbr)) |segment| return segment;
                scanner.segment_iter = undefined;

                while (true) switch (try parse_helper.nextTokenType(tokenizer, .non_markup, MaybeReader, mbr)) {
                    .text_data => {
                        scanner.segment_iter = .{};
                        continue :mainloop;
                    },
                    .cdata_start => {
                        switch (try parse_helper.nextTokenType(tokenizer, .cdata, MaybeReader, mbr)) {
                            else => return ScanError.UnexpectedToken,
                            .eof => return ScanError.UnexpectedEof,
                            .cdata_end => continue,
                            .text_data => {},
                        }
                        scanner.state = .cdata;
                        scanner.segment_iter = .{};
                        continue :mainloop;
                    },
                    .comment_start => switch (try parse_helper.handleCommentSkip(tokenizer, MaybeReader, mbr)) {
                        .normal_end => continue,
                        .invalid_end_triple_dash => return ScanError.UnexpectedToken,
                        .invalid_dash_dash => return ScanError.UnexpectedToken,
                        .eof => return ScanError.UnexpectedEof,
                    },
                    else => |tt| {
                        cached_tt.* = tt;
                        break;
                    },
                };

                return null;
            },
        },

        .cdata => switch (ret_type) {
            .marker => unreachable,
            .segment => {
                if (try scanner.segment_iter.next(tokenizer, .cdata, mbr)) |segment| return segment;
                scanner.segment_iter = undefined;

                switch (try parse_helper.nextTokenType(tokenizer, .cdata, MaybeReader, mbr)) {
                    else => unreachable,
                    .text_data => unreachable,
                    .eof => return ScanError.UnexpectedEof,
                    .cdata_end => {},
                }

                while (true) switch (try parse_helper.nextTokenType(tokenizer, .non_markup, MaybeReader, mbr)) {
                    .eof => return ScanError.UnexpectedEof,
                    .text_data => {
                        scanner.state = .{ .non_markup = null };
                        scanner.segment_iter = .{};
                        continue :mainloop;
                    },
                    .cdata_start => {
                        switch (try parse_helper.nextTokenType(tokenizer, .cdata, MaybeReader, mbr)) {
                            else => return ScanError.UnexpectedToken,
                            .eof => return ScanError.UnexpectedEof,
                            .cdata_end => continue,
                            .text_data => {},
                        }
                        scanner.segment_iter = .{};
                        continue :mainloop;
                    },
                    .comment_start => switch (try parse_helper.handleCommentSkip(tokenizer, MaybeReader, mbr)) {
                        .normal_end => continue,
                        .invalid_end_triple_dash => return ScanError.UnexpectedToken,
                        .invalid_dash_dash => return ScanError.UnexpectedToken,
                        .eof => return ScanError.UnexpectedEof,
                    },
                    else => |tt| {
                        scanner.state = .{ .non_markup = tt };
                        break;
                    },
                };

                return null;
            },
        },

        .pi => switch (ret_type) {
            .marker => unreachable,
            .segment => {
                if (try scanner.segment_iter.next(tokenizer, .pi, mbr)) |segment| return segment;
                scanner.segment_iter = undefined;

                switch (try parse_helper.nextTokenType(tokenizer, .pi, MaybeReader, mbr)) {
                    else => unreachable,
                    .text_data => unreachable,
                    .eof => return ScanError.UnexpectedEof,
                    .pi_end => {},
                }

                scanner.state = .{ .non_markup = null };
                return null;
            },
        },

        .@"<" => switch (ret_type) {
            .marker => switch (try parse_helper.nextTokenType(tokenizer, .element_tag, MaybeReader, mbr)) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,

                .slash => {
                    scanner.state = .@"</";
                    continue;
                },
                .tag_token => {
                    scanner.segment_iter = .{};
                    return .element_open_start;
                },
            },
            .segment => {
                if (try scanner.segment_iter.next(tokenizer, .element_tag, mbr)) |segment| return segment;
                scanner.segment_iter = undefined;
                scanner.state = .element_tag;
                return null;
            },
        },

        .@"</" => switch (ret_type) {
            .marker => switch (try parse_helper.nextTokenType(tokenizer, .element_tag, MaybeReader, mbr)) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,
                .tag_token => {
                    scanner.segment_iter = .{};
                    return .element_close;
                },
            },
            .segment => {
                if (try scanner.segment_iter.next(tokenizer, .element_tag, mbr)) |segment| return segment;
                scanner.segment_iter = undefined;
                switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .element_tag, MaybeReader, mbr)) {
                    else => return ScanError.UnexpectedToken,
                    .eof => return ScanError.UnexpectedEof,
                    .tag_whitespace => unreachable,
                    .angle_bracket_right => {},
                }
                scanner.state = .{ .non_markup = null };
                return null;
            },
        },

        .element_tag => switch (ret_type) {
            .marker => switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .element_tag, MaybeReader, mbr)) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,
                .tag_whitespace => unreachable,

                .tag_token => {
                    scanner.segment_iter = .{};
                    return .attribute_start;
                },

                .slash => {
                    switch (try parse_helper.nextTokenType(tokenizer, .element_tag, MaybeReader, mbr)) {
                        else => return ScanError.UnexpectedToken,
                        .eof => return ScanError.UnexpectedEof,
                        .angle_bracket_right => {},
                    }
                    scanner.state = .{ .non_markup = null };
                    return .element_open_end_inline_close;
                },
                .angle_bracket_right => {
                    scanner.state = .{ .non_markup = null };
                    return .element_open_end;
                },
            },
            .segment => {
                if (try scanner.segment_iter.next(tokenizer, .element_tag, mbr)) |segment| return segment;
                scanner.segment_iter = undefined;

                switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .element_tag, MaybeReader, mbr)) {
                    else => return ScanError.UnexpectedToken,
                    .eof => return ScanError.UnexpectedEof,
                    .tag_whitespace => unreachable,
                    .equals => {},
                }

                scanner.state = switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .element_tag, MaybeReader, mbr)) {
                    else => return ScanError.UnexpectedToken,
                    .eof => return ScanError.UnexpectedEof,
                    .tag_whitespace => unreachable,
                    .quote_single => .attr_value_sq,
                    .quote_double => .attr_value_dq,
                };
                return null;
            },
        },

        inline //
        .attr_value_sq,
        .attr_value_dq,
        => |_, quote_state| {
            const str_ctx: Tokenizer.Context = switch (quote_state) {
                .attr_value_sq => .attribute_value_quote_single,
                .attr_value_dq => .attribute_value_quote_double,
                else => unreachable,
            };
            switch (ret_type) {
                .marker => switch (try parse_helper.nextTokenType(tokenizer, str_ctx, MaybeReader, mbr)) {
                    else => return ScanError.UnexpectedToken,
                    .eof => return ScanError.UnexpectedEof,

                    .text_data => {
                        scanner.segment_iter = .{};
                        return .text;
                    },
                    .ampersand => switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
                        .tag_token => {
                            scanner.segment_iter = .{};
                            scanner.state = switch (quote_state) {
                                .attr_value_sq => .@"sq,&",
                                .attr_value_dq => .@"dq,&",
                                else => unreachable,
                            };
                            return .reference;
                        },
                        .hashtag => {
                            switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
                                else => return ScanError.UnexpectedToken,
                                .tag_token => {},
                            }
                            scanner.segment_iter = .{};
                            scanner.state = switch (quote_state) {
                                .attr_value_sq => .@"sq,&",
                                .attr_value_dq => .@"dq,&",
                                else => unreachable,
                            };
                            return .char_reference;
                        },
                        else => return ScanError.UnexpectedToken,
                    },

                    .quote_single,
                    .quote_double,
                    => {
                        scanner.state = .element_tag;
                        return .attribute_end;
                    },
                },
                .segment => {
                    if (try scanner.segment_iter.next(tokenizer, str_ctx, mbr)) |segment| return segment;
                    scanner.segment_iter = undefined;
                    return null;
                },
            }
        },

        inline //
        .@"sq,&",
        .@"dq,&",
        .@"&",
        => |_, curr_state| switch (ret_type) {
            .marker => unreachable,
            .segment => {
                if (try scanner.segment_iter.next(tokenizer, .reference, mbr)) |segment| return segment;
                scanner.segment_iter = undefined;

                switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
                    else => return ScanError.UnexpectedToken,
                    .semicolon => {},
                }

                scanner.state = switch (curr_state) {
                    .@"sq,&" => .attr_value_sq,
                    .@"dq,&" => .attr_value_dq,
                    .@"&" => .{ .non_markup = null },
                    else => unreachable,
                };
                return null;
            },
        },
    };
}

test sliceScanner {
    const nextSegmentStr = struct {
        fn nextSegmentStr(scanner: anytype) !?[]const u8 {
            if (@TypeOf(scanner.mbr) != parse_helper.MaybeBufferedReader(null)) {
                return scanner.nextSegment();
            }
            const maybe_segment = try scanner.nextSegment();
            const segment = maybe_segment orelse return null;
            return segment.toStr(scanner.tokenizer.src);
        }
    }.nextSegmentStr;

    var tokenizer = Tokenizer.initComplete(
        \\<foo bar= "baz&a;"> lorem ipsum&lt; </foo>
    );
    try std.testing.expectEqual(.angle_bracket_left, tokenizer.nextTypeNoUnderrun(.non_markup));

    var scanner = sliceScanner(&tokenizer);

    try std.testing.expectEqual(.element_open_start, scanner.nextMarker());
    try std.testing.expectEqualDeep("foo", nextSegmentStr(&scanner));
    try std.testing.expectEqualDeep(null, nextSegmentStr(&scanner));

    try std.testing.expectEqual(.attribute_start, scanner.nextMarker());

    try std.testing.expectEqualDeep("bar", nextSegmentStr(&scanner));
    try std.testing.expectEqualDeep(null, nextSegmentStr(&scanner));

    try std.testing.expectEqual(.text, scanner.nextMarker());
    try std.testing.expectEqualDeep("baz", nextSegmentStr(&scanner));
    try std.testing.expectEqualDeep(null, nextSegmentStr(&scanner));

    try std.testing.expectEqual(.reference, scanner.nextMarker());
    try std.testing.expectEqualDeep("a", nextSegmentStr(&scanner));
    try std.testing.expectEqualDeep(null, nextSegmentStr(&scanner));

    try std.testing.expectEqual(.attribute_end, scanner.nextMarker());
    try std.testing.expectEqual(.element_open_end, scanner.nextMarker());

    try std.testing.expectEqual(.text, scanner.nextMarker());
    try std.testing.expectEqualDeep(" lorem ipsum", nextSegmentStr(&scanner));
    try std.testing.expectEqualDeep(null, nextSegmentStr(&scanner));

    try std.testing.expectEqual(.reference, scanner.nextMarker());
    try std.testing.expectEqualDeep("lt", nextSegmentStr(&scanner));
    try std.testing.expectEqualDeep(null, nextSegmentStr(&scanner));

    try std.testing.expectEqual(.text, scanner.nextMarker());
    try std.testing.expectEqualDeep(" ", nextSegmentStr(&scanner));
    try std.testing.expectEqualDeep(null, nextSegmentStr(&scanner));

    try std.testing.expectEqual(.element_close, scanner.nextMarker());
    try std.testing.expectEqualDeep("foo", nextSegmentStr(&scanner));
    try std.testing.expectEqualDeep(null, nextSegmentStr(&scanner));

    try std.testing.expectEqual(.eof, scanner.nextMarker());
}

const ScannerTestItem = union(enum) {
    marker: ScanMarker,
    str: []const u8,
};
fn testScanner(
    buffer_sizes: []const usize,
    src: []const u8,
    expected_items: []const ScannerTestItem,
) !void {
    var str_buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer str_buffer.deinit();

    {
        const max_buffer = try std.testing.allocator.alloc(u8, std.mem.max(usize, buffer_sizes));
        defer std.testing.allocator.free(max_buffer);

        for (buffer_sizes) |buffer_size| {
            const read_buffer = max_buffer[0..buffer_size];
            var fbs = std.io.fixedBufferStream(src);
            var tokenizer = Tokenizer.initStreaming();

            var scanner = readerScanner(&tokenizer, fbs.reader(), read_buffer);
            for (expected_items, 0..) |expected_item, i| {
                errdefer std.log.err("Error occurred on item {d}", .{i});
                switch (expected_item) {
                    .marker => |marker| try std.testing.expectEqual(marker, scanner.nextMarker()),
                    .str => |str| {
                        while (try scanner.nextSegment()) |segment| {
                            try str_buffer.appendSlice(segment);
                        }
                        try std.testing.expectEqualStrings(str, str_buffer.items);
                        str_buffer.clearRetainingCapacity();
                    },
                }
            }
            try std.testing.expectEqual(.eof, scanner.nextMarker());
        }
    }

    var tokenizer = Tokenizer.initComplete(src);

    var scanner = sliceScanner(&tokenizer);
    for (expected_items, 0..) |expected_item, i| {
        errdefer std.log.err("Error occurred on item {d}", .{i});
        switch (expected_item) {
            .marker => |marker| try std.testing.expectEqual(marker, scanner.nextMarker()),
            .str => |str| {
                while (try scanner.nextSegment()) |segment| {
                    try str_buffer.appendSlice(segment.toStr(src));
                }
                try std.testing.expectEqualStrings(str, str_buffer.items);
                str_buffer.clearRetainingCapacity();
            },
        }
    }
    try std.testing.expectEqual(.eof, scanner.nextMarker());
}

test "1" {
    try testScanner(
        &.{
            1,  2,  3,  4,  5,  6,  7,  8,  10,  12,  14,  16,  20,  24,
            28, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256,
        },
        \\foo>
        \\  <fizz buzz="fizzbuzz" e="&#x4123;"/>
        \\  &lt;not markup&gt;
        \\  <?custom stuff?>
        \\  <!-- comment -->
        \\  <![CDATA[<cdata text>]]>
        \\  <bar><baz a="&quot;" b= "c&d;" e ='&f;g' h = 'i&j;k' l='&m;n&o;'></baz></bar>
        \\</foo>
        \\
    ,
        &[_]ScannerTestItem{
            .{ .marker = .element_open_start },
            .{ .str = "foo" },
            .{ .marker = .element_open_end },

            .{ .marker = .text },
            .{ .str = "\n  " },

            .{ .marker = .element_open_start },
            .{ .str = "fizz" },

            .{ .marker = .attribute_start },
            .{ .str = "buzz" },
            .{ .marker = .text },
            .{ .str = "fizzbuzz" },
            .{ .marker = .attribute_end },

            .{ .marker = .attribute_start },
            .{ .str = "e" },
            .{ .marker = .char_reference },
            .{ .str = "x4123" },
            .{ .marker = .attribute_end },

            .{ .marker = .element_open_end_inline_close },

            .{ .marker = .text },
            .{ .str = "\n  " },
            .{ .marker = .reference },
            .{ .str = "lt" },
            .{ .marker = .text },
            .{ .str = "not markup" },
            .{ .marker = .reference },
            .{ .str = "gt" },
            .{ .marker = .text },
            .{ .str = "\n  " },

            .{ .marker = .pi },
            .{ .str = "custom stuff" },

            .{ .marker = .text },
            .{ .str = "\n  \n  <cdata text>\n  " },

            .{ .marker = .element_open_start },
            .{ .str = "bar" },
            .{ .marker = .element_open_end },

            .{ .marker = .element_open_start },
            .{ .str = "baz" },

            .{ .marker = .attribute_start },
            .{ .str = "a" },
            .{ .marker = .reference },
            .{ .str = "quot" },
            .{ .marker = .attribute_end },

            .{ .marker = .attribute_start },
            .{ .str = "b" },
            .{ .marker = .text },
            .{ .str = "c" },
            .{ .marker = .reference },
            .{ .str = "d" },
            .{ .marker = .attribute_end },

            .{ .marker = .attribute_start },
            .{ .str = "e" },
            .{ .marker = .reference },
            .{ .str = "f" },
            .{ .marker = .text },
            .{ .str = "g" },
            .{ .marker = .attribute_end },

            .{ .marker = .attribute_start },
            .{ .str = "h" },
            .{ .marker = .text },
            .{ .str = "i" },
            .{ .marker = .reference },
            .{ .str = "j" },
            .{ .marker = .text },
            .{ .str = "k" },
            .{ .marker = .attribute_end },

            .{ .marker = .attribute_start },
            .{ .str = "l" },
            .{ .marker = .reference },
            .{ .str = "m" },
            .{ .marker = .text },
            .{ .str = "n" },
            .{ .marker = .reference },
            .{ .str = "o" },
            .{ .marker = .attribute_end },

            .{ .marker = .element_open_end },

            .{ .marker = .element_close },
            .{ .str = "baz" },
            .{ .marker = .element_close },
            .{ .str = "bar" },

            .{ .marker = .text },
            .{ .str = "\n" },

            .{ .marker = .element_close },
            .{ .str = "foo" },

            .{ .marker = .text },
            .{ .str = "\n" },
        },
    );
}

const std = @import("std");
const assert = std.debug.assert;

const builtin = @import("builtin");

const iksemel = @import("iksemel.zig");
const Tokenizer = iksemel.Tokenizer;

const parse_helper = @import("parse_helper.zig");
