const std = @import("std");
const assert = std.debug.assert;

const builtin = @import("builtin");

const iksemel = @import("iksemel.zig");
const Tokenizer = iksemel.Tokenizer;

const parse_helper = @import("parse_helper.zig");

pub fn ParseCtx(comptime Impl: type) type {
    return struct {
        inner: Impl,
        const Self = @This();

        /// Called consecutively to construct the source.
        /// If the source represents text outside of markup tags,
        /// the segments of the whole string are not guaranteed
        /// to be contiguous in the source, due to the possibility
        /// of interspersed comments and CDATA Sections.
        /// Otherwise, if the source represents text inside of a
        /// markup tag, such as in attributes and PI sections,
        /// each segment of the whole string is guaranteed to be
        /// contiguous.
        /// Terminated when `segment == null`.
        pub fn feedSrc(
            ctx: Self,
            /// * `?[]const u8`
            /// * `?Tokenizer.Range`
            segment: anytype,
        ) !void {
            switch (@TypeOf(segment)) {
                ?[]const u8, ?Tokenizer.Range => {},
                else => @compileError(
                    "" ++
                        "Expected one of " ++
                        @typeName(?[]const u8) ++
                        " or " ++
                        @typeName(?Tokenizer.Range) ++
                        ", instead got " ++
                        @typeName(@TypeOf(segment)),
                ),
            }
            return ctx.inner.feedSrc(segment);
        }

        pub fn feedMarker(ctx: Self, marker: ParseMarker) !void {
            return ctx.inner.feedMarker(marker);
        }
    };
}

pub const ParseMarker = enum {
    /// Followed by `feedSrc*` to construct the reference id/name.
    /// If this is fed after `text`, the text the reference evaluates
    /// to is is to be considered a continuation of the former run
    /// of data.
    reference,
    /// Followed by `feedSrc*` to construct the text data.
    /// If this is fed after `reference`, it is to be considered
    /// a continuation of the former run of data which the
    /// reference evaluated to.
    text,

    /// Followed by `feedSrc*` to consruct the name.
    /// Afterwards, it may be followed by one of:
    /// * `attribute_start`
    /// * `element_open_end`
    element_open_start,

    /// May be followed by one of:
    /// * `element_open_start`
    /// * `text`
    /// * `reference`
    /// * `element_close`
    element_open_end,
    /// Equivalent to `element_open_end`,
    /// except a new scope is not opened.
    element_open_end_inline_close,

    /// Followed by `feedSrc*` to construct the name.
    element_close,

    /// Followed by `feedSrc*` to construct the attribute name.
    /// Afterwards, it will be followed by a series of
    /// interspersed `reference`s and `text`s, terminated
    /// by `attribute_end`.
    attribute_start,
    /// Terminates the attribute.
    /// May be followed by one of:
    /// * `element_open_end`
    attribute_end,

    /// Followed by `feedSrc*` to construct the Processing Instructions.
    /// Interpretation and validation of the PI are left up to the context.
    /// NOTE: the procedure does not validate the target, or that there
    /// even is a target.
    pi,
};

pub const ParseError = error{
    UnclosedElementTag,
    UnclosedElement,
    UnexpectedToken,
    UnopenedRootElement,
    AngleBracketLeftInAttributeValue,
    InvalidReferenceEnd,
    EmptyReference,
    UnclosedPI,
    EmptyPI,
    CommentDashDash,
    CommentEndTripleDash,
    InvalidCommentStartSingleDash,
    UnclosedCDataSection,
    InvalidCDataStart,
    InvalidCDataEnd,
};

pub inline fn parseSlice(
    /// Must be a non-streaming tokenizer.
    tokenizer: *Tokenizer,
    /// Must satisfy the interface described by `ParseCtx`.
    parse_ctx_impl: anytype,
) !void {
    return parseImpl(parse_ctx_impl, tokenizer, null, .{
        .reader = {},
        .read_buffer = {},
    });
}

pub inline fn parseReader(
    /// `std.io.Reader(...)`
    reader: anytype,
    /// Used as a temporary buffer for certain reads.
    read_buffer: []u8,
    /// Must satisfy the interface described by `ParseCtx`.
    parse_ctx_impl: anytype,
) !void {
    var tokenizer = Tokenizer.initStreaming();
    return parseImpl(parse_ctx_impl, &tokenizer, @TypeOf(reader), .{
        .reader = reader,
        .read_buffer = read_buffer,
    });
}

fn parseImpl(
    parse_ctx_impl: anytype,
    tokenizer: *Tokenizer,
    comptime MaybeReader: ?type,
    mbr: parse_helper.MaybeBufferedReader(MaybeReader),
) !void {
    const Impl = @TypeOf(parse_ctx_impl);
    const parse_ctx: ParseCtx(Impl) = .{ .inner = parse_ctx_impl };

    const root_has_children = switch (try handleLeftAngleBracket(Impl, parse_ctx, tokenizer, MaybeReader, mbr)) {
        .element_open => true,
        .element_open_inline_close => false,
        .element_close => return ParseError.UnopenedRootElement,
    };

    var depth: u64 = @intFromBool(root_has_children);

    var feeding_text = false;
    if (root_has_children) while (true) switch (try parse_helper.nextTokenType(tokenizer, .non_markup, MaybeReader, mbr)) {
        .eof => return ParseError.UnclosedElement,

        .pi_start => {
            if (feeding_text) {
                feeding_text = false;
                try parse_ctx.feedSrc(@as(?if (MaybeReader != null) []const u8 else Tokenizer.Range, null));
            }

            switch (try parse_helper.nextTokenType(tokenizer, .pi, MaybeReader, mbr)) {
                else => unreachable,
                .eof => return ParseError.UnclosedPI,
                .pi_end => return ParseError.EmptyPI,
                .text_data => {},
            }
            try parse_ctx.feedMarker(.pi);
            try feedTokenSrc(Impl, parse_ctx, tokenizer, .pi, MaybeReader, mbr);
            switch (try parse_helper.nextTokenType(tokenizer, .pi, MaybeReader, mbr)) {
                else => unreachable,
                .eof => return ParseError.UnclosedPI,
                .text_data => unreachable,
                .pi_end => {},
            }
        },

        .angle_bracket_left => {
            if (feeding_text) {
                feeding_text = false;
                try parse_ctx.feedSrc(@as(?if (MaybeReader != null) []const u8 else Tokenizer.Range, null));
            }

            switch (try handleLeftAngleBracket(Impl, parse_ctx, tokenizer, MaybeReader, mbr)) {
                .element_open => depth += 1,
                .element_open_inline_close => {},
                .element_close => {
                    depth -= 1;
                    if (depth == 0) break;
                },
            }
        },

        .ampersand => {
            if (feeding_text) {
                feeding_text = false;
                try parse_ctx.feedSrc(@as(?if (MaybeReader != null) []const u8 else Tokenizer.Range, null));
            }

            try handleAmpersand(Impl, parse_ctx, tokenizer, MaybeReader, mbr);
        },

        .text_data => {
            if (!feeding_text) {
                feeding_text = true;
                try parse_ctx.feedMarker(.text);
            }

            var iter: parse_helper.TokenSrcIter(MaybeReader) = .{};
            while (true) {
                const segment = try iter.next(tokenizer, .non_markup, mbr);
                if (segment == null) break;
                try parse_ctx.feedSrc(segment);
            }
        },
        .cdata_start => cdata: {
            if (!feeding_text) {
                feeding_text = true;
                try parse_ctx.feedMarker(.text);
            }

            switch (try parse_helper.nextTokenType(tokenizer, .cdata, MaybeReader, mbr)) {
                else => unreachable,
                .eof => return ParseError.UnclosedCDataSection,
                .cdata_end => break :cdata,
                .text_data => {},
            }

            var iter: parse_helper.TokenSrcIter(MaybeReader) = .{};
            while (true) {
                const segment = try iter.next(tokenizer, .cdata, mbr);
                if (segment == null) break;
                try parse_ctx.feedSrc(segment);
            }

            switch (try parse_helper.nextTokenType(tokenizer, .cdata, MaybeReader, mbr)) {
                else => unreachable,
                .eof => return ParseError.UnclosedCDataSection,
                .text_data => unreachable,
                .cdata_end => {},
            }
        },

        .comment_start => switch (try parse_helper.handleCommentSkip(tokenizer, MaybeReader, mbr)) {
            .normal_end => {},
            .invalid_end_triple_dash => return ParseError.CommentEndTripleDash,
            .invalid_dash_dash => return ParseError.CommentDashDash,
        },
        .invalid_comment_start_single_dash => return ParseError.InvalidCommentStartSingleDash,
        .invalid_cdata_start => return ParseError.InvalidCDataStart,
        .cdata_end => return ParseError.InvalidCDataEnd,
        .dtd_start => return ParseError.UnexpectedToken,
        .invalid_dtd_start => return ParseError.UnexpectedToken,
        .invalid_angle_bracket_left_bang => return ParseError.UnexpectedToken,
        else => unreachable,
    };

    while (true) switch (try parse_helper.nextTokenType(tokenizer, .non_markup, MaybeReader, mbr)) {
        .eof => break,
        .text_data => switch (try parse_helper.skipWhitespaceTokenSrc(tokenizer, .non_markup, MaybeReader, mbr)) {
            .all_whitespace => {},
            .non_whitespace => return ParseError.UnexpectedToken,
        },
        else => return ParseError.UnexpectedToken,
    };
}

const LabResult = enum {
    element_open,
    element_open_inline_close,
    element_close,
};
fn handleLeftAngleBracket(
    comptime Impl: type,
    parse_ctx: ParseCtx(Impl),
    tokenizer: *Tokenizer,
    comptime MaybeReader: ?type,
    /// May be cleared at some point during this function.
    mbr: parse_helper.MaybeBufferedReader(MaybeReader),
) !LabResult {
    switch (try parse_helper.nextTokenType(tokenizer, .element_tag, MaybeReader, mbr)) {
        .eof => return ParseError.UnclosedElementTag,
        .equals,
        .quote_single,
        .quote_double,
        .angle_bracket_right,
        .tag_whitespace,
        => return ParseError.UnexpectedToken,
        .slash => {
            switch (try parse_helper.nextTokenType(tokenizer, .element_tag, MaybeReader, mbr)) {
                .eof => return ParseError.UnclosedElementTag,
                .equals,
                .quote_single,
                .quote_double,
                .angle_bracket_right,
                .tag_whitespace,
                .slash,
                => return ParseError.UnexpectedToken,
                .tag_token => {},
                else => unreachable,
            }

            try parse_ctx.feedMarker(.element_close);
            try feedTokenSrc(Impl, parse_ctx, tokenizer, .element_tag, MaybeReader, mbr);

            switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .element_tag, MaybeReader, mbr)) {
                .eof => return ParseError.UnclosedElementTag,
                .tag_whitespace => unreachable,
                .slash,
                .equals,
                .quote_single,
                .quote_double,
                .tag_token,
                => return ParseError.UnexpectedToken,
                .angle_bracket_right => {},
                else => unreachable,
            }

            return .element_close;
        },
        .tag_token => {
            try parse_ctx.feedMarker(.element_open_start);
            try feedTokenSrc(Impl, parse_ctx, tokenizer, .element_tag, MaybeReader, mbr);

            const ElementOpenEnd = enum {
                normal,
                inline_close,
            };
            const end_kind: ElementOpenEnd = while (true) {
                switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .element_tag, MaybeReader, mbr)) {
                    .eof => return ParseError.UnclosedElementTag,
                    .tag_whitespace => unreachable,
                    .angle_bracket_right => break .normal,
                    .slash => switch (try parse_helper.nextTokenType(tokenizer, .element_tag, MaybeReader, mbr)) {
                        else => return ParseError.UnexpectedToken,
                        .eof => return ParseError.UnclosedElementTag,
                        .angle_bracket_right => break .inline_close,
                    },

                    .equals,
                    .quote_single,
                    .quote_double,
                    => return ParseError.UnexpectedToken,

                    .tag_token => {},

                    else => unreachable,
                }

                try parse_ctx.feedMarker(.attribute_start);
                try feedTokenSrc(Impl, parse_ctx, tokenizer, .element_tag, MaybeReader, mbr);

                switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .element_tag, MaybeReader, mbr)) {
                    else => return ParseError.UnexpectedToken,
                    .eof => return ParseError.UnclosedElementTag,
                    .equals => {},
                }

                const open_quote = switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .element_tag, MaybeReader, mbr)) {
                    else => return ParseError.UnexpectedToken,
                    .eof => return ParseError.UnclosedElementTag,
                    .quote_single,
                    .quote_double,
                    => |open_quote| open_quote,
                };
                const str_ctx: Tokenizer.Context = switch (open_quote) {
                    .quote_single => .attribute_value_quote_single,
                    .quote_double => .attribute_value_quote_double,
                    else => unreachable,
                };

                while (true) switch (try parse_helper.nextTokenType(tokenizer, str_ctx, MaybeReader, mbr)) {
                    .angle_bracket_left => return ParseError.AngleBracketLeftInAttributeValue,

                    .quote_single,
                    .quote_double,
                    => |close_quote| {
                        assert(open_quote == close_quote);
                        break;
                    },

                    .text_data => try feedTokenSrc(Impl, parse_ctx, tokenizer, str_ctx, MaybeReader, mbr),
                    .ampersand => try handleAmpersand(Impl, parse_ctx, tokenizer, MaybeReader, mbr),

                    else => unreachable,
                };
                try parse_ctx.feedMarker(.attribute_end);
            };

            try parse_ctx.feedMarker(switch (end_kind) {
                .normal => .element_open_end,
                .inline_close => .element_open_end_inline_close,
            });
            return switch (end_kind) {
                .normal => .element_open,
                .inline_close => .element_open_inline_close,
            };
        },
        else => unreachable,
    }
}

fn handleAmpersand(
    comptime Impl: type,
    parse_ctx: ParseCtx(Impl),
    tokenizer: *Tokenizer,
    comptime MaybeReader: ?type,
    /// May be cleared at some point during this function.
    mbr: parse_helper.MaybeBufferedReader(MaybeReader),
) !void {
    switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
        else => unreachable,
        .invalid_reference_end => return ParseError.InvalidReferenceEnd,
        .semicolon => return ParseError.EmptyReference,
        .tag_token => {},
    }
    try parse_ctx.feedMarker(.reference);
    try feedTokenSrc(Impl, parse_ctx, tokenizer, .reference, MaybeReader, mbr);
    switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
        else => unreachable,
        .invalid_reference_end => return ParseError.InvalidReferenceEnd,
        .semicolon => {},
        .tag_token => unreachable,
    }
}

fn feedTokenSrc(
    comptime Impl: type,
    parse_ctx: ParseCtx(Impl),
    tokenizer: *Tokenizer,
    context: Tokenizer.Context,
    comptime MaybeReader: ?type,
    mbr: parse_helper.MaybeBufferedReader(MaybeReader),
) !void {
    var iter: parse_helper.TokenSrcIter(MaybeReader) = .{};
    while (true) {
        const segment = try iter.next(tokenizer, context, mbr);
        try parse_ctx.feedSrc(segment);
        if (segment == null) break;
    }
}

pub const IgnoreCtx = struct {
    pub fn feedSrc(ctx: @This(), segment: anytype) !void {
        _ = segment;
        _ = ctx;
    }

    pub fn feedMarker(ctx: @This(), marker: ParseMarker) !void {
        _ = marker;
        _ = ctx;
    }
};

test parseSlice {
    var tokenizer = Tokenizer.initComplete(
        \\<foo>
        \\  <fizz buzz="fizzbuzz"/>
        \\  &lt;not markup&gt;
        \\  <?custom stuff?>
        \\  <!-- comment -->
        \\  <![CDATA[ cdata text ]]>
        \\</foo>
        \\
    );
    assert(iksemel.prolog.parseSlice(&tokenizer, iksemel.prolog.IgnoreCtx{}) catch unreachable == .angle_bracket_left);
    try std.testing.expectEqual({}, parseSlice(&tokenizer, IgnoreCtx{}));
}

test parseReader {
    var fbs = std.io.fixedBufferStream(
        \\<foo>
        \\  <fizz buzz="fizzbuzz"/>
        \\  &lt;not markup&gt;
        \\  <?custom stuff?>
        \\  <!-- comment -->
        \\  <![CDATA[ cdata text ]]>
        \\</foo>
        \\
    );
    var read_buffer: [1]u8 = undefined;
    assert(iksemel.prolog.parseReader(fbs.reader(), &read_buffer, iksemel.prolog.IgnoreCtx{}) catch unreachable == .angle_bracket_left);
    try std.testing.expectEqual({}, parseReader(fbs.reader(), &read_buffer, IgnoreCtx{}));
}

fn TestCtx(comptime streaming: bool) type {
    return struct {
        src: if (streaming) void else []const u8,
        expected: []const TestParseItem,
        index: usize = 0,
        str_index: usize = 0,
        const Self = @This();

        pub fn feedMarker(ctx: *Self, actual_marker: ParseMarker) !void {
            defer ctx.index += 1;
            errdefer std.log.err("Error occurred on item {d}", .{ctx.index});

            switch (ctx.expected[ctx.index]) {
                .marker => |expected_marker| try std.testing.expectEqual(expected_marker, actual_marker),
                .str => |expected_str| {
                    std.log.err("Expected string '{}', got marker {}", .{ std.zig.fmtEscapes(expected_str), actual_marker });
                    return error.TestExpectedEqual;
                },
            }
            try std.testing.expectEqual(0, ctx.str_index);
        }

        const Segment = if (streaming) []const u8 else Tokenizer.Range;
        pub fn feedSrc(ctx: *Self, maybe_actual_segment: ?Segment) !void {
            errdefer std.log.err("Error occurred on item {d}", .{ctx.index});
            const expected_str = switch (ctx.expected[ctx.index]) {
                .str => |str| str,
                .marker => |actual_marker| {
                    const maybe_segment_str: ?[]const u8 = if (streaming)
                        maybe_actual_segment
                    else if (maybe_actual_segment) |seg|
                        seg.toStr(ctx.src)
                    else
                        null;
                    std.log.err("Expected marker {}, got segment '{?}'", .{ actual_marker, if (maybe_segment_str) |str| std.zig.fmtEscapes(str) else null });
                    try std.testing.expectEqual(0, ctx.str_index);
                    return error.TestExpectedEqual;
                },
            };
            const expected_segment_str: []const u8 = expected_str[ctx.str_index..];

            const actual_segment = maybe_actual_segment orelse {
                const actual_str = expected_str[0..ctx.str_index];
                try std.testing.expectEqualStrings(expected_str, actual_str);
                ctx.str_index = 0;
                ctx.index += 1;
                return;
            };
            const actual_segment_str: []const u8 = if (streaming) actual_segment else actual_segment.toStr(ctx.src);

            if (!std.mem.startsWith(u8, expected_segment_str, actual_segment_str)) {
                const actual_str = try std.mem.concat(std.testing.allocator, u8, &.{ expected_str[0..ctx.str_index], actual_segment_str });
                defer std.testing.allocator.free(actual_str);
                try std.testing.expectEqualStrings(expected_str, actual_str);
                @panic("Expected the error to go off, something is quite wrong"); // the error should have been triggered
            }

            ctx.str_index += actual_segment_str.len;
        }
    };
}

const TestParseItem = union(enum) {
    marker: ParseMarker,
    str: []const u8,

    comptime {
        assert(builtin.is_test);
    }

    pub fn format(
        tpi: TestParseItem,
        comptime fmt_str: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt_str;
        _ = options;
        switch (tpi) {
            .marker => |marker| try writer.print("TPI(.{s})", .{@tagName(marker)}),
            .str => |str| try writer.print("TPI('{}')", .{std.zig.fmtEscapes(str)}),
        }
    }
};

fn testParse(
    expected_result: ParseError!void,
    buffer_sizes: []const usize,
    /// whole document source including the prolog.
    /// prolog is ignored, and a root element is expected.
    src_with_prolog: []const u8,
    expected_outs: []const TestParseItem,
) !void {
    const src: []const u8 = src: {
        var tokenizer = Tokenizer.initComplete(src_with_prolog);
        assert(iksemel.prolog.parseSlice(&tokenizer, iksemel.prolog.IgnoreCtx{}) catch unreachable == .angle_bracket_left);
        break :src src_with_prolog[tokenizer.index..];
    };

    {
        const full_read_buffer = try std.testing.allocator.alloc(u8, if (buffer_sizes.len != 0) std.mem.max(usize, buffer_sizes) else 0);
        defer std.testing.allocator.free(full_read_buffer);

        var current_str_buf = std.ArrayList(u8).init(std.testing.allocator);
        defer current_str_buf.deinit();

        for (buffer_sizes) |buf_size| {
            var fbs = std.io.fixedBufferStream(src);

            var ctx: TestCtx(true) = .{
                .src = {},
                .expected = expected_outs,
            };
            try std.testing.expectEqual(expected_result, parseReader(fbs.reader(), full_read_buffer[0..buf_size], &ctx));
        }
    }

    var ctx: TestCtx(false) = .{
        .src = src,
        .expected = expected_outs,
    };
    var tokenizer = Tokenizer.initComplete(src);
    try std.testing.expectEqual(expected_result, parseSlice(&tokenizer, &ctx));
}

test "1" {
    try testParse({}, &.{8}, "<foo/>", &[_]TestParseItem{
        .{ .marker = .element_open_start },
        .{ .str = "foo" },
        .{ .marker = .element_open_end_inline_close },
    });

    try testParse(
        {},
        &.{
            1,   2,   3,   4,   5,   6,   7,   8,   10,  12,  14,
            16,  18,  20,  22,  24,  26,  28,  30,  32,  36,  40,
            44,  48,  52,  56,  60,  64,  72,  80,  88,  96,  104,
            112, 120, 128, 144, 160, 176, 192, 208, 224, 240, 256,
        },
        \\<foo>
        \\  <fizz buzz="fizzbuzz"/>
        \\  &lt;not markup&gt;
        \\  <?custom stuff?>
        \\  <!-- comment -->
        \\  <![CDATA[ cdata text ]]>
        \\  <bar><baz a="&quot;" b= "c&d;" e ='&f;g' h = 'i&j;k' l='&m;n&o;'></baz></bar>
        \\</foo>
        \\
    ,
        &[_]TestParseItem{
            .{ .marker = .element_open_start }, .{ .str = "foo" },
            .{ .marker = .element_open_end },   .{ .marker = .text },
            .{ .str = "\n  " },                 .{ .marker = .element_open_start },
            .{ .str = "fizz" },                 .{ .marker = .attribute_start },
            .{ .str = "buzz" },                 .{ .str = "fizzbuzz" },
            .{ .marker = .attribute_end },      .{ .marker = .element_open_end_inline_close },

            .{ .marker = .text },               .{ .str = "\n  " },
            .{ .marker = .reference },          .{ .str = "lt" },
            .{ .marker = .text },               .{ .str = "not markup" },
            .{ .marker = .reference },          .{ .str = "gt" },
            .{ .marker = .text },               .{ .str = "\n  " },

            .{ .marker = .pi },                 .{ .str = "custom stuff" },

            .{ .marker = .text },               .{ .str = "\n  \n   cdata text \n  " },

            .{ .marker = .element_open_start }, .{ .str = "bar" },
            .{ .marker = .element_open_end },   .{ .marker = .element_open_start },
            .{ .str = "baz" },                  .{ .marker = .attribute_start },
            .{ .str = "a" },                    .{ .marker = .reference },
            .{ .str = "quot" },                 .{ .marker = .attribute_end },

            .{ .marker = .attribute_start },    .{ .str = "b" },

            .{ .str = "c" },                    .{ .marker = .reference },
            .{ .str = "d" },                    .{ .marker = .attribute_end },

            .{ .marker = .attribute_start },    .{ .str = "e" },

            .{ .marker = .reference },          .{ .str = "f" },
            .{ .str = "g" },                    .{ .marker = .attribute_end },

            .{ .marker = .attribute_start },    .{ .str = "h" },

            .{ .str = "i" },                    .{ .marker = .reference },
            .{ .str = "j" },                    .{ .str = "k" },
            .{ .marker = .attribute_end },      .{ .marker = .attribute_start },
            .{ .str = "l" },                    .{ .marker = .reference },
            .{ .str = "m" },                    .{ .str = "n" },
            .{ .marker = .reference },          .{ .str = "o" },
            .{ .marker = .attribute_end },      .{ .marker = .element_open_end },

            .{ .marker = .element_close },      .{ .str = "baz" },

            .{ .marker = .element_close },      .{ .str = "bar" },

            .{ .marker = .text },               .{ .str = "\n" },

            .{ .marker = .element_close },      .{ .str = "foo" },
        },
    );
}
