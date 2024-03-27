//! Facilitates the scanning of elements in the document body.
//! Operates on a tokenizer which has tokenized past the prolog,
//! and which has returned the first left angle bracket token.

pub inline fn fullScanner() Scanner(null) {
    return .{
        .reader = {},
        .read_buffer = {},
    };
}

pub inline fn streamScanner(reader: anytype, read_buffer: []u8) Scanner(@TypeOf(reader)) {
    return .{
        .reader = reader,
        .read_buffer = read_buffer,
    };
}

pub fn Scanner(comptime MaybeReader: ?type) type {
    return struct {
        reader: (MaybeReader orelse void),
        read_buffer: if (MaybeReader != null) []u8 else void,
        state: State = .@"<",
        const Self = @This();

        pub const Error = ScanError || if (MaybeReader) |Reader| Reader.Error else error{};
        pub const TokenizerAPI = if (MaybeReader != null) Tokenizer.Stream else Tokenizer.Full;
        pub const Src = if (MaybeReader != null) []const u8 else Tokenizer.Range;

        pub fn nextMarker(
            scanner: *Self,
            tokenizer: *TokenizerAPI,
        ) Error!ScanMarker {
            return @errorCast(nextMarkerOrSegmentImpl(
                MaybeReader != null,
                &scanner.state,
                scanner.erasedReader(),
                scanner.read_buffer,
                tokenizer.asTokenizer(),
                .marker,
            ));
        }

        pub fn nextSrc(
            scanner: *Self,
            tokenizer: *TokenizerAPI,
        ) Error!?Src {
            return @errorCast(nextMarkerOrSegmentImpl(
                MaybeReader != null,
                &scanner.state,
                scanner.erasedReader(),
                scanner.read_buffer,
                tokenizer.asTokenizer(),
                .src,
            ));
        }

        inline fn erasedReader(scanner: *Self) if (MaybeReader != null) std.io.AnyReader else void {
            if (MaybeReader == null) return;
            return scanner.reader.any();
        }
    };
}

pub const ScanError = error{
    UnexpectedToken,
    UnexpectedEof,
};

pub const ScanMarker = enum {
    eof,

    /// `nextSrc` will return the segments of the PI.
    pi,

    /// `nextSrc` will return the segments of the text.
    text,
    //// `nextSrc` will return the segments of the reference name.
    reference,
    //// `nextSrc` will return the segments of the character reference number string.
    char_reference,

    /// First, `nextSrc` will return the segments of the element name.
    /// Secondly, `nextMarker` will return one of:
    /// * `.attribute_start`
    /// * `.element_open_end`
    /// * `.element_open_end_inline_close`
    /// If it's `.attribute_start`, follow retrieve the attribute information,
    /// and then repeat the second step here.
    element_open_start,

    /// First, `nextSrc` will return the segments of the attribute name.
    /// Secondly, `nextMarker` will return one of:
    /// * `.text`
    /// * `.reference`
    /// * `.char_reference`
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

    /// `nextSrc` will return the segments of the element name.
    element_close,
};

const State = enum {
    non_markup,
    pi,
    ref,

    maybe_continue_text,
    text_data,
    cdata,

    @"<",

    element_close_tag_start,
    element_close_tag_end,
    element_tag,

    attribute_name_start,
    attribute_name_end,

    attribute_value_type_sq,
    attribute_value_type_dq,

    attribute_value_text_sq,
    attribute_value_text_dq,

    attribute_value_ref_sq,
    attribute_value_ref_dq,
};

fn nextMarkerOrSegmentImpl(
    comptime from_reader: bool,
    state: *State,
    reader: if (from_reader) std.io.AnyReader else void,
    read_buffer: if (from_reader) []u8 else void,
    tokenizer: *Tokenizer,
    comptime ret_type: enum { marker, src },
) !switch (ret_type) {
    .marker => ScanMarker,
    .src => ?if (from_reader) []const u8 else Tokenizer.Range,
} {
    const MaybeReader: ?type = if (from_reader) std.io.AnyReader else null;
    const mbr: parse_helper.MaybeBufferedReader(MaybeReader) = .{
        .reader = reader,
        .read_buffer = read_buffer,
    };

    return while (true) break switch (state.*) {
        .non_markup => switch (ret_type) {
            .marker => switch (try parse_helper.nextTokenType(tokenizer, .non_markup, MaybeReader, mbr)) {
                .eof => break .eof,
                .text_data => {
                    state.* = .text_data;
                    break .text;
                },
                .cdata_start => {
                    switch (try parse_helper.nextTokenType(tokenizer, .cdata, MaybeReader, mbr)) {
                        else => unreachable,
                        .eof => return ScanError.UnexpectedEof,
                        .cdata_end => continue,
                        .text_data => {},
                    }
                    state.* = .cdata;
                    break .text;
                },
                .pi_start => {
                    state.* = .pi;
                    continue;
                },
                .ampersand => {
                    state.* = .ref;
                    continue;
                },
                .angle_bracket_left => {
                    state.* = .@"<";
                    continue;
                },
                .comment_start => switch (try parse_helper.handleCommentSkip(tokenizer, MaybeReader, mbr)) {
                    .normal_end => continue,
                    .invalid_end_triple_dash => return ScanError.UnexpectedToken,
                    .invalid_dash_dash => return ScanError.UnexpectedToken,
                    .eof => return ScanError.UnexpectedEof,
                },
                else => return ScanError.UnexpectedToken,
            },
            .src => break null,
        },

        .maybe_continue_text => switch (ret_type) {
            .marker => unreachable,
            .src => {
                switch (try parse_helper.nextTokenType(tokenizer, .non_markup, MaybeReader, mbr)) {
                    else => unreachable,

                    .text_data => {
                        state.* = .text_data;
                        continue;
                    },
                    .cdata_start => {
                        switch (try parse_helper.nextTokenType(tokenizer, .cdata, MaybeReader, mbr)) {
                            else => unreachable,
                            .eof => return ScanError.UnexpectedEof,
                            .cdata_end => continue,
                            .text_data => {},
                        }
                        state.* = .cdata;
                        continue;
                    },

                    .eof => state.* = .non_markup,
                    .pi_start => state.* = .pi,
                    .angle_bracket_left => state.* = .@"<",
                    .ampersand => state.* = .ref,
                    .comment_start => switch (try parse_helper.handleCommentSkip(tokenizer, MaybeReader, mbr)) {
                        .normal_end => continue,
                        .invalid_end_triple_dash => return ScanError.UnexpectedToken,
                        .invalid_dash_dash => return ScanError.UnexpectedToken,
                        .eof => return ScanError.UnexpectedEof,
                    },

                    .cdata_end => return ScanError.UnexpectedToken,
                    .dtd_start => return ScanError.UnexpectedToken,
                    .invalid_comment_start_single_dash => return ScanError.UnexpectedToken,
                    .invalid_cdata_start => return ScanError.UnexpectedToken,
                    .invalid_dtd_start => return ScanError.UnexpectedToken,
                    .invalid_angle_bracket_left_bang => return ScanError.UnexpectedToken,
                }
                break null;
            },
        },

        .text_data => switch (ret_type) {
            .marker => unreachable,
            .src => {
                if (MaybeReader != null) {
                    if (try parse_helper.nextTokenSegment(tokenizer, .non_markup, mbr.reader, mbr.read_buffer)) |segment| {
                        break segment;
                    }
                    state.* = .maybe_continue_text;
                    continue;
                } else {
                    const range = tokenizer.full.nextSrc(.non_markup);
                    state.* = .maybe_continue_text;
                    break range;
                }
            },
        },

        .cdata => switch (ret_type) {
            .marker => unreachable,
            .src => {
                if (MaybeReader != null) {
                    if (try parse_helper.nextTokenSegment(tokenizer, .cdata, mbr.reader, mbr.read_buffer)) |segment| {
                        break segment;
                    }
                    switch (try parse_helper.nextTokenType(tokenizer, .cdata, MaybeReader, mbr)) {
                        else => unreachable,
                        .text_data => unreachable,
                        .eof => return ScanError.UnexpectedEof,
                        .cdata_end => {},
                    }
                    state.* = .maybe_continue_text;
                    continue;
                } else {
                    const range = tokenizer.full.nextSrc(.cdata);
                    switch (try parse_helper.nextTokenType(tokenizer, .cdata, MaybeReader, mbr)) {
                        else => unreachable,
                        .text_data => unreachable,
                        .eof => return ScanError.UnexpectedEof,
                        .cdata_end => {},
                    }
                    state.* = .maybe_continue_text;
                    break range;
                }
            },
        },

        .pi => switch (ret_type) {
            .marker => {
                switch (try parse_helper.nextTokenType(tokenizer, .pi, MaybeReader, mbr)) {
                    else => unreachable,
                    .eof => return ScanError.UnexpectedEof,
                    .pi_end => return ScanError.UnexpectedToken,
                    .text_data => {},
                }
                break .pi;
            },
            .src => {
                if (MaybeReader != null) {
                    if (try parse_helper.nextTokenSegment(tokenizer, .pi, mbr.reader, mbr.read_buffer)) |segment| {
                        break segment;
                    }
                    switch (try parse_helper.nextTokenType(tokenizer, .pi, MaybeReader, mbr)) {
                        else => unreachable,
                        .text_data => unreachable,
                        .eof => return ScanError.UnexpectedEof,
                        .pi_end => {},
                    }
                    state.* = .non_markup;
                    continue;
                } else {
                    const range = tokenizer.full.nextSrc(.pi);
                    switch (try parse_helper.nextTokenType(tokenizer, .pi, MaybeReader, mbr)) {
                        else => unreachable,
                        .text_data => unreachable,
                        .eof => return ScanError.UnexpectedEof,
                        .pi_end => {},
                    }
                    state.* = .non_markup;
                    break range;
                }
            },
        },

        .ref => switch (ret_type) {
            .marker => {
                const ref_kind: ScanMarker = switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
                    else => unreachable,
                    .eof => return ScanError.UnexpectedEof,
                    .invalid_reference_end => return ScanError.UnexpectedToken,
                    .semicolon => return ScanError.UnexpectedToken,
                    .hashtag => switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
                        else => unreachable,
                        .eof => return ScanError.UnexpectedEof,
                        .hashtag => return ScanError.UnexpectedToken,
                        .invalid_reference_end => return ScanError.UnexpectedToken,
                        .semicolon => return ScanError.UnexpectedToken,
                        .tag_token => .char_reference,
                    },
                    .tag_token => .reference,
                };
                break ref_kind;
            },
            .src => {
                if (MaybeReader != null) {
                    if (try parse_helper.nextTokenSegment(tokenizer, .reference, mbr.reader, mbr.read_buffer)) |segment| {
                        break segment;
                    }
                    try expectReferenceSemicolonAfterTagToken(tokenizer, MaybeReader, mbr);
                    state.* = .non_markup;
                    continue;
                } else {
                    const range = tokenizer.full.nextSrc(.reference);
                    try expectReferenceSemicolonAfterTagToken(tokenizer, MaybeReader, mbr);
                    state.* = .non_markup;
                    break range;
                }
            },
        },

        .@"<" => switch (ret_type) {
            .marker => switch (try parse_helper.nextTokenType(tokenizer, .element_tag, MaybeReader, mbr)) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,

                .slash => {
                    switch (try parse_helper.nextTokenType(tokenizer, .element_tag, MaybeReader, mbr)) {
                        else => return ScanError.UnexpectedToken,
                        .eof => return ScanError.UnexpectedEof,
                        .tag_token => {},
                    }
                    state.* = .element_close_tag_start;
                    break .element_close;
                },
                .tag_token => break .element_open_start,
            },
            .src => {
                if (MaybeReader != null) {
                    if (try parse_helper.nextTokenSegment(tokenizer, .element_tag, mbr.reader, mbr.read_buffer)) |segment| {
                        break segment;
                    }
                    state.* = .element_tag;
                    continue;
                } else {
                    state.* = .element_tag;
                    break tokenizer.full.nextSrc(.element_tag);
                }
            },
        },
        .element_close_tag_start => switch (ret_type) {
            .marker => switch (try parse_helper.nextTokenType(tokenizer, .element_tag, MaybeReader, mbr)) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,
                .tag_token => break .element_close,
            },
            .src => {
                if (MaybeReader != null) {
                    if (try parse_helper.nextTokenSegment(tokenizer, .element_tag, mbr.reader, mbr.read_buffer)) |segment| {
                        break segment;
                    }
                    switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .element_tag, MaybeReader, mbr)) {
                        else => return ScanError.UnexpectedToken,
                        .eof => return ScanError.UnexpectedEof,
                        .angle_bracket_right => {},
                    }
                    state.* = .non_markup;
                    continue;
                } else {
                    const range = tokenizer.full.nextSrc(.element_tag);
                    switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .element_tag, MaybeReader, mbr)) {
                        else => return ScanError.UnexpectedToken,
                        .eof => return ScanError.UnexpectedEof,
                        .angle_bracket_right => {},
                    }
                    state.* = .non_markup;
                    break range;
                }
            },
        },
        .element_close_tag_end => switch (ret_type) {
            .src => break null,
            .marker => {
                switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .element_tag, MaybeReader, mbr)) {
                    else => return ScanError.UnexpectedToken,
                    .eof => return ScanError.UnexpectedEof,
                    .tag_whitespace => unreachable,
                    .angle_bracket_right => {},
                }
                state.* = .non_markup;
                continue;
            },
        },

        .element_tag => switch (ret_type) {
            .src => break null,
            .marker => {
                const first_non_whitespace_tt = switch (try parse_helper.nextTokenType(tokenizer, .element_tag, MaybeReader, mbr)) {
                    else => return ScanError.UnexpectedToken,
                    .eof => return ScanError.UnexpectedEof,
                    .slash, .angle_bracket_right => |tt| tt,
                    .tag_whitespace => tt: {
                        try parse_helper.skipWhitespaceSrcUnchecked(tokenizer, .element_tag, MaybeReader, mbr);
                        break :tt try parse_helper.nextTokenType(tokenizer, .element_tag, MaybeReader, mbr);
                    },
                };
                switch (first_non_whitespace_tt) {
                    else => return ScanError.UnexpectedToken,
                    .eof => return ScanError.UnexpectedEof,
                    .slash => {
                        switch (try parse_helper.nextTokenType(tokenizer, .element_tag, MaybeReader, mbr)) {
                            else => return ScanError.UnexpectedToken,
                            .eof => return ScanError.UnexpectedEof,
                            .angle_bracket_right => {},
                        }
                        state.* = .non_markup;
                        break .element_open_end_inline_close;
                    },
                    .angle_bracket_right => {
                        state.* = .non_markup;
                        break .element_open_end;
                    },
                    .tag_token => {
                        state.* = .attribute_name_start;
                        break .attribute_start;
                    },
                }
            },
        },
        .attribute_name_start => switch (ret_type) {
            .marker => unreachable,
            .src => {
                if (MaybeReader != null) {
                    if (try parse_helper.nextTokenSegment(tokenizer, .element_tag, mbr.reader, mbr.read_buffer)) |segment| {
                        break segment;
                    }
                    state.* = .attribute_name_end;
                    continue;
                } else {
                    state.* = .attribute_name_end;
                    break tokenizer.full.nextSrc(.element_tag);
                }
            },
        },
        .attribute_name_end => switch (ret_type) {
            .src => break null,
            .marker => {
                switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .element_tag, MaybeReader, mbr)) {
                    else => return ScanError.UnexpectedToken,
                    .eof => return ScanError.UnexpectedEof,
                    .tag_whitespace => unreachable,
                    .equals => {},
                }

                const quote_type = switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .element_tag, MaybeReader, mbr)) {
                    else => return ScanError.UnexpectedToken,
                    .eof => return ScanError.UnexpectedEof,
                    .tag_whitespace => unreachable,
                    inline //
                    .quote_single,
                    .quote_double,
                    => |tt| comptime QuoteType.fromTokenType(tt).?,
                };
                state.* = switch (quote_type) {
                    .single => .attribute_value_type_sq,
                    .double => .attribute_value_type_dq,
                };
                continue;
            },
        },

        .attribute_value_type_sq,
        .attribute_value_type_dq,
        => |quote_state| {
            const quote_type: QuoteType = switch (quote_state) {
                .attribute_value_type_sq => .single,
                .attribute_value_type_dq => .double,
                else => unreachable,
            };
            switch (ret_type) {
                .marker => {
                    switch (try parse_helper.nextTokenType(tokenizer, quote_type.attributeValueCtx(), MaybeReader, mbr)) {
                        else => unreachable,
                        .eof => return ScanError.UnexpectedEof,
                        .angle_bracket_left => return ScanError.UnexpectedToken,

                        .quote_single, .quote_double => {
                            state.* = .element_tag;
                            break .attribute_end;
                        },
                        .ampersand => {
                            const ref_kind: ScanMarker = switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
                                else => unreachable,
                                .eof => return ScanError.UnexpectedEof,
                                .invalid_reference_end => return ScanError.UnexpectedToken,
                                .semicolon => return ScanError.UnexpectedToken,
                                .hashtag => switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
                                    else => unreachable,
                                    .eof => return ScanError.UnexpectedEof,
                                    .hashtag => return ScanError.UnexpectedToken,
                                    .invalid_reference_end => return ScanError.UnexpectedToken,
                                    .semicolon => return ScanError.UnexpectedToken,
                                    .tag_token => .char_reference,
                                },
                                .tag_token => .reference,
                            };
                            state.* = switch (quote_type) {
                                .single => .attribute_value_ref_sq,
                                .double => .attribute_value_ref_dq,
                            };
                            break ref_kind;
                        },
                        .text_data => {
                            state.* = switch (quote_type) {
                                .single => .attribute_value_text_sq,
                                .double => .attribute_value_text_dq,
                            };
                            break .text;
                        },
                    }
                },
                .src => break null,
            }
        },

        .attribute_value_ref_sq,
        .attribute_value_ref_dq,
        => |quote_state| {
            const quote_type: QuoteType = switch (quote_state) {
                .attribute_value_ref_sq => .single,
                .attribute_value_ref_dq => .double,
                else => unreachable,
            };
            const type_state: State = switch (quote_type) {
                .single => .attribute_value_type_sq,
                .double => .attribute_value_type_dq,
            };
            switch (ret_type) {
                .marker => unreachable,
                .src => {
                    if (MaybeReader != null) {
                        if (try parse_helper.nextTokenSegment(tokenizer, .reference, mbr.reader, mbr.read_buffer)) |segment| {
                            break segment;
                        }
                        try expectReferenceSemicolonAfterTagToken(tokenizer, MaybeReader, mbr);
                        state.* = type_state;
                        continue;
                    } else {
                        const range = tokenizer.full.nextSrc(.reference);
                        try expectReferenceSemicolonAfterTagToken(tokenizer, MaybeReader, mbr);
                        state.* = type_state;
                        break range;
                    }
                },
            }
        },

        .attribute_value_text_sq,
        .attribute_value_text_dq,
        => |quote_state| {
            const quote_type: QuoteType = switch (quote_state) {
                .attribute_value_text_sq => .single,
                .attribute_value_text_dq => .double,
                else => unreachable,
            };
            const type_state: State = switch (quote_type) {
                .single => .attribute_value_type_sq,
                .double => .attribute_value_type_dq,
            };
            switch (ret_type) {
                .marker => unreachable,
                .src => {
                    if (MaybeReader != null) {
                        if (try parse_helper.nextTokenSegment(tokenizer, quote_type.attributeValueCtx(), mbr.reader, mbr.read_buffer)) |segment| {
                            break segment;
                        }
                        state.* = type_state;
                        continue;
                    } else {
                        state.* = type_state;
                        break tokenizer.full.nextSrc(quote_type.attributeValueCtx());
                    }
                },
            }
        },
    };
}

fn expectReferenceSemicolonAfterTagToken(
    tokenizer: *Tokenizer,
    comptime MaybeReader: ?type,
    mbr: parse_helper.MaybeBufferedReader(MaybeReader),
) !void {
    switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
        else => unreachable,
        .tag_token => unreachable,
        .eof => return ScanError.UnexpectedEof,
        .hashtag => return ScanError.UnexpectedToken,
        .invalid_reference_end => return ScanError.UnexpectedToken,
        .semicolon => {},
    }
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
            var tokenizer = Tokenizer.initStream();

            var scanner = streamScanner(fbs.reader(), read_buffer);
            for (expected_items, 0..) |expected_item, i| {
                errdefer std.log.err("Error occurred on item {d}", .{i});
                switch (expected_item) {
                    .marker => |marker| try std.testing.expectEqual(marker, scanner.nextMarker(&tokenizer.stream)),
                    .str => |str| {
                        while (try scanner.nextSrc(&tokenizer.stream)) |segment| {
                            try str_buffer.appendSlice(segment);
                        }
                        try std.testing.expectEqualStrings(str, str_buffer.items);
                        str_buffer.clearRetainingCapacity();
                    },
                }
            }
            try std.testing.expectEqual(.eof, scanner.nextMarker(&tokenizer.stream));
        }
    }

    var tokenizer = Tokenizer.initFull(src);

    var scanner = fullScanner();
    for (expected_items, 0..) |expected_item, i| {
        errdefer std.log.err("Error occurred on item {d}", .{i});
        switch (expected_item) {
            .marker => |marker| try std.testing.expectEqual(marker, scanner.nextMarker(&tokenizer.full)),
            .str => |str| {
                while (try scanner.nextSrc(&tokenizer.full)) |segment| {
                    try str_buffer.appendSlice(segment.toStr(src));
                }
                try std.testing.expectEqualStrings(str, str_buffer.items);
                str_buffer.clearRetainingCapacity();
            },
        }
    }
    try std.testing.expectEqual(.eof, scanner.nextMarker(&tokenizer.full));
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
const QuoteType = Tokenizer.QuoteType;

const parse_helper = @import("parse_helper.zig");
