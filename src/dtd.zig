/// Expects that the last value returned by `tokenizer.nextType(.dtd)`
/// to have been `.square_bracket_left`.
pub inline fn internalSubsetSliceScanner(tokenizer: *Tokenizer) InternalSubsetScanner(null) {
    return .{
        .tokenizer = tokenizer,
        .mbr = .{
            .reader = {},
            .read_buffer = {},
        },
    };
}

/// Expects that the last value returned by `tokenizer.nextType(.dtd)`
/// to have been `.square_bracket_left`. `reader` and `read_buf` will be used
/// as the source to feed the tokenizer.
pub inline fn internalSubsetReaderScanner(
    tokenizer: *Tokenizer,
    reader: anytype,
    read_buf: []u8,
) InternalSubsetScanner(@TypeOf(reader)) {
    return .{
        .tokenizer = tokenizer,
        .mbr = .{
            .reader = reader,
            .read_buffer = read_buf,
        },
    };
}

pub fn InternalSubsetScanner(comptime MaybeReader: ?type) type {
    return struct {
        tokenizer: *Tokenizer,
        mbr: parse_helper.MaybeBufferedReader(MaybeReader),
        state: State = .blank,
        segment_iter: parse_helper.TokenSrcIter(MaybeReader) = undefined,
        const Self = @This();

        pub const Segment = if (MaybeReader != null)
            []const u8
        else
            Tokenizer.Range;

        pub inline fn nextMarker(scanner: *Self) !InternalSubsetScanMarker {
            return nextMarkerOrSegmentImpl(MaybeReader, scanner, .marker);
        }

        /// Returns segments of a string, either as a slice or a range,
        /// based upon whether this is backed by a streaming or
        /// non-streaming Tokenizer. A full string is terminated by
        /// a null sentinel value.
        ///
        /// All segments are guaranteed to be consecutive in the source.
        ///
        /// An empty string is comprised of at least one non-null segment
        /// representing an empty range/slice, followed by the null sentinel;
        /// an absent string is simply a standalone null sentinel.
        pub fn nextSegment(scanner: *Self) !?Segment {
            return nextMarkerOrSegmentImpl(MaybeReader, scanner, .segment);
        }
    };
}

pub const InternalSubsetScanError = error{
    UnexpectedToken,
    UnexpectedEof,
    EmptyPI,
    MissingAsterisk,
};

pub const InternalSubsetScanMarker = union(enum) {
    /// Encountered ']', terminating the internal subset.
    end,

    /// `nextSegment` will return the PI data.
    pi,

    /// First `nextSegment` will return the declared element's name.
    /// Then `nextMarker` will return `.content_spec`, with a payload
    /// describing the procedure to obtain the content specification.
    element_decl,
    content_spec: ContentSpec,
    children_tok: ?ChildrenToken,
    mixed_content_spec_end: MixedContentSpecEnd,

    pub const ContentSpec = enum {
        /// The content specification is simply 'EMPTY',
        /// there is no more information to acquire.
        /// The declaration is already closed after this.
        empty,
        /// The content specification is simply 'ANY',
        /// there is no more information to acquire.
        /// The declaration is already closed after this.
        any,
        /// The content specification is a list of possible names,
        /// returned in order by `nextSegment`.
        /// Afterwards, `nextMarker` will return `.content_spec_end`;
        /// if the list contained at least one name, `.mixed_content_spec_end = .zero_or_many`,
        /// otherwise, if the list was empty, it may be either `.one` or `.zero_or_many`.
        /// The declaration is closed after the aforementioned procedure.
        mixed,
        /// The content specification is a content particle.
        /// The tokens forming the content particle will be returned
        /// as a series of `nextMarker() = .{ .children_tok = tok }`,
        /// terminated by `tok == null`.
        ///
        /// The returned series of tokens will match `children` in:
        /// ```
        /// children ::= (choice | seq) ('?' | '*' | '+')?
        /// cp       ::= (Name | choice | seq) ('?' | '*' | '+')?
        /// choice   ::= '(' S? cp ( S? '|' S? cp )+ S? ')'
        /// seq      ::= '(' S? cp ( S? ',' S? cp )* S? ')'
        /// ```
        /// The declaration is closed after the aforementioned procedure.
        children,
    };
    pub const ChildrenToken = enum {
        /// `nextSegment` returns the source for the name.
        name,

        lparen,
        rparen,

        qmark,
        asterisk,
        plus,

        comma,
        pipe,
    };
    pub const MixedContentSpecEnd = enum {
        /// '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*'
        zero_or_many,
        /// '(' S? '#PCDATA' S? ')'
        one,
    };
};

const State = union(enum) {
    end,
    blank,
    pi,

    element_start,
    element_mixed_list_name,
    element_mixed_list_rparen,
    element_mixed_empty_rparen,

    element_children_stacked_lparen_lparen,
    element_children_stacked_lparen,
    element_children_stacked_lparen_tag_token,
    element_children_stacked_tag_token,

    element_children_name: u32,
    element_children_quantifier: u32,
    element_children_sep: u32,
    element_children_lparen: u32,
    element_children_rparen: u32,
};

fn nextMarkerOrSegmentImpl(
    comptime MaybeReader: ?type,
    scanner: *InternalSubsetScanner(MaybeReader),
    comptime ret_type: enum { marker, segment },
) !switch (ret_type) {
    .marker => InternalSubsetScanMarker,
    .segment => ?InternalSubsetScanner(MaybeReader).Segment,
} {
    const tokenizer: *Tokenizer = scanner.tokenizer;
    const mbr = scanner.mbr;
    return while (true) break switch (scanner.state) {
        .end => switch (ret_type) {
            .marker => break .end,
            .segment => unreachable,
        },
        .blank => switch (ret_type) {
            .marker => switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                .tag_whitespace => unreachable,

                .square_bracket_right => {
                    scanner.state = .end;
                    break .end;
                },

                .pi_start => switch (try parse_helper.nextTokenType(tokenizer, .pi, MaybeReader, mbr)) {
                    else => unreachable,
                    .eof => return InternalSubsetScanError.UnexpectedEof,
                    .pi_end => return InternalSubsetScanError.EmptyPI,
                    .text_data => {
                        scanner.segment_iter = .{};
                        scanner.state = .pi;
                        break .pi;
                    },
                },

                .dtd_decl => {
                    const decl_str = (try parse_helper.nextTokenSrcAsEnum(tokenizer, .dtd, MaybeReader, mbr, enum {
                        @"<!ENTITY",
                        @"<!ELEMENT",
                        @"<!ATTLIST",
                        @"<!NOTATION",
                    })) orelse return InternalSubsetScanError.UnexpectedToken;
                    switch (decl_str) {
                        .@"<!ENTITY" => @panic("TODO"),
                        .@"<!ELEMENT" => {
                            if (try parse_helper.skipIfTagWhitespaceOrGetNextTokType(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
                                .tag_whitespace => unreachable,
                                .tag_token => unreachable,
                                .eof => return InternalSubsetScanError.UnexpectedEof,
                                else => return InternalSubsetScanError.UnexpectedToken,
                            };
                            switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                else => return InternalSubsetScanError.UnexpectedToken,
                                .eof => return InternalSubsetScanError.UnexpectedEof,
                                .tag_whitespace => unreachable,
                                .tag_token => {},
                            }
                            scanner.segment_iter = .{};
                            scanner.state = .element_start;
                            break .element_decl;
                        },
                        .@"<!ATTLIST" => @panic("TODO"),
                        .@"<!NOTATION" => @panic("TODO"),
                    }
                },

                .comment_start => switch (try parse_helper.handleCommentSkip(tokenizer, MaybeReader, mbr)) {
                    .normal_end => continue,
                    .invalid_end_triple_dash => return InternalSubsetScanError.UnexpectedToken,
                    .invalid_dash_dash => return InternalSubsetScanError.UnexpectedToken,
                    .eof => return InternalSubsetScanError.UnexpectedEof,
                },
                .eof => return InternalSubsetScanError.UnexpectedEof,
                else => return InternalSubsetScanError.UnexpectedToken,
            },
            .segment => unreachable,
        },

        .pi => switch (ret_type) {
            .marker => unreachable,
            .segment => {
                if (try scanner.segment_iter.next(tokenizer, .pi, mbr)) |segment| break segment;
                scanner.segment_iter = undefined;

                switch (try parse_helper.nextTokenType(tokenizer, .pi, MaybeReader, mbr)) {
                    else => unreachable,
                    .text_data => unreachable,
                    .eof => return InternalSubsetScanError.UnexpectedEof,
                    .pi_end => {},
                }
                scanner.state = .blank;
                break null;
            },
        },

        .element_start => switch (ret_type) {
            // this goes second
            .marker => {
                if (try parse_helper.skipIfTagWhitespaceOrGetNextTokType(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
                    .tag_whitespace => unreachable,
                    .tag_token => unreachable,
                    .eof => return InternalSubsetScanError.UnexpectedEof,
                    else => return InternalSubsetScanError.UnexpectedToken,
                };
                switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                    else => return InternalSubsetScanError.UnexpectedToken,
                    .eof => return InternalSubsetScanError.UnexpectedEof,
                    .tag_whitespace => unreachable,
                    .tag_token => {
                        const empty_or_any = (try parse_helper.nextTokenSrcAsEnum(tokenizer, .dtd, MaybeReader, mbr, enum { EMPTY, ANY })) orelse
                            return InternalSubsetScanError.UnexpectedToken;
                        switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                            else => return InternalSubsetScanError.UnexpectedToken,
                            .eof => return InternalSubsetScanError.UnexpectedEof,
                            .tag_whitespace => unreachable,
                            .angle_bracket_right => {},
                        }
                        scanner.state = .blank;
                        break .{ .content_spec = switch (empty_or_any) {
                            .EMPTY => .empty,
                            .ANY => .any,
                        } };
                    },
                    .lparen => switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                        else => return InternalSubsetScanError.UnexpectedToken,
                        .eof => return InternalSubsetScanError.UnexpectedEof,

                        .hashtag => {
                            switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                else => return InternalSubsetScanError.UnexpectedToken,
                                .eof => return InternalSubsetScanError.UnexpectedEof,
                                .tag_token => if (try parse_helper.nextTokenSrcAsEnum(tokenizer, .dtd, MaybeReader, mbr, enum { PCDATA }) == null) {
                                    return InternalSubsetScanError.UnexpectedToken;
                                },
                            }
                            scanner.state = switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                                else => return InternalSubsetScanError.UnexpectedToken,
                                .tag_whitespace => unreachable,
                                .tag_token => unreachable,

                                .pipe => switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                                    else => return InternalSubsetScanError.UnexpectedToken,
                                    .tag_whitespace => unreachable,
                                    .tag_token => .element_mixed_list_name,
                                },
                                .rparen => .element_mixed_empty_rparen,
                            };
                            break .{ .content_spec = .mixed };
                        },

                        .lparen => {
                            scanner.state = .element_children_stacked_lparen_lparen;
                            break .{ .content_spec = .children };
                        },
                        .tag_token => {
                            scanner.state = .element_children_stacked_lparen_tag_token;
                            scanner.segment_iter = .{};
                            break .{ .content_spec = .children };
                        },
                    },
                }
            },

            // this goes first
            .segment => {
                if (try scanner.segment_iter.next(tokenizer, .dtd, mbr)) |segment| break segment;
                scanner.segment_iter = undefined;
                break null;
            },
        },

        .element_mixed_list_name => switch (ret_type) {
            .marker => unreachable,
            .segment => {
                if (try scanner.segment_iter.next(tokenizer, .dtd, mbr)) |segment| break segment;
                scanner.segment_iter = undefined;

                switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                    else => return InternalSubsetScanError.UnexpectedToken,
                    .eof => return InternalSubsetScanError.UnexpectedEof,
                    .tag_whitespace => unreachable,

                    .rparen => scanner.state = .element_mixed_list_rparen,

                    .pipe => switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                        else => return InternalSubsetScanError.UnexpectedToken,
                        .tag_whitespace => unreachable,
                        .tag_token => scanner.segment_iter = .{},
                    },
                }

                break null;
            },
        },
        inline //
        .element_mixed_list_rparen,
        .element_mixed_empty_rparen,
        => |_, tag| switch (ret_type) {
            // this goes second
            .marker => {
                const mixed_content_spec_end: InternalSubsetScanMarker.MixedContentSpecEnd = switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                    .asterisk => blk: {
                        switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                            else => return InternalSubsetScanError.UnexpectedToken,
                            .eof => return InternalSubsetScanError.UnexpectedEof,
                            .tag_whitespace => unreachable,
                            .angle_bracket_right => {},
                        }
                        break :blk .zero_or_many;
                    },
                    .tag_whitespace => blk: {
                        try parse_helper.skipWhitespaceSrcUnchecked(tokenizer, .dtd, MaybeReader, mbr);
                        switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                            else => return InternalSubsetScanError.UnexpectedToken,
                            .eof => return InternalSubsetScanError.UnexpectedEof,
                            .tag_whitespace => unreachable,
                            .angle_bracket_right => {},
                        }
                        break :blk .one;
                    },
                    .angle_bracket_right => .one,
                    .eof => return InternalSubsetScanError.UnexpectedEof,
                    else => return InternalSubsetScanError.UnexpectedToken,
                };

                switch (mixed_content_spec_end) {
                    .one => switch (tag) {
                        .element_mixed_list_rparen => return InternalSubsetScanError.MissingAsterisk,
                        .element_mixed_empty_rparen => {},
                        else => comptime unreachable,
                    },
                    .zero_or_many => {},
                }

                scanner.state = .blank;
                break .{ .mixed_content_spec_end = mixed_content_spec_end };
            },

            // this goes first
            .segment => break null,
        },

        .element_children_stacked_lparen_lparen => switch (ret_type) {
            .marker => {
                scanner.state = .element_children_stacked_lparen;
                break .{ .children_tok = .lparen };
            },
            .segment => unreachable,
        },
        .element_children_stacked_lparen => switch (ret_type) {
            .marker => {
                scanner.state = .{ .element_children_lparen = 2 };
                break .{ .children_tok = .lparen };
            },
            .segment => unreachable,
        },

        .element_children_stacked_lparen_tag_token => switch (ret_type) {
            .marker => {
                scanner.state = .element_children_stacked_tag_token;
                break .{ .children_tok = .lparen };
            },
            .segment => unreachable,
        },
        .element_children_stacked_tag_token => switch (ret_type) {
            .marker => break .{ .children_tok = .name },
            .segment => {
                if (try scanner.segment_iter.next(tokenizer, .dtd, mbr)) |segment| break segment;
                scanner.state = .{ .element_children_name = 1 };
                break null;
            },
        },

        .element_children_name => |depth| switch (ret_type) {
            .marker => switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                .rparen => {
                    scanner.state = .{ .element_children_rparen = depth - 1 };
                    break .{ .children_tok = .rparen };
                },
                inline .comma, .pipe => |tt| {
                    scanner.state = .{ .element_children_sep = depth };
                    break .{ .children_tok = @field(InternalSubsetScanMarker.ChildrenToken, @tagName(tt)) };
                },
                inline .qmark, .asterisk, .plus => |tt| {
                    scanner.state = .{ .element_children_quantifier = depth };
                    break .{ .children_tok = @field(InternalSubsetScanMarker.ChildrenToken, @tagName(tt)) };
                },
                else => return InternalSubsetScanError.UnexpectedToken,
            },
            .segment => {
                if (try scanner.segment_iter.next(tokenizer, .dtd, mbr)) |segment| break segment;
                scanner.segment_iter = undefined;
                break null;
            },
        },
        .element_children_quantifier => |depth| switch (ret_type) {
            .marker => switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                .rparen => {
                    if (depth == 0) return InternalSubsetScanError.UnexpectedToken;
                    scanner.state = .{ .element_children_rparen = depth - 1 };
                    break .{ .children_tok = .rparen };
                },
                inline .comma, .pipe => |tt| {
                    if (depth == 0) return InternalSubsetScanError.UnexpectedToken;
                    scanner.state = .{ .element_children_sep = depth };
                    break .{ .children_tok = @field(InternalSubsetScanMarker.ChildrenToken, @tagName(tt)) };
                },
                .angle_bracket_right => {
                    if (depth != 0) return InternalSubsetScanError.UnexpectedToken;
                    scanner.state = .blank;
                    break .{ .children_tok = null };
                },
                else => return InternalSubsetScanError.UnexpectedToken,
            },
            .segment => unreachable,
        },
        .element_children_sep => |depth| switch (ret_type) {
            .marker => switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                .tag_token => {
                    scanner.state = .{ .element_children_name = depth };
                    scanner.segment_iter = .{};
                    break .{ .children_tok = .name };
                },
                .lparen => {
                    scanner.state = .{ .element_children_lparen = depth + 1 };
                    break .{ .children_tok = .lparen };
                },
                else => return InternalSubsetScanError.UnexpectedToken,
            },
            .segment => unreachable,
        },
        .element_children_lparen => |*depth| switch (ret_type) {
            .marker => switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                .tag_token => {
                    scanner.state = .{ .element_children_name = depth.* };
                    scanner.segment_iter = .{};
                    break .{ .children_tok = .name };
                },
                .lparen => {
                    depth.* += 1;
                    break .{ .children_tok = .lparen };
                },
                else => return InternalSubsetScanError.UnexpectedToken,
            },
            .segment => unreachable,
        },
        .element_children_rparen => |*depth| switch (ret_type) {
            .marker => switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                .rparen => {
                    if (depth.* == 0) return InternalSubsetScanError.UnexpectedToken;
                    depth.* -= 1;
                    break .{ .children_tok = .rparen };
                },
                inline .comma, .pipe => |tt| {
                    if (depth.* == 0) return InternalSubsetScanError.UnexpectedToken;
                    scanner.state = .{ .element_children_sep = depth.* };
                    break .{ .children_tok = @field(InternalSubsetScanMarker.ChildrenToken, @tagName(tt)) };
                },
                inline .qmark, .asterisk, .plus => |tt| {
                    scanner.state = .{ .element_children_quantifier = depth.* };
                    break .{ .children_tok = @field(InternalSubsetScanMarker.ChildrenToken, @tagName(tt)) };
                },
                .angle_bracket_right => {
                    scanner.state = .blank;
                    break .{ .children_tok = null };
                },
                else => return InternalSubsetScanError.UnexpectedToken,
            },
            .segment => unreachable,
        },
    };
}

const ScannerTestItem = union(enum) {
    marker: InternalSubsetScanMarker,
    str: ?[]const u8,
};
fn testInternalSubsetScanner(
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
            assert(parse_helper.nextTokenType(&tokenizer, .dtd, @TypeOf(fbs.reader()), .{
                .reader = fbs.reader(),
                .read_buffer = read_buffer,
            }) catch unreachable == .square_bracket_left);

            var scanner = internalSubsetReaderScanner(&tokenizer, fbs.reader(), read_buffer);
            for (expected_items, 0..) |expected_item, i| {
                errdefer std.log.err("Error occurred on item {d}", .{i});
                switch (expected_item) {
                    .marker => |marker| try std.testing.expectEqual(marker, scanner.nextMarker()),
                    .str => |expected_str| {
                        const actual_str: ?[]const u8 = blk: {
                            try str_buffer.appendSlice((try scanner.nextSegment()) orelse break :blk null);
                            while (try scanner.nextSegment()) |segment| {
                                try str_buffer.appendSlice(segment);
                            }
                            break :blk str_buffer.items;
                        };
                        if (expected_str != null and actual_str != null) {
                            try std.testing.expectEqualStrings(expected_str.?, actual_str.?);
                        } else if ((expected_str == null) != (actual_str == null)) {
                            std.log.err("Expected {[expected_quote]s}{[expected]?}{[expected_quote]s}, got {[actual_quote]s}{[actual]?}{[actual_quote]s}", .{
                                .expected_quote = if (expected_str != null) "'" else "",
                                .expected = if (expected_str) |str| std.zig.fmtEscapes(str) else null,
                                .actual_quote = if (actual_str != null) "'" else "",
                                .actual = if (actual_str) |str| std.zig.fmtEscapes(str) else null,
                            });
                            return error.TestExpectedEqual;
                        }
                        str_buffer.clearRetainingCapacity();
                    },
                }
            }
            try std.testing.expectEqual(.end, scanner.nextMarker());
        }
    }

    var tokenizer = Tokenizer.initComplete(src);
    assert(tokenizer.nextTypeNoUnderrun(.dtd) == .square_bracket_left);

    var scanner = internalSubsetSliceScanner(&tokenizer);
    for (expected_items, 0..) |expected_item, i| {
        errdefer std.log.err("Error occurred on item {d}", .{i});
        switch (expected_item) {
            .marker => |marker| try std.testing.expectEqual(marker, scanner.nextMarker()),
            .str => |expected_str| {
                const actual_str: ?[]const u8 = blk: {
                    try str_buffer.appendSlice(((try scanner.nextSegment()) orelse break :blk null).toStr(tokenizer.src));
                    while (try scanner.nextSegment()) |segment| {
                        try str_buffer.appendSlice(segment.toStr(tokenizer.src));
                    }
                    break :blk str_buffer.items;
                };
                if (expected_str != null and actual_str != null) {
                    try std.testing.expectEqualStrings(expected_str.?, actual_str.?);
                } else if ((expected_str == null) != (actual_str == null)) {
                    std.log.err("Expected {[expected_quote]s}{[expected]?}{[expected_quote]s}, got {[actual_quote]s}{[actual]?}{[actual_quote]s}", .{
                        .expected_quote = if (expected_str != null) "'" else "",
                        .expected = if (expected_str) |str| std.zig.fmtEscapes(str) else null,
                        .actual_quote = if (actual_str != null) "'" else "",
                        .actual = if (actual_str) |str| std.zig.fmtEscapes(str) else null,
                    });
                    return error.TestExpectedEqual;
                }
                str_buffer.clearRetainingCapacity();
            },
        }
    }
    try std.testing.expectEqual(.end, scanner.nextMarker());
}

test "1" {
    try testInternalSubsetScanner(
        &.{
            1,  2,  3,  4,  5,  6,  7,  8,  10,  12,  14,  16,  20,  24,
            28, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256,
        },
        \\[
        \\  <!-- ELEMENT EMPTY or ANY -->
        \\  <!ELEMENT a EMPTY>
        \\  <!ELEMENT b EMPTY >
        \\  <!ELEMENT c ANY>
        \\  <!ELEMENT d ANY >
        \\
        \\  <!-- ELEMENT Mixed -->
        \\  <!ELEMENT e (#PCDATA)>
        \\  <!ELEMENT f (#PCDATA)*>
        \\  <!ELEMENT g (#PCDATA|foo)*>
        \\  <!ELEMENT h (#PCDATA|bar|baz)*>
        \\
        \\  <!-- ELEMENT children -->
        \\  <!ELEMENT i (front, body, back?)+>
        \\  <!ELEMENT j (head, (p | list | note)*, div2*)>
        \\  <!ELEMENT k (div_mix | dict_mix)*>
        \\]
    ,
        &[_]ScannerTestItem{
            .{ .marker = .element_decl },
            .{ .str = "a" },
            .{ .marker = .{ .content_spec = .empty } },

            .{ .marker = .element_decl },
            .{ .str = "b" },
            .{ .marker = .{ .content_spec = .empty } },

            .{ .marker = .element_decl },
            .{ .str = "c" },
            .{ .marker = .{ .content_spec = .any } },

            .{ .marker = .element_decl },
            .{ .str = "d" },
            .{ .marker = .{ .content_spec = .any } },

            .{ .marker = .element_decl },
            .{ .str = "e" },
            .{ .marker = .{ .content_spec = .mixed } },
            .{ .str = null },
            .{ .marker = .{ .mixed_content_spec_end = .one } },

            .{ .marker = .element_decl },
            .{ .str = "f" },
            .{ .marker = .{ .content_spec = .mixed } },
            .{ .str = null },
            .{ .marker = .{ .mixed_content_spec_end = .zero_or_many } },

            .{ .marker = .element_decl },
            .{ .str = "g" },
            .{ .marker = .{ .content_spec = .mixed } },
            .{ .str = "foo" },
            .{ .str = null },
            .{ .marker = .{ .mixed_content_spec_end = .zero_or_many } },

            .{ .marker = .element_decl },
            .{ .str = "h" },
            .{ .marker = .{ .content_spec = .mixed } },
            .{ .str = "bar" },
            .{ .str = "baz" },
            .{ .str = null },
            .{ .marker = .{ .mixed_content_spec_end = .zero_or_many } },

            .{ .marker = .element_decl },
            .{ .str = "i" },
            .{ .marker = .{ .content_spec = .children } },
            .{ .marker = .{ .children_tok = .lparen } },
            .{ .marker = .{ .children_tok = .name } },
            .{ .str = "front" },
            .{ .marker = .{ .children_tok = .comma } },
            .{ .marker = .{ .children_tok = .name } },
            .{ .str = "body" },
            .{ .marker = .{ .children_tok = .comma } },
            .{ .marker = .{ .children_tok = .name } },
            .{ .str = "back" },
            .{ .marker = .{ .children_tok = .qmark } },
            .{ .marker = .{ .children_tok = .rparen } },
            .{ .marker = .{ .children_tok = .plus } },
            .{ .marker = .{ .children_tok = null } },

            .{ .marker = .element_decl },
            .{ .str = "j" },
            .{ .marker = .{ .content_spec = .children } },
            .{ .marker = .{ .children_tok = .lparen } },
            .{ .marker = .{ .children_tok = .name } },
            .{ .str = "head" },
            .{ .marker = .{ .children_tok = .comma } },
            .{ .marker = .{ .children_tok = .lparen } },
            .{ .marker = .{ .children_tok = .name } },
            .{ .str = "p" },
            .{ .marker = .{ .children_tok = .pipe } },
            .{ .marker = .{ .children_tok = .name } },
            .{ .str = "list" },
            .{ .marker = .{ .children_tok = .pipe } },
            .{ .marker = .{ .children_tok = .name } },
            .{ .str = "note" },
            .{ .marker = .{ .children_tok = .rparen } },
            .{ .marker = .{ .children_tok = .asterisk } },
            .{ .marker = .{ .children_tok = .comma } },
            .{ .marker = .{ .children_tok = .name } },
            .{ .str = "div2" },
            .{ .marker = .{ .children_tok = .asterisk } },
            .{ .marker = .{ .children_tok = .rparen } },
            .{ .marker = .{ .children_tok = null } },

            .{ .marker = .element_decl },
            .{ .str = "k" },
            .{ .marker = .{ .content_spec = .children } },
            .{ .marker = .{ .children_tok = .lparen } },
            .{ .marker = .{ .children_tok = .name } },
            .{ .str = "div_mix" },
            .{ .marker = .{ .children_tok = .pipe } },
            .{ .marker = .{ .children_tok = .name } },
            .{ .str = "dict_mix" },
            .{ .marker = .{ .children_tok = .rparen } },
            .{ .marker = .{ .children_tok = .asterisk } },
            .{ .marker = .{ .children_tok = null } },
        },
    );
}

const std = @import("std");
const assert = std.debug.assert;

const builtin = @import("builtin");

const iksemel = @import("iksemel.zig");
const Tokenizer = iksemel.Tokenizer;

const parse_helper = @import("parse_helper.zig");
