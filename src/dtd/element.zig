pub const FullScanner = Scanner(null);
pub inline fn fullScanner() FullScanner {
    return .{ .mbr = .{} };
}

pub fn StreamScanner(comptime Reader: type) type {
    return Scanner(Reader);
}
pub inline fn streamScanner(reader: anytype, read_buffer: []u8) StreamScanner(@TypeOf(reader)) {
    return .{
        .mbr = .{
            .reader = reader,
            .read_buffer = read_buffer,
        },
    };
}

pub const ContentSpecKind = enum {
    /// 'EMPTY'
    empty,
    /// 'ANY'
    any,
    /// `'(' S? '#PCDATA' (S? '|' S? Name)* S? ')*'`
    /// or
    /// `'(' S? '#PCDATA' S? ')'`
    mixed,
    /// `children ::= (choice | seq) ('?' | '*' | '+')?`
    /// `cp       ::= (Name | choice | seq) ('?' | '*' | '+')?`
    /// `choice   ::= '(' S? cp ( S? '|' S? cp )+ S? ')'`
    /// `seq      ::= '(' S? cp ( S? ',' S? cp )* S? ')'`
    children,
};

pub const MixedEnd = enum {
    /// The Mixed content spec is terminated by ')*'.
    /// This is always valid.
    zero_or_many,
    /// The Mixed content spec is terminated by ')'.
    /// This is only valid if there were no names listed.
    one,
};

pub const ChildrenTokenKind = enum {
    name,

    lparen,
    rparen,

    qmark,
    asterisk,
    plus,

    comma,
    pipe,
};

pub const ScanError = error{
    UnexpectedToken,
    UnexpectedEof,
};

pub fn Scanner(comptime MaybeReader: ?type) type {
    return struct {
        mbr: parse_helper.MaybeBufferedReader(MaybeReader),
        state: State = .start,
        const Self = @This();

        pub const SrcError = if (MaybeReader) |Reader| Reader.Error else error{};

        pub const TokenizerAPI = if (MaybeReader != null) Tokenizer.Stream else Tokenizer.Full;
        pub const Src = if (MaybeReader != null) []const u8 else Tokenizer.Range;

        /// This must be called first.
        /// Expects that the last token returned by `tokenizer` was `.angle_bracket_left_bang`, followed
        /// by a `.tag_token`, whose source satisfies `dtd.MarkupDeclKind.fromString(source) == .element`.
        /// See `nameNextSegment` next.
        pub fn expectName(scanner: *Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!void {
            assert(scanner.state == .start);
            defer assert(scanner.state != .start);
            errdefer scanner.state = .err;

            try scanner.expectAndSkipTagWhitespace(tokenizer.asTokenizer());

            switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,
                .tag_whitespace => unreachable,
                .tag_token => {},
            }
            scanner.state = .name;
        }

        /// This must be called second, and then until it returns null.
        /// The returned source segments should be concatenated in order
        /// to obtain the declared element's name.
        /// See `contentSpecKind` next.
        pub fn nameNextSegment(scanner: *Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!?Src {
            assert(scanner.state == .name);
            errdefer scanner.state = .err;

            if (try parse_helper.nextTokenSegment(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) |segment| {
                return segment;
            }
            scanner.state = .content_spec_kind;
            return null;
        }

        /// This must be called after `scanner.nameNextSegment(tokenizer) = null`.
        /// Returns the content specification kind.
        /// If this returns `.empty` or `.any`, we have reached the end of the
        /// element declaration.
        /// See `mixedCheckEnd`, and `childrenNextTokenKind` next.
        pub fn contentSpecKind(scanner: *Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!ContentSpecKind {
            assert(scanner.state == .content_spec_kind);
            defer assert(scanner.state != .content_spec_kind);
            errdefer scanner.state = .err;

            try scanner.expectAndSkipTagWhitespace(tokenizer.asTokenizer());

            switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,

                .tag_token => {
                    const maybe_empty_or_any = try parse_helper.nextTokenSrcAsEnum(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr, enum { EMPTY, ANY });
                    const empty_or_any = maybe_empty_or_any orelse return ScanError.UnexpectedToken;

                    switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) {
                        else => return ScanError.UnexpectedToken,
                        .eof => return ScanError.UnexpectedEof,
                        .tag_token => unreachable,
                        .angle_bracket_right => {},
                    }

                    scanner.state = .end;
                    return switch (empty_or_any) {
                        .EMPTY => .empty,
                        .ANY => .any,
                    };
                },

                .lparen => {},
            }

            switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,

                .hashtag => {
                    switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) {
                        else => return ScanError.UnexpectedToken,
                        .eof => return ScanError.UnexpectedEof,
                        .tag_token => {},
                    }

                    if (try parse_helper.nextTokenSrcAsEnum(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr, enum { PCDATA }) == null) {
                        return ScanError.UnexpectedToken;
                    }

                    scanner.state = .mixed_check;
                    return .mixed;
                },

                .lparen => {
                    scanner.state = .children_queue_lparen_lparen;
                    return .children;
                },
                .tag_token => {
                    scanner.state = .children_queue_lparen_name;
                    return .children;
                },
            }
        }

        /// This must be called after `scanner.contentSpecKind(tokenizer) = .mixed`, or
        /// after `scanner.mixedNameNextSegment(tokenizer) = null`.
        /// If this returns null, it indicates we have not reached the end yet, and
        /// there is a(nother) name in the list.
        /// If this returns a non-null value, it represents how the Mixed content spec
        /// was terminated, and that we have reached the end of the element declaration.
        /// See `mixedNameNextSegment` next.
        pub fn mixedCheckEnd(scanner: *Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!?MixedEnd {
            assert(scanner.state == .mixed_check);
            defer assert(scanner.state != .mixed_check);
            errdefer scanner.state = .err;

            return switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,
                .tag_whitespace => unreachable,

                .rparen => blk: {
                    const immediate_tt: enum {
                        asterisk,
                        tag_whitespace,
                        angle_bracket_right,
                    } = switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) {
                        else => return ScanError.UnexpectedToken,
                        .eof => return ScanError.UnexpectedEof,

                        .asterisk => .asterisk,
                        .tag_whitespace => .tag_whitespace,
                        .angle_bracket_right => .angle_bracket_right,
                    };

                    switch (immediate_tt) {
                        .asterisk => {},
                        .tag_whitespace => try parse_helper.skipWhitespaceSrcUnchecked(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr),
                        .angle_bracket_right => {},
                    }

                    const next_non_whitespace_tt: Tokenizer.TokenType.Subset(.markup) = switch (immediate_tt) {
                        .asterisk => try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr),
                        .tag_whitespace => try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr),
                        .angle_bracket_right => .angle_bracket_right,
                    };

                    switch (next_non_whitespace_tt) {
                        else => return ScanError.UnexpectedToken,
                        .eof => return ScanError.UnexpectedEof,
                        .tag_whitespace => unreachable,
                        .angle_bracket_right => {},
                    }

                    scanner.state = .end;
                    break :blk switch (immediate_tt) {
                        .asterisk => .zero_or_many,
                        .tag_whitespace, .angle_bracket_right => .one,
                    };
                },

                .pipe => blk: {
                    switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) {
                        else => return ScanError.UnexpectedToken,
                        .eof => return ScanError.UnexpectedEof,
                        .tag_token => {},
                    }
                    scanner.state = .mixed_name;
                    break :blk null;
                },
            };
        }

        /// This must be called after `scanner.mixedCheckEnd(tokenizer) = null`.
        /// The returned source segments should be concatenated in order
        /// to obtain the listed name.
        /// See `mixedCheckEnd` next.
        pub fn mixedNameNextSegment(scanner: *Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!?Src {
            assert(scanner.state == .mixed_name);
            errdefer scanner.state = .err;

            if (try parse_helper.nextTokenSegment(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) |segment| {
                return segment;
            }
            scanner.state = .mixed_check;
            return null;
        }

        /// This must be called after `scanner.contentSpecKind(tokenizer) = .children`,
        /// after `scanner.childrenNextTokenKind(tokenizer) != null`, after
        /// `scanner.childrenNextTokenKind(tokenizer) != .name`, or after
        /// `scanner.childrenNameNextSegment(tokenizer) = null`.
        /// If the returned value is null, we have reached the end of the element declaration.
        /// If the returned vale is non-null, it indicates the token type of the children
        /// content spec. Specifically the `.name` token has its source returned as a string;
        /// all others are represented by the tag.
        pub fn childrenNextTokenKind(scanner: *Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!?ChildrenTokenKind {
            errdefer scanner.state = .err;

            const Category = enum {
                lparen,
                rparen,
                name,
                quantifier,
                separator,
            };

            const prev_kind: Category = switch (scanner.state) {
                else => unreachable,

                .children_queue_lparen_lparen => {
                    scanner.state = .children_queue_lparen;
                    return .lparen;
                },
                .children_queue_lparen_name => {
                    scanner.state = .children_queue_name;
                    return .lparen;
                },

                .children_queue_lparen => {
                    scanner.state = .children_lparen;
                    return .lparen;
                },
                .children_queue_name => {
                    scanner.state = .children_name;
                    return .name;
                },

                .children_lparen => .lparen,
                .children_rparen => .rparen,
                .children_name => .name,
                .children_quantifier => .quantifier,
                .children_separator => .separator,
            };

            const child_tok_kind: ChildrenTokenKind = switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,

                .angle_bracket_right => {
                    scanner.state = .end;
                    return null;
                },

                .tag_whitespace => blk: {
                    try parse_helper.skipWhitespaceSrcUnchecked(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr);
                    break :blk switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) {
                        else => return ScanError.UnexpectedToken,
                        .eof => return ScanError.UnexpectedEof,
                        .tag_whitespace => unreachable,

                        .angle_bracket_right => {
                            scanner.state = .end;
                            return null;
                        },

                        .lparen => .lparen,
                        .rparen => .rparen,
                        .tag_token => .name,

                        .comma => .comma,
                        .pipe => .pipe,
                    };
                },

                .lparen => .lparen,
                .rparen => .rparen,
                .tag_token => .name,

                .qmark => .qmark,
                .asterisk => .asterisk,
                .plus => .plus,

                .comma => .comma,
                .pipe => .pipe,
            };

            const next_kind: Category = switch (child_tok_kind) {
                .lparen => .lparen,
                .rparen => .rparen,
                .name => .name,
                .qmark, .asterisk, .plus => .quantifier,
                .comma, .pipe => .separator,
            };

            const trans = struct {
                const TransBits = packed struct { prev: Category, next: Category };
                const TransInt = std.meta.Int(.unsigned, @bitSizeOf(TransBits));
                fn trans(prev: Category, next: Category) TransInt {
                    return @bitCast(TransBits{ .prev = prev, .next = next });
                }
            }.trans;

            switch (trans(prev_kind, next_kind)) {
                trans(.lparen, .lparen) => {},
                trans(.lparen, .rparen) => return ScanError.UnexpectedToken,
                trans(.lparen, .name) => {},
                trans(.lparen, .quantifier) => return ScanError.UnexpectedToken,
                trans(.lparen, .separator) => return ScanError.UnexpectedToken,

                trans(.rparen, .lparen) => return ScanError.UnexpectedToken,
                trans(.rparen, .rparen) => {},
                trans(.rparen, .name) => return ScanError.UnexpectedToken,
                trans(.rparen, .quantifier) => {},
                trans(.rparen, .separator) => {},

                trans(.name, .lparen) => return ScanError.UnexpectedToken,
                trans(.name, .rparen) => {},
                trans(.name, .name) => return ScanError.UnexpectedToken,
                trans(.name, .quantifier) => {},
                trans(.name, .separator) => {},

                trans(.quantifier, .lparen) => return ScanError.UnexpectedToken,
                trans(.quantifier, .rparen) => {},
                trans(.quantifier, .name) => return ScanError.UnexpectedToken,
                trans(.quantifier, .quantifier) => return ScanError.UnexpectedToken,
                trans(.quantifier, .separator) => {},

                trans(.separator, .lparen) => {},
                trans(.separator, .rparen) => return ScanError.UnexpectedToken,
                trans(.separator, .name) => {},
                trans(.separator, .quantifier) => return ScanError.UnexpectedToken,
                trans(.separator, .separator) => return ScanError.UnexpectedToken,

                else => unreachable,
            }

            scanner.state = switch (next_kind) {
                .lparen => .children_lparen,
                .rparen => .children_rparen,
                .name => .children_name,
                .quantifier => .children_quantifier,
                .separator => .children_separator,
            };
            return child_tok_kind;
        }

        pub fn childrenNameNextSegment(scanner: *Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!?Src {
            assert(scanner.state == .children_name);
            errdefer scanner.state = .err;

            if (try parse_helper.nextTokenSegment(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) |segment| {
                return segment;
            }
            return null;
        }

        fn expectAndSkipTagWhitespace(scanner: *const Self, tokenizer: *Tokenizer) (SrcError || ScanError)!void {
            if (try parse_helper.skipIfTagWhitespaceOrGetNextTokType(tokenizer, .markup, MaybeReader, scanner.mbr)) |tt| switch (tt) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,
                .tag_whitespace => unreachable,
            };
        }

        const State = enum {
            start,
            end,
            err,

            name,
            content_spec_kind,

            mixed_check,
            mixed_name,

            children_queue_lparen_lparen,
            children_queue_lparen_name,

            children_queue_lparen,
            children_queue_name,

            children_lparen,
            children_rparen,
            children_name,
            children_quantifier,
            children_separator,
        };
    };
}

const ScannerTestValues = struct {
    name: []const u8,
    content_spec: ContentSpec,

    fn deinit(stv: ScannerTestValues, allocator: std.mem.Allocator) void {
        allocator.free(stv.name);
        stv.content_spec.deinit(allocator);
    }

    const ChildrenToken = union(ChildrenTokenKind) {
        name: []const u8,

        lparen,
        rparen,

        qmark,
        asterisk,
        plus,

        comma,
        pipe,

        fn deinit(ct: ChildrenToken, allocator: std.mem.Allocator) void {
            switch (ct) {
                .name => |name| allocator.free(name),
                inline else => |empty| empty,
            }
        }

        fn expectEqual(expected: ChildrenToken, actual: ChildrenToken) !void {
            try std.testing.expectEqual(std.meta.activeTag(expected), std.meta.activeTag(actual));
            switch (expected) {
                .name => |name| try std.testing.expectEqualStrings(name, actual.name),
                inline else => |empty| empty,
            }
        }
    };

    const ContentSpec = union(ContentSpecKind) {
        empty,
        any,
        mixed: struct { []const []const u8, MixedEnd },
        children: []const ChildrenToken,

        fn deinit(content_spec: ContentSpec, allocator: std.mem.Allocator) void {
            switch (content_spec) {
                .empty, .any => |empty| empty,
                .mixed => |mixed| {
                    const names, const end = mixed;
                    _ = end;
                    for (names) |name| allocator.free(name);
                    allocator.free(names);
                },
                .children => |children_toks| {
                    for (children_toks) |children_tok| children_tok.deinit(allocator);
                    allocator.free(children_toks);
                },
            }
        }

        fn expectEqual(expected: ContentSpec, actual: ContentSpec) !void {
            try std.testing.expectEqual(std.meta.activeTag(expected), std.meta.activeTag(actual));
            switch (expected) {
                .empty, .any => {},
                .mixed => |expected_mixed| {
                    const expected_names, const expected_end = expected_mixed;
                    const actual_names, const actual_end = actual.mixed;

                    try std.testing.expectEqual(expected_end, actual_end);

                    const common_len = @min(expected_names.len, actual_names.len);
                    for (expected_names[0..common_len], actual_names[0..common_len], 0..common_len) |expected_name, actual_name, i| {
                        errdefer std.log.err("Difference occurred on name {d}", .{i});
                        try std.testing.expectEqualStrings(expected_name, actual_name);
                    }
                    try std.testing.expectEqual(expected_names.len, actual_names.len);
                },
                .children => |expected_toks| {
                    const actual_toks = actual.children;
                    const common_len = @min(expected_toks.len, actual_toks.len);
                    for (expected_toks[0..common_len], actual_toks[0..common_len], 0..common_len) |expected_tok, actual_tok, i| {
                        errdefer std.log.err("Difference occurred on name {d}", .{i});
                        try expected_tok.expectEqual(actual_tok);
                    }
                    try std.testing.expectEqual(expected_toks.len, actual_toks.len);
                },
            }
        }
    };

    fn expectEqual(expected: ScannerTestValues, actual: ScannerTestValues) !void {
        errdefer std.log.err("\nExpected:\n{}\nGot:\n{}", .{ expected, actual });
        try std.testing.expectEqualStrings(expected.name, actual.name);
        try expected.content_spec.expectEqual(actual.content_spec);
    }

    fn parse(
        allocator: std.mem.Allocator,
        comptime MaybeReader: ?type,
        tokenizer: *Scanner(MaybeReader).TokenizerAPI,
        mbr: parse_helper.MaybeBufferedReader(MaybeReader),
    ) !ScannerTestValues {
        const src = if (MaybeReader == null) tokenizer.asTokenizer().src;
        var scanner = if (MaybeReader != null) streamScanner(mbr.reader, mbr.read_buffer) else fullScanner();

        try scanner.expectName(tokenizer);

        const name: []const u8 = str: {
            var name = std.ArrayList(u8).init(allocator);
            defer name.deinit();

            while (try scanner.nameNextSegment(tokenizer)) |segment| {
                const segment_str = if (MaybeReader != null) segment else segment.toStr(src);
                try name.appendSlice(segment_str);
            }

            break :str try name.toOwnedSlice();
        };
        errdefer allocator.free(name);

        const content_spec_kind = try scanner.contentSpecKind(tokenizer);
        const content_spec: ContentSpec = switch (content_spec_kind) {
            inline .empty, .any => |tag| @unionInit(ContentSpec, @tagName(tag), {}),
            .mixed => .{ .mixed = mixed: {
                var name_list = std.ArrayList([]const u8).init(allocator);
                defer {
                    for (name_list.items) |listed_name| allocator.free(listed_name);
                    name_list.deinit();
                }

                const end: MixedEnd = while (true) {
                    if (try scanner.mixedCheckEnd(tokenizer)) |end| break end;

                    const listed_name: []const u8 = str: {
                        var listed_name = std.ArrayList(u8).init(allocator);
                        defer listed_name.deinit();

                        while (try scanner.mixedNameNextSegment(tokenizer)) |segment| {
                            const segment_str = if (MaybeReader != null) segment else segment.toStr(src);
                            try listed_name.appendSlice(segment_str);
                        }

                        break :str try listed_name.toOwnedSlice();
                    };
                    errdefer allocator.free(listed_name);

                    try name_list.append(listed_name);
                };

                break :mixed .{ try name_list.toOwnedSlice(), end };
            } },
            .children => .{ .children = children: {
                var children_tokens = std.ArrayList(ChildrenToken).init(allocator);
                defer {
                    for (children_tokens.items) |children_tok| children_tok.deinit(allocator);
                    children_tokens.deinit();
                }

                while (try scanner.childrenNextTokenKind(tokenizer)) |tok_kind| {
                    try children_tokens.ensureUnusedCapacity(1);
                    children_tokens.appendAssumeCapacity(switch (tok_kind) {
                        inline else => |tag| @unionInit(ChildrenToken, @tagName(tag), {}),
                        .name => .{ .name = str: {
                            var child_name = std.ArrayList(u8).init(allocator);
                            defer child_name.deinit();

                            while (try scanner.childrenNameNextSegment(tokenizer)) |segment| {
                                const segment_str = if (MaybeReader != null) segment else segment.toStr(src);
                                try child_name.appendSlice(segment_str);
                            }

                            break :str try child_name.toOwnedSlice();
                        } },
                    });
                }

                break :children try children_tokens.toOwnedSlice();
            } },
        };
        errdefer content_spec.deinit(allocator);

        return .{
            .name = name,
            .content_spec = content_spec,
        };
    }
};

fn testScanner(
    buffer_sizes: []const usize,
    /// Should begin with '<!ENTITY'
    src: []const u8,
    expected: ScannerTestValues,
) !void {
    {
        const max_buffer = try std.testing.allocator.alloc(u8, std.mem.max(usize, buffer_sizes));
        defer std.testing.allocator.free(max_buffer);

        for (buffer_sizes) |buffer_size| {
            const read_buffer = max_buffer[0..buffer_size];
            var fbs = std.io.fixedBufferStream(src);
            var tokenizer = Tokenizer.initStream();

            const Reader = @TypeOf(fbs).Reader;
            const mbr: parse_helper.MaybeBufferedReader(Reader) = .{
                .reader = fbs.reader(),
                .read_buffer = read_buffer,
            };

            try expectElementDeclStart(Reader, &tokenizer.stream, mbr);
            const actual = try ScannerTestValues.parse(std.testing.allocator, Reader, &tokenizer.stream, mbr);
            defer actual.deinit(std.testing.allocator);
            try expected.expectEqual(actual);
        }
    }

    var tokenizer = Tokenizer.initFull(src);

    try expectElementDeclStart(null, &tokenizer.full, .{});
    const actual = try ScannerTestValues.parse(std.testing.allocator, null, &tokenizer.full, .{});
    defer actual.deinit(std.testing.allocator);
    try expected.expectEqual(actual);
}

fn expectElementDeclStart(
    comptime MaybeReader: ?type,
    tokenizer: *Scanner(MaybeReader).TokenizerAPI,
    mbr: parse_helper.MaybeBufferedReader(MaybeReader),
) !void {
    const src = if (MaybeReader == null) tokenizer.asTokenizer().src;
    try std.testing.expectEqual(.angle_bracket_left_bang, try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .markup, MaybeReader, mbr));
    try std.testing.expectEqual(.tag_token, try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .markup, MaybeReader, mbr));
    var bstr: std.BoundedArray(u8, iksemel.dtd.MarkupDeclKind.max_str_len) = .{};
    while (try parse_helper.nextTokenSegment(tokenizer.asTokenizer(), .markup, MaybeReader, mbr)) |segment| {
        const segment_str: []const u8 = if (MaybeReader != null) segment else segment.toStr(src);
        bstr.appendSlice(segment_str) catch return error.TestExpectedEqual;
    }
    errdefer std.log.err("Actual: '{}'", .{std.zig.fmtEscapes(bstr.constSlice())});
    try std.testing.expectEqual(.element, iksemel.dtd.MarkupDeclKind.fromString(bstr.constSlice()));
}

test "EMPTY or ANY" {
    const buf_sizes = [_]usize{
        1,  2,  3,  4,  5,  6,  7,  8,  10,  12,  14,  16,  20,  24,
        28, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256,
    };

    try testScanner(&buf_sizes, "<!ELEMENT foo EMPTY>", .{ .name = "foo", .content_spec = .empty });
    try testScanner(&buf_sizes, "<!ELEMENT foo EMPTY >", .{ .name = "foo", .content_spec = .empty });

    try testScanner(&buf_sizes, "<!ELEMENT foo ANY>", .{ .name = "foo", .content_spec = .any });
    try testScanner(&buf_sizes, "<!ELEMENT foo ANY >", .{ .name = "foo", .content_spec = .any });
}

test "Mixed" {
    const buf_sizes = [_]usize{
        1,  2,  3,  4,  5,  6,  7,  8,  10,  12,  14,  16,  20,  24,
        28, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256,
    };

    try testScanner(&buf_sizes, "<!ELEMENT foo (#PCDATA)>", .{
        .name = "foo",
        .content_spec = .{ .mixed = .{ &.{}, .one } },
    });

    try testScanner(&buf_sizes, "<!ELEMENT foo (#PCDATA)*>", .{
        .name = "foo",
        .content_spec = .{ .mixed = .{ &.{}, .zero_or_many } },
    });

    try testScanner(&buf_sizes, "<!ELEMENT foo (#PCDATA| bar)>", .{
        .name = "foo",
        .content_spec = .{
            .mixed = .{
                // this is invalid, but can be handled by the programmer
                &.{"bar"}, .one,
            },
        },
    });

    try testScanner(&buf_sizes, "<!ELEMENT foo (#PCDATA |bar)*>", .{
        .name = "foo",
        .content_spec = .{ .mixed = .{
            &.{"bar"}, .zero_or_many,
        } },
    });

    try testScanner(&buf_sizes, "<!ELEMENT foo (#PCDATA| bar |baz)*>", .{
        .name = "foo",
        .content_spec = .{ .mixed = .{
            &.{ "bar", "baz" }, .zero_or_many,
        } },
    });

    try testScanner(&buf_sizes, "<!ELEMENT foo (#PCDATA| bar |baz | fizz|buzz)*>", .{
        .name = "foo",
        .content_spec = .{ .mixed = .{
            &.{ "bar", "baz", "fizz", "buzz" }, .zero_or_many,
        } },
    });
}

test "children" {
    const buf_sizes = [_]usize{
        1,  2,  3,  4,  5,  6,  7,  8,  10,  12,  14,  16,  20,  24,
        28, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256,
    };

    const name = struct {
        inline fn name(str: []const u8) ScannerTestValues.ChildrenToken {
            return .{ .name = str };
        }
    }.name;

    try testScanner(&buf_sizes, "<!ELEMENT foo (front, body, back?)+>", .{
        .name = "foo",
        .content_spec = .{ .children = &.{
            .lparen,

            name("front"),
            .comma,
            name("body"),
            .comma,
            name("back"),
            .qmark,

            .rparen,
            .plus,
        } },
    });

    try testScanner(&buf_sizes, "<!ELEMENT lorem (head, (p | list | note)*, div2*)>", .{
        .name = "lorem",
        .content_spec = .{ .children = &.{
            .lparen,

            name("head"),

            .comma,

            .lparen,
            name("p"),
            .pipe,
            name("list"),
            .pipe,
            name("note"),
            .rparen,
            .asterisk,

            .comma,

            name("div2"),
            .asterisk,

            .rparen,
        } },
    });

    try testScanner(&buf_sizes, "<!ELEMENT k (div_mix | dict_mix)*>", .{
        .name = "k",
        .content_spec = .{ .children = &.{
            .lparen,
            name("div_mix"),
            .pipe,
            name("dict_mix"),
            .rparen,
            .asterisk,
        } },
    });
}

test "children invalid nesting" { // these are examples of the fact that the scanner doesn't track nesting depth, it must instead be tracked by the programmer
    const buf_sizes = [_]usize{
        1,  2,  3,  4,  5,  6,  7,  8,  10,  12,  14,  16,  20,  24,
        28, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256,
    };

    const name = struct {
        inline fn name(str: []const u8) ScannerTestValues.ChildrenToken {
            return .{ .name = str };
        }
    }.name;

    try testScanner(&buf_sizes, "<!ELEMENT xyz (a))>", .{
        .name = "xyz",
        .content_spec = .{ .children = &.{
            .lparen,
            name("a"),
            .rparen,
            .rparen,
        } },
    });

    try testScanner(&buf_sizes, "<!ELEMENT abc ((x)*>", .{
        .name = "abc",
        .content_spec = .{ .children = &.{
            .lparen,
            .lparen,
            name("x"),
            .rparen,
            .asterisk,
        } },
    });

    try testScanner(&buf_sizes, "<!ELEMENT bb ((wwww),vvv*>", .{
        .name = "bb",
        .content_spec = .{ .children = &.{
            .lparen,
            .lparen,
            name("wwww"),
            .rparen,
            .comma,
            name("vvv"),
            .asterisk,
        } },
    });
}

const std = @import("std");
const assert = std.debug.assert;

const iksemel = @import("../iksemel.zig");
const Tokenizer = iksemel.Tokenizer;

const parse_helper = @import("../parse_helper.zig");
const test_helper = @import("../test_helper.zig");
