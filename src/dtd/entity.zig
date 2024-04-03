//! Facilitates the scanning of entity declarations in the DTD Internal Subset.

pub const Kind = enum {
    general,
    parameter,
};

pub const DefinitionKind = enum {
    entity_value,
    external_id_public,
    external_id_system,
};

pub const ValueStrKind = enum {
    /// Run of text data.
    text,
    /// General Entity Reference.
    /// `'&' Name ';'`
    ge_ref,
    /// Parameter Entity Reference.
    /// `'%' Name ';'`
    pe_ref,
    /// Character Entity Reference.
    /// `'&#' Digits ';'`
    char_ref,
};

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
        state: State = .start,
        const Self = @This();

        pub const SrcError = if (MaybeReader) |Reader| Reader.Error else error{};
        pub const ScanError = error{
            UnexpectedToken,
            UnexpectedEof,
        };

        pub const TokenizerAPI = if (MaybeReader != null) Tokenizer.Stream else Tokenizer.Full;
        pub const Src = if (MaybeReader != null) []const u8 else Tokenizer.Range;

        /// This must be called first.
        /// Returns the entity kind.
        /// See `nameNextSegment` next.
        pub fn entityKind(scanner: *Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!Kind {
            assert(scanner.state == .start);
            errdefer scanner.state = .err;
            defer scanner.state = .name;

            try scanner.expectAndSkipTagWhitespace(tokenizer);

            switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr())) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,
                .tag_whitespace => unreachable,
                .tag_token => return .general,
                .percent => {},
            }

            try scanner.expectAndSkipTagWhitespace(tokenizer);

            switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr())) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,
                .tag_whitespace => unreachable,
                .tag_token => return .parameter,
            }
        }

        /// This must be called second, and then until it returns null.
        /// The returned source segments should be concatenated in order
        /// to obtain the declared entity's name.
        /// See `definitionKind` next.
        pub fn nameNextSegment(scanner: *Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!?Src {
            assert(scanner.state == .name);
            errdefer scanner.state = .err;

            if (try parse_helper.nextTokenSegment(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr())) |segment| {
                return segment;
            }
            scanner.state = .value_kind;
            return null;
        }

        /// This must be called after `scanner.nameNextSegment(tokenizer) = null`.
        /// Returns the entity definition kind.
        /// See `entityValueNextStrKind`, `pubidLiteralNextSegment`, and `systemLiteralNextSegment` next.
        pub fn definitionKind(scanner: *Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!DefinitionKind {
            assert(scanner.state == .value_kind);
            errdefer scanner.state = .err;

            try scanner.expectAndSkipTagWhitespace(tokenizer);

            switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr())) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,
                .tag_whitespace => unreachable,

                inline //
                .quote_single,
                .quote_double,
                => |quote_tt| {
                    scanner.state = switch (comptime Tokenizer.QuoteType.fromTokenType(Tokenizer.TokenType.fromNarrow(quote_tt)).?) {
                        .single => .entity_value_kind_sq,
                        .double => .entity_value_kind_dq,
                    };
                    return .entity_value;
                },

                .tag_token => {
                    const maybe_pub_or_sys = try parse_helper.nextTokenSrcAsEnum(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr(), enum { PUBLIC, SYSTEM });
                    const pub_or_sys = maybe_pub_or_sys orelse return ScanError.UnexpectedToken;

                    try scanner.expectAndSkipTagWhitespace(tokenizer);
                    const quote_type, const content = try scanner.expectSystemLiteral(tokenizer);

                    scanner.state = switch (content) {
                        .text => switch (pub_or_sys) {
                            .PUBLIC => switch (quote_type) {
                                .single => .pub_literal_sq,
                                .double => .pub_literal_dq,
                            },
                            .SYSTEM => switch (quote_type) {
                                .single => .sys_literal_sq,
                                .double => .sys_literal_dq,
                            },
                        },
                        .empty => switch (pub_or_sys) {
                            .PUBLIC => .pub_literal_empty,
                            .SYSTEM => .sys_literal_empty,
                        },
                    };

                    return switch (pub_or_sys) {
                        .PUBLIC => .external_id_public,
                        .SYSTEM => .external_id_system,
                    };
                },
            }
        }

        /// This must be called after `scanner.definitionKind(tokenizer) = .entity_value`, or
        /// after `scanner.entityValueNextStrSegment(tokenizer) = null`.
        /// If this returns a non-null value, it indicates the meaning of the next string.
        /// If this returns null, it indicates the string has ended and we have reached the
        /// end of the entity declaration.
        /// See `entityValueNextStrSegment` next.
        pub fn entityValueNextStrKind(scanner: *Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!?ValueStrKind {
            const quote_type: Tokenizer.QuoteType = switch (scanner.state) {
                .entity_value_kind_sq => .single,
                .entity_value_kind_dq => .double,
                else => unreachable,
            };
            defer assert(switch (quote_type) {
                .single => scanner.state != .entity_value_kind_sq,
                .double => scanner.state != .entity_value_kind_dq,
            });
            errdefer scanner.state = .err;

            switch (try parse_helper.nextTokenType(tokenizer.asTokenizer(), quote_type.entityValueCtx(), MaybeReader, scanner.mbr())) {
                else => unreachable,
                .eof => return ScanError.UnexpectedEof,
                .text_data => {
                    scanner.state = switch (quote_type) {
                        .single => .entity_value_str_sq,
                        .double => .entity_value_str_dq,
                    };
                    return .text;
                },

                .percent => {
                    switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .reference, MaybeReader, scanner.mbr())) {
                        .eof,
                        => return ScanError.UnexpectedEof,

                        .hashtag,
                        .semicolon,
                        .invalid_reference_end,
                        => return ScanError.UnexpectedToken,

                        .tag_token => {},
                    }

                    scanner.state = switch (quote_type) {
                        .single => .entity_value_ref_sq,
                        .double => .entity_value_ref_dq,
                    };
                    return .pe_ref;
                },

                .ampersand => {
                    const ref_kind: ValueStrKind = switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .reference, MaybeReader, scanner.mbr())) {
                        .eof => return ScanError.UnexpectedEof,

                        .semicolon,
                        .invalid_reference_end,
                        => return ScanError.UnexpectedToken,

                        .hashtag => switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .reference, MaybeReader, scanner.mbr())) {
                            .eof => return ScanError.UnexpectedEof,

                            .hashtag,
                            .semicolon,
                            .invalid_reference_end,
                            => return ScanError.UnexpectedToken,

                            .tag_token => .char_ref,
                        },
                        .tag_token => .ge_ref,
                    };

                    scanner.state = switch (quote_type) {
                        .single => .entity_value_ref_sq,
                        .double => .entity_value_ref_dq,
                    };
                    return ref_kind;
                },

                .quote_single,
                .quote_double,
                => {},
            }

            switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr())) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,
                .tag_whitespace => unreachable,
                .angle_bracket_right => {},
            }

            scanner.state = .end;
            return null;
        }

        /// This must be called after `scanner.entityValueNextStrKind(tokenizer) != null`, and
        /// then until it returns null.
        /// The returned source segments should be concatenated in order
        /// to obtain the content of the text or the reference name.
        /// See `entityValueNextStrKind` next.
        pub fn entityValueNextStrSegment(scanner: *Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!?Src {
            const quote_type: Tokenizer.QuoteType, //
            const is_ref: bool //
            = switch (scanner.state) {
                .entity_value_str_sq => .{ .single, false },
                .entity_value_str_dq => .{ .double, false },

                .entity_value_ref_sq => .{ .single, true },
                .entity_value_ref_dq => .{ .double, true },

                else => unreachable,
            };
            errdefer scanner.state = .err;

            const ref_or_str_context = if (is_ref) .reference else quote_type.entityValueCtx();
            if (try parse_helper.nextTokenSegment(tokenizer.asTokenizer(), ref_or_str_context, MaybeReader, scanner.mbr())) |segment| {
                return segment;
            }

            if (is_ref) switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .reference, MaybeReader, scanner.mbr())) {
                .eof => return ScanError.UnexpectedEof,
                .hashtag,
                .invalid_reference_end,
                => return ScanError.UnexpectedToken,
                .tag_token => unreachable,
                .semicolon => {},
            };

            scanner.state = switch (quote_type) {
                .single => .entity_value_kind_sq,
                .double => .entity_value_kind_dq,
            };
            return null;
        }

        /// This must be called after `scanner.definitionKind(tokenizer) = .external_id_public`, and
        /// then until it returns null.
        /// The returned source segments should be concatenated in order
        /// to obtain the content of the Pubid Literal.
        /// See `systemLiteralNextSegment` next.
        pub fn pubidLiteralNextSegment(scanner: *Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!?Src {
            errdefer scanner.state = .err;
            switch (scanner.state) {
                .pub_literal_sq,
                .pub_literal_dq,
                => |state_tag| {
                    const quote_type: Tokenizer.QuoteType = switch (state_tag) {
                        .pub_literal_sq => .single,
                        .pub_literal_dq => .double,
                        else => unreachable,
                    };
                    if (try parse_helper.nextTokenSegment(tokenizer.asTokenizer(), quote_type.systemLiteralCtx(), MaybeReader, scanner.mbr())) |segment| {
                        return segment;
                    }
                    try scanner.expectSystemLiteralEndQuoteAfterTextData(tokenizer, quote_type);
                },

                .pub_literal_empty => {},
                else => unreachable,
            }

            try scanner.expectAndSkipTagWhitespace(tokenizer);
            const quote_type, const content = try scanner.expectSystemLiteral(tokenizer);

            scanner.state = switch (content) {
                .text => switch (quote_type) {
                    .single => .sys_literal_sq,
                    .double => .sys_literal_dq,
                },
                .empty => .sys_literal_empty,
            };

            return null;
        }

        /// This must be called after `scanner.definitionKind(tokenizer) = .external_id_system`, or
        /// after `scanner.pubidLiteralNextSegment(tokenizer) = null`, and then until it returns null.
        /// The returned source segments should be concatenated in order to obtain the content of the
        /// System Literal.
        /// See `nDataDeclPresent` next.
        pub fn systemLiteralNextSegment(scanner: *Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!?Src {
            errdefer scanner.state = .err;
            switch (scanner.state) {
                .sys_literal_sq,
                .sys_literal_dq,
                => |state_tag| {
                    const quote_type: Tokenizer.QuoteType = switch (state_tag) {
                        .sys_literal_sq => .single,
                        .sys_literal_dq => .double,
                        else => unreachable,
                    };
                    if (try parse_helper.nextTokenSegment(tokenizer.asTokenizer(), quote_type.systemLiteralCtx(), MaybeReader, scanner.mbr())) |segment| {
                        return segment;
                    }
                    try scanner.expectSystemLiteralEndQuoteAfterTextData(tokenizer, quote_type);
                },

                .sys_literal_empty => {},
                else => unreachable,
            }

            scanner.state = .check_for_ndata_decl;
            return null;
        }

        /// This must be called after `scanner.systemLiteralNextSegment(tokenizer) = null`.
        /// Returns true if there is an NDataDecl following the system literal (`'NDATA' Name`).
        /// Otherwise returns false, indicating we have reached the end of the entity declaration.
        /// NOTE: If it was the case that `scanner.kind(tokenizer) = .parameter`, this returning
        /// true means the declaration is invalid.
        /// See `nDataDeclNameNextSegment` next.
        pub fn nDataDeclPresent(scanner: *Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!bool {
            assert(scanner.state == .check_for_ndata_decl);
            defer assert(scanner.state != .check_for_ndata_decl);
            errdefer scanner.state = .err;

            switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr())) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,

                .angle_bracket_right => {
                    scanner.state = .end;
                    return false;
                },
                .tag_whitespace => {},
            }
            try parse_helper.skipWhitespaceSrcUnchecked(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr());

            switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr())) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,
                .tag_whitespace => unreachable,

                .angle_bracket_right => {
                    scanner.state = .end;
                    return false;
                },

                .tag_token => {},
            }

            if (try parse_helper.nextTokenSrcAsEnum(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr(), enum { NDATA }) == null) {
                return ScanError.UnexpectedToken;
            }

            try scanner.expectAndSkipTagWhitespace(tokenizer);

            switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr())) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,
                .tag_whitespace => unreachable,

                .tag_token => {},
            }

            scanner.state = .ndata_decl_name;
            return true;
        }

        /// This must be called after `scanner.nDataDeclPresent(tokenizer) = true`, and then until
        /// it returns null.
        /// The returned source segments should be concatenated in order to obtain the referenced
        /// notation's name.
        /// After this returns null, we have reached the end of the entity declaration.
        pub fn nDataDeclNameNextSegment(scanner: *Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!?Src {
            assert(scanner.state == .ndata_decl_name);
            errdefer scanner.state = .err;

            if (try parse_helper.nextTokenSegment(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr())) |segment| {
                return segment;
            }

            switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr())) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,
                .tag_whitespace => unreachable,
                .angle_bracket_right => {},
            }

            scanner.state = .end;
            return null;
        }

        fn expectSystemLiteralEndQuoteAfterTextData(
            scanner: *const Self,
            tokenizer: *TokenizerAPI,
            quote_type: Tokenizer.QuoteType,
        ) !void {
            switch (try parse_helper.nextTokenType(tokenizer.asTokenizer(), quote_type.systemLiteralCtx(), MaybeReader, scanner.mbr())) {
                else => unreachable,
                .eof => return ScanError.UnexpectedEof,
                .text_data => unreachable,

                .quote_single,
                .quote_double,
                => {},
            }
        }

        const SystemLiteralContent = enum { text, empty };
        fn expectSystemLiteral(scanner: *const Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!struct { Tokenizer.QuoteType, SystemLiteralContent } {
            const quote_type: Tokenizer.QuoteType = switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr())) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,

                .quote_single,
                .quote_double,
                => |quote_tt| Tokenizer.QuoteType.fromTokenType(Tokenizer.TokenType.fromNarrow(quote_tt)).?,
            };

            const content: SystemLiteralContent = switch (try parse_helper.nextTokenType(tokenizer.asTokenizer(), quote_type.systemLiteralCtx(), MaybeReader, scanner.mbr())) {
                else => unreachable,
                .eof => return ScanError.UnexpectedEof,

                .text_data,
                => .text,

                .quote_single,
                .quote_double,
                => .empty,
            };

            return .{ quote_type, content };
        }

        fn expectAndSkipTagWhitespace(scanner: *const Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!void {
            if (try parse_helper.skipIfTagWhitespaceOrGetNextTokType(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr())) |tt| switch (tt) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,
                .tag_whitespace => unreachable,
            };
        }

        inline fn mbr(scanner: *const Self) parse_helper.MaybeBufferedReader(MaybeReader) {
            if (MaybeReader == null) return .{};
            return .{
                .reader = scanner.reader,
                .read_buffer = scanner.read_buffer,
            };
        }

        const State = enum {
            start,
            end,
            err,

            name,
            value_kind,

            entity_value_kind_sq,
            entity_value_kind_dq,

            entity_value_str_sq,
            entity_value_str_dq,

            entity_value_ref_sq,
            entity_value_ref_dq,

            pub_literal_empty,
            pub_literal_sq,
            pub_literal_dq,

            sys_literal_empty,
            sys_literal_sq,
            sys_literal_dq,

            check_for_ndata_decl,
            ndata_decl_name,
        };
    };
}

const ScannerTestValues = struct {
    kind: Kind,
    name: []const u8,
    definition: Definition,

    const Definition = union(DefinitionKind) {
        entity_value: []const struct { ValueStrKind, []const u8 },
        /// Pubid Literal, System Literal, NDataDecl?
        external_id_public: struct { []const u8, []const u8, ?[]const u8 },
        /// System Literal, NDataDecl?
        external_id_system: struct { []const u8, ?[]const u8 },

        fn deinit(def: Definition, allocator: std.mem.Allocator) void {
            switch (def) {
                .entity_value => |entity_value| {
                    for (entity_value) |part| allocator.free(part[1]);
                    allocator.free(entity_value);
                },
                .external_id_public => |extid_pub| {
                    allocator.free(extid_pub[0]);
                    allocator.free(extid_pub[1]);
                    allocator.free(extid_pub[2] orelse "");
                },
                .external_id_system => |extid_sys| {
                    allocator.free(extid_sys[0]);
                    allocator.free(extid_sys[1] orelse "");
                },
            }
        }

        pub fn format(
            definition: Definition,
            comptime fmt_str: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = fmt_str;
            _ = options;
            switch (definition) {
                .entity_value => |entity_value| {
                    const quote_type: Tokenizer.QuoteType = for (entity_value) |value_segment| {
                        if (value_segment[0] != .text) continue;
                        if (std.mem.indexOfScalar(u8, value_segment[1], '"') != null) break .single;
                    } else .double;

                    try writer.writeByte(quote_type.toChar());

                    for (entity_value) |value_segment| {
                        const kind, const str = value_segment;
                        switch (kind) {
                            .text => {},
                            .ge_ref => try writer.writeByte('&'),
                            .pe_ref => try writer.writeByte('%'),
                            .char_ref => try writer.writeAll("&#"),
                        }
                        try writer.print("{}", .{std.zig.fmtEscapes(str)});
                        if (kind != .text) try writer.writeByte(';');
                    }

                    try writer.writeByte(quote_type.toChar());
                },

                .external_id_public => |extid_pub| {
                    const pubid, const sys, const maybe_ndata = extid_pub;

                    const pubid_qt: Tokenizer.QuoteType = if (std.mem.indexOfScalar(u8, pubid, '"') != null) .single else .double;
                    const sys_qt: Tokenizer.QuoteType = if (std.mem.indexOfScalar(u8, sys, '"') != null) .single else .double;
                    try writer.print("PUBLIC {0c}{1}{0c} {2c}{3}{2c}", .{
                        pubid_qt.toChar(), std.zig.fmtEscapes(pubid),
                        sys_qt.toChar(),   std.zig.fmtEscapes(sys),
                    });

                    if (maybe_ndata) |ndata| {
                        try writer.print(" NDATA {}", .{std.zig.fmtEscapes(ndata)});
                    }
                },

                .external_id_system => |extid_sys| {
                    const sys, const maybe_ndata = extid_sys;

                    const sys_qt: Tokenizer.QuoteType = if (std.mem.indexOfScalar(u8, sys, '"') != null) .single else .double;
                    try writer.print("SYSTEM {0c}{1}{0c}", .{ sys_qt.toChar(), std.zig.fmtEscapes(sys) });

                    if (maybe_ndata) |ndata| {
                        try writer.print(" NDATA {}", .{std.zig.fmtEscapes(ndata)});
                    }
                },
            }
        }

        fn expectEqual(expected: Definition, actual: Definition) !void {
            try std.testing.expectEqual(std.meta.activeTag(expected), std.meta.activeTag(actual));
            switch (expected) {
                .entity_value => |entity_value| {
                    try std.testing.expectEqual(entity_value.len, actual.entity_value.len);
                    for (entity_value, actual.entity_value, 0..) |expected_segment, actual_segment, i| {
                        errdefer std.log.err("Difference occurred in entity value at index {d}", .{i});
                        try std.testing.expectEqual(expected_segment[0], actual_segment[0]);
                        try std.testing.expectEqualStrings(expected_segment[1], actual_segment[1]);
                    }
                },
                .external_id_public => |external_id_public| {
                    try std.testing.expectEqualStrings(external_id_public[0], actual.external_id_public[0]);
                    try std.testing.expectEqualStrings(external_id_public[1], actual.external_id_public[1]);
                    try test_helper.expectEqualStringOrErrOrNull(external_id_public[2], actual.external_id_public[2]);
                },
                .external_id_system => |external_id_system| {
                    try std.testing.expectEqualStrings(external_id_system[0], actual.external_id_system[0]);
                    try test_helper.expectEqualStringOrErrOrNull(external_id_system[1], actual.external_id_system[1]);
                },
            }
        }
    };

    fn deinit(stv: ScannerTestValues, allocator: std.mem.Allocator) void {
        allocator.free(stv.name);
        stv.definition.deinit(allocator);
    }

    pub fn format(
        stv: ScannerTestValues,
        comptime fmt_str: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt_str;
        _ = options;
        try writer.print("<!ENTITY{s}{} {}>", .{
            if (stv.kind != .parameter) " " else " % ",
            std.zig.fmtEscapes(stv.name),
            stv.definition,
        });
    }

    fn expectEqual(expected: ScannerTestValues, actual: ScannerTestValues) !void {
        errdefer std.log.err("\nExpected:\n{}\nGot:\n{}", .{ expected, actual });
        try std.testing.expectEqual(expected.kind, actual.kind);
        try std.testing.expectEqualStrings(expected.name, actual.name);
        try expected.definition.expectEqual(actual.definition);
    }

    fn parse(
        allocator: std.mem.Allocator,
        comptime MaybeReader: ?type,
        tokenizer: *Scanner(MaybeReader).TokenizerAPI,
        mbr: parse_helper.MaybeBufferedReader(MaybeReader),
    ) !ScannerTestValues {
        const src = if (MaybeReader == null) tokenizer.asTokenizer().src;
        var scanner = if (MaybeReader != null) streamScanner(mbr.reader, mbr.read_buffer) else fullScanner();

        const kind = try scanner.entityKind(tokenizer);

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

        const def_kind = try scanner.definitionKind(tokenizer);

        const definition: Definition = switch (def_kind) {
            .entity_value => def: {
                var values = std.ArrayList(struct { ValueStrKind, []const u8 }).init(allocator);
                defer {
                    for (values.items) |value| allocator.free(value[1]);
                    values.deinit();
                }

                while (try scanner.entityValueNextStrKind(tokenizer)) |str_kind| {
                    var value_str = std.ArrayList(u8).init(allocator);
                    defer value_str.deinit();

                    while (try scanner.entityValueNextStrSegment(tokenizer)) |segment| {
                        const segment_str = if (MaybeReader != null) segment else segment.toStr(src);
                        try value_str.appendSlice(segment_str);
                    }

                    try values.ensureUnusedCapacity(1);
                    values.appendAssumeCapacity(.{ str_kind, try value_str.toOwnedSlice() });
                }

                break :def .{ .entity_value = try values.toOwnedSlice() };
            },

            inline //
            .external_id_public,
            .external_id_system,
            => |tag| def: {
                const PubidLiteral = switch (tag) {
                    .external_id_public => []const u8,
                    .external_id_system => void,
                    else => comptime unreachable,
                };
                const pubid_literal: PubidLiteral = switch (tag) {
                    .external_id_public => str: {
                        var pubid_literal = std.ArrayList(u8).init(allocator);
                        defer pubid_literal.deinit();

                        while (try scanner.pubidLiteralNextSegment(tokenizer)) |segment| {
                            const segment_str = if (MaybeReader != null) segment else segment.toStr(src);
                            try pubid_literal.appendSlice(segment_str);
                        }

                        break :str try pubid_literal.toOwnedSlice();
                    },
                    .external_id_system => {},
                    else => comptime unreachable,
                };
                errdefer switch (tag) {
                    .external_id_public => allocator.free(pubid_literal),
                    .external_id_system => {},
                    else => comptime unreachable,
                };

                const system_literal: []const u8 = str: {
                    var system_literal = std.ArrayList(u8).init(allocator);
                    defer system_literal.deinit();

                    while (try scanner.systemLiteralNextSegment(tokenizer)) |segment| {
                        const segment_str = if (MaybeReader != null) segment else segment.toStr(src);
                        try system_literal.appendSlice(segment_str);
                    }

                    break :str try system_literal.toOwnedSlice();
                };
                errdefer allocator.free(system_literal);

                const ndata_decl: ?[]const u8 = if (try scanner.nDataDeclPresent(tokenizer)) str: {
                    var ndata_decl = std.ArrayList(u8).init(allocator);
                    defer ndata_decl.deinit();

                    while (try scanner.nDataDeclNameNextSegment(tokenizer)) |segment| {
                        const segment_str = if (MaybeReader != null) segment else segment.toStr(src);
                        try ndata_decl.appendSlice(segment_str);
                    }

                    break :str try ndata_decl.toOwnedSlice();
                } else null;
                errdefer allocator.free(ndata_decl orelse "");

                break :def switch (tag) {
                    .external_id_public => .{ .external_id_public = .{ pubid_literal, system_literal, ndata_decl } },
                    .external_id_system => .{ .external_id_system = .{ system_literal, ndata_decl } },
                    else => comptime unreachable,
                };
            },
        };
        errdefer definition.deinit(allocator);

        return .{
            .kind = kind,
            .name = name,
            .definition = definition,
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

            try expectEntityDeclStart(Reader, &tokenizer.stream, mbr);
            const actual = try ScannerTestValues.parse(std.testing.allocator, Reader, &tokenizer.stream, mbr);
            defer actual.deinit(std.testing.allocator);
            try expected.expectEqual(actual);
        }
    }

    var tokenizer = Tokenizer.initFull(src);

    try expectEntityDeclStart(null, &tokenizer.full, .{});
    const actual = try ScannerTestValues.parse(std.testing.allocator, null, &tokenizer.full, .{});
    defer actual.deinit(std.testing.allocator);
    try expected.expectEqual(actual);
}

fn expectEntityDeclStart(
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
    try std.testing.expectEqual(.entity, iksemel.dtd.MarkupDeclKind.fromString(bstr.constSlice()));
}

test "EntityValue" {
    const buf_sizes = [_]usize{
        1,  2,  3,  4,  5,  6,  7,  8,  10,  12,  14,  16,  20,  24,
        28, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256,
    };

    try testScanner(&buf_sizes, "<!ENTITY foo \"bar\">", .{
        .kind = .general,
        .name = "foo",
        .definition = .{ .entity_value = &.{
            .{ .text, "bar" },
        } },
    });
    try testScanner(&buf_sizes, "<!ENTITY % foo \"bar\">", .{
        .kind = .parameter,
        .name = "foo",
        .definition = .{ .entity_value = &.{
            .{ .text, "bar" },
        } },
    });

    try testScanner(&buf_sizes, "<!ENTITY foo 'bar&fizz;baz'>", .{
        .kind = .general,
        .name = "foo",
        .definition = .{ .entity_value = &.{
            .{ .text, "bar" },
            .{ .ge_ref, "fizz" },
            .{ .text, "baz" },
        } },
    });
    try testScanner(&buf_sizes, "<!ENTITY % foo 'bar&fizz;baz'>", .{
        .kind = .parameter,
        .name = "foo",
        .definition = .{ .entity_value = &.{
            .{ .text, "bar" },
            .{ .ge_ref, "fizz" },
            .{ .text, "baz" },
        } },
    });

    try testScanner(&buf_sizes, "<!ENTITY foo '&#34;bar&#xA1;'>", .{
        .kind = .general,
        .name = "foo",
        .definition = .{ .entity_value = &.{
            .{ .char_ref, "34" },
            .{ .text, "bar" },
            .{ .char_ref, "xA1" },
        } },
    });
    try testScanner(&buf_sizes, "<!ENTITY % foo '&#34;bar&#xA1;'>", .{
        .kind = .parameter,
        .name = "foo",
        .definition = .{ .entity_value = &.{
            .{ .char_ref, "34" },
            .{ .text, "bar" },
            .{ .char_ref, "xA1" },
        } },
    });

    try testScanner(&buf_sizes, "<!ENTITY foo '%bar;'>", .{
        .kind = .general,
        .name = "foo",
        .definition = .{ .entity_value = &.{
            .{ .pe_ref, "bar" },
        } },
    });
    try testScanner(&buf_sizes, "<!ENTITY % foo '%bar;'>", .{
        .kind = .parameter,
        .name = "foo",
        .definition = .{ .entity_value = &.{
            .{ .pe_ref, "bar" },
        } },
    });
}

test "External Id" {
    const buf_sizes = [_]usize{
        1,  2,  3,  4,  5,  6,  7,  8,  10,  12,  14,  16,  20,  24,
        28, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256,
    };

    try testScanner(&buf_sizes, "<!ENTITY foo SYSTEM 'bar'>", .{
        .kind = .general,
        .name = "foo",
        .definition = .{ .external_id_system = .{ "bar", null } },
    });
    try testScanner(&buf_sizes, "<!ENTITY % foo SYSTEM 'bar'>", .{
        .kind = .parameter,
        .name = "foo",
        .definition = .{ .external_id_system = .{ "bar", null } },
    });

    try testScanner(&buf_sizes, "<!ENTITY foo SYSTEM 'bar' NDATA baz>", .{
        .kind = .general,
        .name = "foo",
        .definition = .{ .external_id_system = .{ "bar", "baz" } },
    });
    // this is invalid, but it's left to be handled by the programmer
    try testScanner(&buf_sizes, "<!ENTITY % foo SYSTEM 'bar' NDATA baz>", .{
        .kind = .parameter,
        .name = "foo",
        .definition = .{ .external_id_system = .{ "bar", "baz" } },
    });

    try testScanner(&buf_sizes, "<!ENTITY foo PUBLIC 'bar' 'baz'>", .{
        .kind = .general,
        .name = "foo",
        .definition = .{ .external_id_public = .{ "bar", "baz", null } },
    });
    try testScanner(&buf_sizes, "<!ENTITY % foo PUBLIC 'bar' 'baz'>", .{
        .kind = .parameter,
        .name = "foo",
        .definition = .{ .external_id_public = .{ "bar", "baz", null } },
    });

    try testScanner(&buf_sizes, "<!ENTITY foo PUBLIC 'bar' 'baz' NDATA fizz>", .{
        .kind = .general,
        .name = "foo",
        .definition = .{ .external_id_public = .{ "bar", "baz", "fizz" } },
    });
    // this is invalid, but it's left to be handled by the programmer
    try testScanner(&buf_sizes, "<!ENTITY % foo PUBLIC 'bar' 'baz' NDATA fizz>", .{
        .kind = .parameter,
        .name = "foo",
        .definition = .{ .external_id_public = .{ "bar", "baz", "fizz" } },
    });
}

const std = @import("std");
const assert = std.debug.assert;

const iksemel = @import("../iksemel.zig");
const Tokenizer = iksemel.Tokenizer;

const parse_helper = @import("../parse_helper.zig");
const test_helper = @import("../test_helper.zig");
