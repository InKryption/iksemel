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

pub const AttributeType = enum {
    cdata,
    id,
    idref,
    idrefs,
    entity,
    entities,
    nmtoken,
    nmtokens,

    notation,
    enumeration,
};

pub const DefaultDeclKind = enum {
    /// `'#REQUIRED'`
    required,
    /// `'#IMPLIED'`
    implied,
    /// `'#FIXED' S AttValue`
    fixed,
    /// `AttValue`
    value,
};

pub const AttributeValueStrKind = enum {
    /// `'&' Name ';'`
    entity_ref,
    /// `'&#' Digits ';'`
    character_ref,
    /// Run of textual data.
    text,
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
        /// by a `.tag_token`, whose source satisfies `dtd.MarkupDeclKind.fromString(source) == .attlist`.
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
        /// See `pendingAttDef` next.
        pub fn nameNextSegment(scanner: *Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!?Src {
            assert(scanner.state == .name);
            errdefer scanner.state = .err;

            if (try parse_helper.nextTokenSegment(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) |segment| {
                return segment;
            }
            scanner.state = .check_for_end_or_attdef;
            return null;
        }

        /// This must be called after `scanner.nameNextSegment(tokenizer) = null`,
        /// after `scanner.defaultDeclKind(tokenizer) = .required`,
        /// after `scanner.defaultDeclKind(tokenizer) = .implied`, or
        /// after `scanner.defaultDeclValueNextStrKind(tokenizer) = null`.
        /// Returns true if there is a(nother) attribute definition, and
        /// returns false if we have reached the end of the attribute list
        /// declaration.
        /// See `attributeNameNextSegment` next.
        pub fn pendingAttDef(scanner: *Self, tokenizer: *TokenizerAPI) !bool {
            assert(scanner.state == .check_for_end_or_attdef);
            defer assert(scanner.state != .check_for_end_or_attdef);
            errdefer scanner.state = .err;

            const first_non_whitespace_tt: enum { tag_token, angle_bracket_right } = blk: {
                switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) {
                    else => return ScanError.UnexpectedToken,
                    .eof => return ScanError.UnexpectedEof,

                    .angle_bracket_right => break :blk .angle_bracket_right,
                    .tag_whitespace => {},
                }
                try parse_helper.skipWhitespaceSrcUnchecked(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr);

                break :blk switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) {
                    else => return ScanError.UnexpectedToken,
                    .eof => return ScanError.UnexpectedEof,
                    .tag_whitespace => unreachable,

                    .angle_bracket_right => .angle_bracket_right,
                    .tag_token => .tag_token,
                };
            };

            switch (first_non_whitespace_tt) {
                .angle_bracket_right => {
                    scanner.state = .end;
                    return false;
                },
                .tag_token => {
                    scanner.state = .attdef_name;
                    return true;
                },
            }
        }

        /// This must be called after `scanner.pendingAttDef(tokenizer) = true`, and
        /// then until it returns null.
        /// The returned source segments should be concatenated in order to obtain
        /// the declared attribute's name.
        /// See `attributeType` next.
        pub fn attributeNameNextSegment(scanner: *Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!?Src {
            assert(scanner.state == .attdef_name);
            errdefer scanner.state = .err;

            if (try parse_helper.nextTokenSegment(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) |segment| {
                return segment;
            }
            scanner.state = .attdef_type;
            return null;
        }

        /// This must be called after `scanner.attributeNameNextSegment(tokenizer) = null`.
        /// See `enumeratedNameNextSegment`, and `defaultDeclKind` next.
        pub fn attributeType(scanner: *Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!AttributeType {
            try scanner.expectAndSkipTagWhitespace(tokenizer.asTokenizer());

            const attr_type: AttributeType = switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,
                .tag_whitespace => unreachable,

                .tag_token => blk: {
                    const TypeKw = enum {
                        CDATA,
                        ID,
                        IDREF,
                        IDREFS,
                        ENTITY,
                        ENTITIES,
                        NMTOKEN,
                        NMTOKENS,

                        NOTATION,
                    };
                    const maybe_type_kw = try parse_helper.nextTokenSrcAsEnum(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr, TypeKw);
                    const type_kw = maybe_type_kw orelse return ScanError.UnexpectedToken;
                    break :blk switch (type_kw) {
                        inline else => |tag| comptime attr_type: {
                            var lower_buf = @tagName(tag)[0..].*;
                            const lower = std.ascii.lowerString(&lower_buf, @tagName(tag));
                            break :attr_type @field(AttributeType, lower);
                        },
                    };
                },
                .lparen => .enumeration,
            };

            if (attr_type == .notation) {
                try scanner.expectAndSkipTagWhitespace(tokenizer.asTokenizer());
                switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) {
                    else => return ScanError.UnexpectedToken,
                    .eof => return ScanError.UnexpectedEof,
                    .tag_whitespace => unreachable,
                    .lparen => {},
                }
            }

            switch (attr_type) {
                .notation, .enumeration => switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) {
                    else => return ScanError.UnexpectedToken,
                    .eof => return ScanError.UnexpectedEof,
                    .tag_whitespace => unreachable,
                    .tag_token => {},
                },
                else => {},
            }

            scanner.state = switch (attr_type) {
                .notation, .enumeration => .attdef_enumerated,
                else => .default_decl,
            };
            return attr_type;
        }

        /// This must be called after `scanner.attributeNameNextSegment(tokenizer) = .notation`, or
        /// after `scanner.attributeNameNextSegment(tokenizer) = .enumeration`, and then until it
        /// returns null.
        /// The first return value will be non-null; it will then be optionally followed by a series
        /// of source segments which should be concatenated in order to obtain the enumerated name/nmtoken.
        /// After obtaining one name, it should be called again: if it returns null immediately,
        /// the list of names has been terminated, otherwise another name/nmtoken is being returned.
        /// See `defaultDeclKind` next.
        pub fn enumeratedNameNextSegment(scanner: *Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!?Src {
            switch (scanner.state) {
                .attdef_enumerated => {},
                .default_decl => return null,
                else => unreachable,
            }
            assert(scanner.state == .attdef_enumerated);
            errdefer scanner.state = .err;

            if (try parse_helper.nextTokenSegment(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) |segment| {
                return segment;
            }

            switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,
                .tag_whitespace => unreachable,
                .pipe => switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) {
                    else => return ScanError.UnexpectedToken,
                    .eof => return ScanError.UnexpectedEof,
                    .tag_whitespace => unreachable,
                    .tag_token => {},
                },
                .rparen => scanner.state = .default_decl,
            }

            return null;
        }

        /// This must be called after `scanner.enumeratedNameNextSegment(tokenizer) = null` two times consecutively.
        /// See `defaultDeclValueNextStrKind`, and `pendingAttDef` next.
        pub fn defaultDeclKind(scanner: *Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!DefaultDeclKind {
            assert(scanner.state == .default_decl);
            try scanner.expectAndSkipTagWhitespace(tokenizer.asTokenizer());

            const dd_kind: union(DefaultDeclKind) {
                required,
                implied,
                fixed: xml.prod.QuoteType,
                value: xml.prod.QuoteType,
            } = switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,

                .hashtag => dd: {
                    switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr)) {
                        else => return ScanError.UnexpectedToken,
                        .eof => return ScanError.UnexpectedEof,
                        .tag_token => {},
                    }
                    const maybe_dd_kw = try parse_helper.nextTokenSrcAsEnum(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr, enum { REQUIRED, IMPLIED, FIXED });
                    const dd_kw = maybe_dd_kw orelse return ScanError.UnexpectedToken;

                    break :dd switch (dd_kw) {
                        .REQUIRED => .required,
                        .IMPLIED => .implied,
                        .FIXED => .{ .fixed = fixed_quote_type: {
                            try scanner.expectAndSkipTagWhitespace(tokenizer.asTokenizer());
                            const possibly_quote_tt = try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .markup, MaybeReader, scanner.mbr);
                            const maybe_quote_type = xml.prod.QuoteType.fromTokenTypeNarrow(possibly_quote_tt);
                            break :fixed_quote_type maybe_quote_type orelse return ScanError.UnexpectedToken;
                        } },
                    };
                },

                inline //
                .quote_single,
                .quote_double,
                => |quote_tt| .{ .value = comptime xml.prod.QuoteType.fromTokenTypeNarrow(quote_tt).? },
            };

            scanner.state = switch (dd_kind) {
                .required, .implied => .check_for_end_or_attdef,
                .fixed, .value => |quote_tt| switch (quote_tt) {
                    .single => .default_decl_value_sq,
                    .double => .default_decl_value_dq,
                },
            };
            return dd_kind;
        }

        /// This must be called after `scanner.defaultDeclKind(tokenizer) = .fixed` or
        /// after `scanner.defaultDeclKind(tokenizer) = .value`.
        /// Returns null to indicate the attribute value string has been terminated.
        /// See `defaultDeclValueNextStrSegment`, and `pendingAttDef` next.
        pub fn defaultDeclValueNextStrKind(scanner: *Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!?AttributeValueStrKind {
            const quote_type: xml.prod.QuoteType = switch (scanner.state) {
                .default_decl_value_sq => .single,
                .default_decl_value_dq => .double,
                else => unreachable,
            };
            defer assert(switch (quote_type) {
                .single => scanner.state != .default_decl_value_sq,
                .double => scanner.state != .default_decl_value_dq,
            });
            errdefer scanner.state = .err;

            switch (try parse_helper.nextTokenType(tokenizer.asTokenizer(), quote_type.attributeValueCtx(), MaybeReader, scanner.mbr)) {
                else => unreachable,
                .eof => return ScanError.UnexpectedEof,
                .angle_bracket_left => return ScanError.UnexpectedToken,

                .quote_single, .quote_double => {
                    scanner.state = .check_for_end_or_attdef;
                    return null;
                },

                .text_data => {
                    scanner.state = switch (quote_type) {
                        .single => .default_decl_value_str_sq,
                        .double => .default_decl_value_str_dq,
                    };
                    return .text;
                },
                .ampersand => {
                    const kind: AttributeValueStrKind = switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .reference, MaybeReader, scanner.mbr)) {
                        .eof => return ScanError.UnexpectedEof,
                        .semicolon => return ScanError.UnexpectedToken,
                        .invalid_reference_end => return ScanError.UnexpectedToken,

                        .hashtag => switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .reference, MaybeReader, scanner.mbr)) {
                            .eof => return ScanError.UnexpectedEof,
                            .semicolon => return ScanError.UnexpectedToken,
                            .invalid_reference_end => return ScanError.UnexpectedToken,
                            .hashtag => return ScanError.UnexpectedToken,

                            .tag_token => .character_ref,
                        },
                        .tag_token => .entity_ref,
                    };
                    scanner.state = switch (quote_type) {
                        .single => .default_decl_value_ref_sq,
                        .double => .default_decl_value_ref_dq,
                    };
                    return kind;
                },
            }
        }

        pub fn defaultDeclValueNextStrSegment(scanner: *Self, tokenizer: *TokenizerAPI) (SrcError || ScanError)!?Src {
            const quote_type: xml.prod.QuoteType, const is_ref: bool = switch (scanner.state) {
                .default_decl_value_str_sq => .{ .single, false },
                .default_decl_value_str_dq => .{ .double, false },

                .default_decl_value_ref_sq => .{ .single, true },
                .default_decl_value_ref_dq => .{ .double, true },

                else => unreachable,
            };
            errdefer scanner.state = .err;

            const str_or_ref_ctx: Tokenizer.Context = if (is_ref) .reference else quote_type.attributeValueCtx();
            if (try parse_helper.nextTokenSegment(tokenizer.asTokenizer(), str_or_ref_ctx, MaybeReader, scanner.mbr)) |segment| {
                return segment;
            }

            if (is_ref) switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .reference, MaybeReader, scanner.mbr)) {
                .eof => return ScanError.UnexpectedEof,
                .hashtag => return ScanError.UnexpectedToken,
                .invalid_reference_end => return ScanError.UnexpectedToken,
                .tag_token => unreachable,
                .semicolon => {},
            };

            scanner.state = switch (quote_type) {
                .single => .default_decl_value_sq,
                .double => .default_decl_value_dq,
            };
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
            check_for_end_or_attdef,
            attdef_name,
            attdef_type,
            attdef_enumerated,
            default_decl,

            default_decl_value_sq,
            default_decl_value_dq,

            default_decl_value_str_sq,
            default_decl_value_str_dq,

            default_decl_value_ref_sq,
            default_decl_value_ref_dq,
        };
    };
}

const ScannerTestValues = struct {
    name: []const u8,
    defs: []const AttDef.Tuple,

    const AttDef = struct {
        name: []const u8,
        type: Type,
        default_decl: DefaultDecl,

        const Tuple = struct { []const u8, Type, DefaultDecl };
        inline fn destructure(att_def: AttDef) Tuple {
            return .{ att_def.name, att_def.type, att_def.default_decl };
        }
        inline fn init(tuple: Tuple) AttDef {
            const name, const attr_type, const default_decl = tuple;
            return .{
                .name = name,
                .type = attr_type,
                .default_decl = default_decl,
            };
        }

        const Type = union(AttributeType) {
            cdata,
            id,
            idref,
            idrefs,
            entity,
            entities,
            nmtoken,
            nmtokens,

            notation: []const []const u8,
            enumeration: []const []const u8,

            fn deinit(att_type: Type, allocator: std.mem.Allocator) void {
                const strings = switch (att_type) {
                    .notation, .enumeration => |strings| strings,
                    inline else => |empty| {
                        empty;
                        return;
                    },
                };
                for (strings) |string| allocator.free(string);
                allocator.free(strings);
            }

            fn expectEqual(expected: Type, actual: Type) !void {
                try std.testing.expectEqual(std.meta.activeTag(expected), std.meta.activeTag(actual));
                switch (expected) {
                    inline else => |empty| empty,
                    inline //
                    .notation,
                    .enumeration,
                    => |expected_strings, tag| {
                        const actual_strings = @field(actual, @tagName(tag));
                        const common_len = @min(expected_strings.len, actual_strings.len);
                        for (expected_strings[0..common_len], actual_strings[0..common_len], 0..common_len) |expected_str, actual_str, i| {
                            errdefer std.log.err("Difference occurred on string {d}", .{i});
                            try std.testing.expectEqualStrings(expected_str, actual_str);
                        }
                        try std.testing.expectEqual(expected_strings.len, actual_strings.len);
                    },
                }
            }
        };

        const DefaultDecl = union(DefaultDeclKind) {
            required,
            implied,
            fixed: []const struct { AttributeValueStrKind, []const u8 },
            value: []const struct { AttributeValueStrKind, []const u8 },

            fn deinit(dd: DefaultDecl, allocator: std.mem.Allocator) void {
                const parts = switch (dd) {
                    .required, .implied => |empty| {
                        empty;
                        return;
                    },
                    .fixed, .value => |parts| parts,
                };
                for (parts) |part| {
                    const kind, const str = part;
                    _ = kind;
                    allocator.free(str);
                }
                allocator.free(parts);
            }

            fn expectEqual(expected: DefaultDecl, actual: DefaultDecl) !void {
                try std.testing.expectEqual(std.meta.activeTag(expected), std.meta.activeTag(actual));
                switch (expected) {
                    inline else => |empty| empty,
                    inline //
                    .fixed,
                    .value,
                    => |expected_parts, tag| {
                        const actual_parts = @field(actual, @tagName(tag));
                        const common_len = @min(expected_parts.len, actual_parts.len);
                        for (expected_parts[0..common_len], actual_parts[0..common_len], 0..common_len) |expected_part, actual_part, i| {
                            errdefer std.log.err("Difference occurred on string {d}", .{i});
                            try std.testing.expectEqual(expected_part[0], actual_part[0]);
                            try std.testing.expectEqualStrings(expected_part[1], actual_part[1]);
                        }
                        try std.testing.expectEqual(expected_parts.len, actual_parts.len);
                    },
                }
            }
        };

        fn deinit(def: AttDef, allocator: std.mem.Allocator) void {
            allocator.free(def.name);
            def.type.deinit(allocator);
            def.default_decl.deinit(allocator);
        }

        fn expectEqual(expected: AttDef, actual: AttDef) !void {
            try std.testing.expectEqualStrings(expected.name, actual.name);
            try expected.type.expectEqual(actual.type);
            try expected.default_decl.expectEqual(actual.default_decl);
        }
    };

    fn deinit(stv: ScannerTestValues, allocator: std.mem.Allocator) void {
        allocator.free(stv.name);
        for (stv.defs) |def| AttDef.init(def).deinit(allocator);
        allocator.free(stv.defs);
    }

    fn expectEqual(expected: ScannerTestValues, actual: ScannerTestValues) !void {
        try std.testing.expectEqualStrings(expected.name, actual.name);

        const defs_common_len = @min(expected.defs.len, actual.defs.len);
        for (expected.defs[0..defs_common_len], actual.defs[0..defs_common_len], 0..defs_common_len) |expected_def_tup, actual_def_tup, i| {
            errdefer std.log.err("Difference occurred on attribute {d}", .{i});
            const expected_def = AttDef.init(expected_def_tup);
            const actual_def = AttDef.init(actual_def_tup);
            try expected_def.expectEqual(actual_def);
        }
        try std.testing.expectEqual(expected.defs.len, actual.defs.len);
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

        var defs = std.ArrayList(AttDef.Tuple).init(allocator);
        defer {
            for (defs.items) |def| AttDef.init(def).deinit(allocator);
            defs.deinit();
        }

        while (try scanner.pendingAttDef(tokenizer)) {
            const attr_name: []const u8 = blk: {
                var attr_name = std.ArrayList(u8).init(allocator);
                defer attr_name.deinit();

                while (try scanner.attributeNameNextSegment(tokenizer)) |segment| {
                    const segment_str = if (MaybeReader != null) segment else segment.toStr(src);
                    try attr_name.appendSlice(segment_str);
                }

                break :blk try attr_name.toOwnedSlice();
            };
            errdefer allocator.free(attr_name);

            const attr_type_tag = try scanner.attributeType(tokenizer);
            const attr_type: AttDef.Type = switch (attr_type_tag) {
                inline //
                .cdata,
                .id,
                .idref,
                .idrefs,
                .entity,
                .entities,
                .nmtoken,
                .nmtokens,
                => |tag| @unionInit(AttDef.Type, @tagName(tag), {}),

                inline //
                .notation,
                .enumeration,
                => |tag| @unionInit(AttDef.Type, @tagName(tag), enumerated: {
                    var enumerated_names = std.ArrayList([]const u8).init(allocator);
                    defer enumerated_names.deinit();

                    while (true) {
                        var enumerated_name = std.ArrayList(u8).init(allocator);
                        defer enumerated_name.deinit();

                        try enumerated_name.appendSlice(blk: {
                            const first_segment = (try scanner.enumeratedNameNextSegment(tokenizer)) orelse break;
                            break :blk if (MaybeReader != null) first_segment else first_segment.toStr(src);
                        });

                        while (try scanner.enumeratedNameNextSegment(tokenizer)) |segment| {
                            const segment_str = if (MaybeReader != null) segment else segment.toStr(src);
                            try enumerated_name.appendSlice(segment_str);
                        }

                        try enumerated_names.ensureUnusedCapacity(1);
                        enumerated_names.appendAssumeCapacity(try enumerated_name.toOwnedSlice());
                    }

                    break :enumerated try enumerated_names.toOwnedSlice();
                }),
            };
            errdefer attr_type.deinit(allocator);

            const default_decl_kind = try scanner.defaultDeclKind(tokenizer);
            const default_decl: AttDef.DefaultDecl = switch (default_decl_kind) {
                inline //
                .required,
                .implied,
                => |tag| @unionInit(AttDef.DefaultDecl, @tagName(tag), {}),
                inline //
                .fixed,
                .value,
                => |tag| @unionInit(AttDef.DefaultDecl, @tagName(tag), att_val: {
                    var attribute_val = std.ArrayList(struct { AttributeValueStrKind, []const u8 }).init(allocator);
                    defer attribute_val.deinit();

                    while (try scanner.defaultDeclValueNextStrKind(tokenizer)) |kind| {
                        const attr_str: []const u8 = blk: {
                            var attr_str = std.ArrayList(u8).init(allocator);
                            defer attr_str.deinit();

                            while (try scanner.defaultDeclValueNextStrSegment(tokenizer)) |segment| {
                                const segment_str = if (MaybeReader != null) segment else segment.toStr(src);
                                try attr_str.appendSlice(segment_str);
                            }

                            break :blk try attr_str.toOwnedSlice();
                        };
                        errdefer allocator.free(attr_str);

                        try attribute_val.append(.{ kind, attr_str });
                    }

                    break :att_val try attribute_val.toOwnedSlice();
                }),
            };
            errdefer default_decl.deinit(allocator);

            try defs.append(AttDef.destructure(.{
                .name = attr_name,
                .type = attr_type,
                .default_decl = default_decl,
            }));
        }

        return .{
            .name = name,
            .defs = try defs.toOwnedSlice(),
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

            try expectAttlistDeclStart(Reader, &tokenizer.stream, mbr);
            const actual = try ScannerTestValues.parse(std.testing.allocator, Reader, &tokenizer.stream, mbr);
            defer actual.deinit(std.testing.allocator);
            try expected.expectEqual(actual);
        }
    }

    var tokenizer = Tokenizer.initFull(src);

    try expectAttlistDeclStart(null, &tokenizer.full, .{});
    const actual = try ScannerTestValues.parse(std.testing.allocator, null, &tokenizer.full, .{});
    defer actual.deinit(std.testing.allocator);
    try expected.expectEqual(actual);
}

fn expectAttlistDeclStart(
    comptime MaybeReader: ?type,
    tokenizer: *Scanner(MaybeReader).TokenizerAPI,
    mbr: parse_helper.MaybeBufferedReader(MaybeReader),
) !void {
    const src = if (MaybeReader == null) tokenizer.asTokenizer().src;
    try std.testing.expectEqual(.angle_bracket_left_bang, try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer.asTokenizer(), .markup, MaybeReader, mbr));
    try std.testing.expectEqual(.tag_token, try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .markup, MaybeReader, mbr));
    var bstr: std.BoundedArray(u8, xml.dtd.MarkupDeclKind.max_str_len) = .{};
    while (try parse_helper.nextTokenSegment(tokenizer.asTokenizer(), .markup, MaybeReader, mbr)) |segment| {
        const segment_str: []const u8 = if (MaybeReader != null) segment else segment.toStr(src);
        bstr.appendSlice(segment_str) catch return error.TestExpectedEqual;
    }
    errdefer std.log.err("Actual: '{}'", .{std.zig.fmtEscapes(bstr.constSlice())});
    try std.testing.expectEqual(.attlist, xml.dtd.MarkupDeclKind.fromString(bstr.constSlice()));
}

test "basic" {
    const buf_sizes = [_]usize{
        1,  2,  3,  4,  5,  6,  7,  8,  10,  12,  14,  16,  20,  24,
        28, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256,
    };

    try testScanner(&buf_sizes, " <!ATTLIST abc   lorem NOTATION (x| y| z) #IMPLIED   ipsum (j|k |l) #FIXED '&quot;'   fizz ID #REQUIRED  >", .{
        .name = "abc",
        .defs = &[_]ScannerTestValues.AttDef.Tuple{
            .{ "lorem", .{ .notation = &.{ "x", "y", "z" } }, .implied },
            .{ "ipsum", .{ .enumeration = &.{ "j", "k", "l" } }, .{ .fixed = &.{.{ .entity_ref, "quot" }} } },
            .{ "fizz", .id, .required },
        },
    });

    // TODO: add these as test cases
    //  <!-- no attributes -->
    //  <!ATTLIST foo>
    //
    //  <!-- CDATA & ID examples - most of the other kinds work the exact same -->
    //  <!ATTLIST a b CDATA #REQUIRED>
    //  <!ATTLIST c d CDATA #IMPLIED>
    //  <!ATTLIST e f CDATA #FIXED "fizz&lt;buzz">
    //  <!ATTLIST g h CDATA "fizz&lt;buzz">
    //
    //  <!ATTLIST i j ID #REQUIRED>
    //  <!ATTLIST k l ID #IMPLIED>
    //  <!ATTLIST m n ID #FIXED "fizz&lt;buzz">
    //  <!ATTLIST o p ID "fizz&lt;buzz">
    //
    //  <!-- NOTATION & Enumeration - these work the same, just different validation rules for the listed strings (Name vs Nmtoken) -->
    //  <!ATTLIST q r NOTATION (foo | bar| baz ) #REQUIRED>
    //  <!ATTLIST s t (fizz| buzz) #IMPLIED>
    //  <!ATTLIST u v NOTATION (fizz) #IMPLIED>
    //  <!ATTLIST w x (buzz) #IMPLIED>

}

const std = @import("std");
const assert = std.debug.assert;

const xml = @import("../iksemel.zig");
const Tokenizer = xml.Tokenizer;

const parse_helper = @import("../parse_helper.zig");
const test_helper = @import("../test_helper.zig");
