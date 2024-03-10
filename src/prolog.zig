const std = @import("std");
const assert = std.debug.assert;

const iksemel = @import("iksemel.zig");
const Tokenizer = iksemel.Tokenizer;

pub const ExternalIdKind = enum { SYSTEM, PUBLIC };

pub const ReferenceKind = enum {
    /// Represents a General Entity reference ('&foo;').
    general,
    /// Represents a Parsed Entity reference ('&foo;').
    parsed,
};

pub const EmptyOrAny = enum {
    EMPTY,
    ANY,
};

pub const ContentParticleQuantity = enum {
    /// '?'
    none_or_one,
    /// '*'
    none_or_many,
    /// '+'
    one_or_many,
};

pub const AttributeType = enum {
    CDATA,

    ID,
    IDREF,
    IDREFS,
    ENTITY,
    ENTITIES,
    NMTOKEN,
    NMTOKENS,

    NOTATION,
    enumeration,
};

pub const DefaultDeclKind = enum {
    REQUIRED,
    IMPLIED,
    FIXED,
};

pub const StrBuffer = union(enum) {
    static: *std.ArrayListUnmanaged(u8),
    dyn: *std.ArrayList(u8),
};

pub fn ParseCtx(comptime Inner: type) type {
    return struct {
        inner: Inner,
        const Self = @This();

        inline fn checkSrcType(comptime optional: enum { is_optional, not_optional }, comptime T: type) ?[]const u8 {
            const ExpectedSlice: type, //
            const ExpectedRange: type //
            = switch (optional) {
                .is_optional => .{ ?[]const u8, ?Tokenizer.Range },
                .not_optional => .{ []const u8, Tokenizer.Range },
            };
            comptime return switch (T) {
                ExpectedSlice => null,
                ExpectedRange => null,
                else => "Expected " ++ @typeName(ExpectedSlice) ++ " or " ++ @typeName(ExpectedRange) ++ ", instead got " ++ @typeName(T),
            };
        }

        /// Feeds the data from a PI section.
        pub fn feedPI(
            ctx: Self,
            /// * `[]const u8`
            /// * `Tokenizer.Range`
            data: anytype,
        ) !void {
            comptime if (checkSrcType(.not_optional, @TypeOf(data))) |err| @compileError(err);
            return ctx.inner.feedPI(data);
        }

        /// Marks the start of the DTD, providing the DTD name.
        /// Optionally followed by a call to `feedDTDExternalId`,
        /// and then afterwards, possibly followed by a series
        /// of interspersed calls to:
        /// * `feedPI`.
        /// * `feedDTDEntityStart`, followed by calls described in its docs.
        /// * `feedDTDElementStart`, followed by calls described in its docs.
        /// * `feedDTDAttlistStart`, followed by calls described in its docs.
        /// * `feedDTDNotation`.
        pub fn feedDTDName(
            ctx: Self,
            /// * `[]const u8`
            /// * `Tokenizer.Range`
            name: anytype,
        ) !void {
            comptime if (checkSrcType(.not_optional, @TypeOf(name))) |err| @compileError(err);
            return ctx.inner.feedDTDName(name);
        }

        pub fn feedDTDExternalId(
            ctx: Self,
            kind: ExternalIdKind,
            /// * `?[]const u8`
            /// * `?Tokenizer.Range`
            pubid_lit: anytype,
            /// * `[]const u8`
            /// * `Tokenizer.Range`
            system_lit: anytype,
        ) !void {
            comptime if (checkSrcType(.is_optional, @TypeOf(pubid_lit))) |err| @compileError(err);
            comptime if (checkSrcType(.not_optional, @TypeOf(system_lit))) |err| @compileError(err);
            return ctx.inner.feedDTDExternalId(kind, pubid_lit, system_lit);
        }

        // --- DTD ENTITY ---

        /// Begins an Entity Declaration, providing the kind (parsed or general), and the
        /// entity name. The next call can be to:
        /// * `feedDTDEntityValueTextSegment` or `feedDTDEntityValueReference`:
        ///   if either of these are called, they may be called multiple times afterwards
        ///   to construct the entity value, terminated by a call to `feedDTDEntityValueEnd`.
        ///
        /// * `feedDTDEntityValueExternalId`:
        ///   the entity value is constructed by this single method call.
        pub fn feedDTDEntityStart(
            ctx: Self,
            kind: ReferenceKind,
            /// * `[]const u8`
            /// * `Tokenizer.Range`
            name: anytype,
        ) !void {
            comptime if (checkSrcType(.not_optional, @TypeOf(name))) |err| @compileError(err);
            return ctx.inner.feedDTDEntityStart(kind, name);
        }

        /// Called consecutively to construct the text.
        /// Can be interspersed with calls to `feedDTDEntityValueReference`.
        /// Terminated by a call to `feedDTDEntityValueEnd`.
        pub fn feedDTDEntityValueTextSegment(
            ctx: Self,
            /// * `[]const u8`
            /// * `Tokenizer.Range`
            text: anytype,
        ) !void {
            comptime if (checkSrcType(.not_optional, @TypeOf(text))) |err| @compileError(err);
            return ctx.inner.feedDTDEntityValueTextSegment(text);
        }

        pub fn feedDTDEntityValueReference(
            ctx: Self,
            kind: ReferenceKind,
            /// * `[]const u8`
            /// * `Tokenizer.Range`
            id: anytype,
        ) !void {
            comptime if (checkSrcType(.not_optional, @TypeOf(id))) |err| @compileError(err);
            return ctx.inner.feedDTDEntityValueReference(kind, id);
        }

        pub fn feedDTDEntityValueEnd(ctx: Self) !void {
            return ctx.inner.feedDTDEntityValueEnd();
        }

        pub fn feedDTDEntityValueExternalId(
            ctx: Self,
            kind: ExternalIdKind,
            /// * `?[]const u8`
            /// * `?Tokenizer.Range`
            pubid_lit: anytype,
            /// * `[]const u8`
            /// * `Tokenizer.Range`
            system_lit: anytype,
            /// * `?[]const u8`
            /// * `?Tokenizer.Range`
            ndata_name: anytype,
        ) !void {
            comptime if (checkSrcType(.is_optional, @TypeOf(pubid_lit))) |err| @compileError(err);
            comptime if (checkSrcType(.not_optional, @TypeOf(system_lit))) |err| @compileError(err);
            comptime if (checkSrcType(.is_optional, @TypeOf(ndata_name))) |err| @compileError(err);
            return ctx.inner.feedDTDEntityValueExternalId(kind, pubid_lit, system_lit, ndata_name);
        }

        // --- DTD ELEMENT ---

        /// Opens a DTD Element Declaration, providing the name.
        /// The next call can be to:
        /// * `feedDTDElementEmptyOrAny`:
        ///   ends the element declaration immediately as the content specification is simply 'EMPTY' or 'ANY'.
        pub fn feedDTDElementStart(
            ctx: Self,
            /// * `[]const u8`
            /// * `Tokenizer.Range`
            name: anytype,
        ) !void {
            comptime if (checkSrcType(.not_optional, @TypeOf(name))) |err| @compileError(err);
            return ctx.inner.feedDTDElementStart(name);
        }

        pub fn feedDTDElementEmptyOrAny(ctx: Self, kind: EmptyOrAny) !void {
            return ctx.inner.feedDTDElementEmptyOrAny(kind);
        }

        /// Feeds a Content Particle composed by a name.
        pub fn feedDTDElementIdentifier(
            ctx: Self,
            /// * `[]const u8`
            /// * `Tokenizer.Range`
            name: anytype,
            quantity: ?ContentParticleQuantity,
        ) !void {
            comptime if (checkSrcType(.not_optional, @TypeOf(name))) |err| @compileError(err);
            return ctx.inner.feedDTDElementIdentifier(name, quantity);
        }

        /// Feeds a left parentheses, indicating the start of a grouped sequence or choice.
        /// Can be followed by:
        /// * `feedDTDElementLParen`, nesting deeper.
        /// * `feedDTDElementIdentifier`,
        pub fn feedDTDElementLParen(ctx: Self) !void {
            return ctx.inner.feedDTDElementLParen();
        }

        /// Feeds a right parentheses, indicating the end of the current grouped sequence or choice.
        /// Provides the quantity indicator following it.
        /// Indicates whether this is the end of the element content specification.
        pub fn feedDTDElementRParen(ctx: Self, quantity: ?ContentParticleQuantity, end: bool) !void {
            return ctx.inner.feedDTDElementRParen(quantity, end);
        }

        /// Feeds the '#PCDATA' token.
        pub fn feedDTDElementPCData(ctx: Self) !void {
            return ctx.inner.feedDTDElementPCData();
        }

        /// Feeds the '|' token.
        pub fn feedDTDElementChoiceSep(ctx: Self) !void {
            return ctx.inner.feedDTDElementChoiceSep();
        }

        /// Feeds the ',' token.
        pub fn feedDTDElementSequenceSep(ctx: Self) !void {
            return ctx.inner.feedDTDElementSequenceSep();
        }

        // --- DTD ATTLIST ---

        /// Opens a DTD Attribute List Declaration, providing the name.
        /// Can be followed by calls to:
        /// * `feedDTDAttlistDefStart`
        /// * `feedDTDAttlistDefaultDeclStart`
        pub fn feedDTDAttlistStart(
            ctx: Self,
            /// * `[]const u8`
            /// * `Tokenizer.Range`
            name: anytype,
        ) !void {
            comptime if (checkSrcType(.not_optional, @TypeOf(name))) |err| @compileError(err);
            return ctx.inner.feedDTDAttlistStart(name);
        }

        /// Starts an attribute definition, providing the name
        /// and type.
        /// If `attr_type == .NOTATION` or `attr_type == .enumeration`,
        /// this will be followed by a call to `feedDTDAttlistDefNmtoken`,
        /// otherwise it will be followed by a call to `feedDTDAttlistDefaultDeclStart`.
        pub fn feedDTDAttlistDefStart(
            ctx: Self,
            /// * `[]const u8`
            /// * `Tokenizer.Range`
            name: anytype,
            attr_type: AttributeType,
        ) !void {
            comptime if (checkSrcType(.not_optional, @TypeOf(name))) |err| @compileError(err);
            return ctx.inner.feedDTDAttlistDefStart(name, attr_type);
        }

        /// This may be followed by another call to `feedDTDAttlistDefNmtoken`,
        /// or terminated by a call to `feedDTDAttlistDefaultDeclStart`.
        pub fn feedDTDAttlistDefNmtoken(
            ctx: Self,
            /// * `[]const u8`
            /// * `Tokenizer.Range`
            name: anytype,
        ) !void {
            comptime if (checkSrcType(.not_optional, @TypeOf(name))) |err| @compileError(err);
            return ctx.inner.feedDTDAttlistDefNmtoken(name);
        }

        /// Feeds the '#REQUIRED', '#IMPLIED', or '#FIXED' token, or nothing.
        /// If `kind == .FIXED` or `kind == null`, this will be followed by a series of consecutive interspersed
        /// calls to `feedDTDAttlistDefaultDeclValueSegment` and to `feedDTDAttlistDefaultDeclValueReference`,
        /// terminated by a call to `feedDTDAttlistDefaultDeclValueEnd`.
        pub fn feedDTDAttlistDefaultDeclStart(ctx: Self, kind: ?DefaultDeclKind) !void {
            return ctx.inner.feedDTDAttlistDefaultDeclStart(kind);
        }

        pub fn feedDTDAttlistDefaultDeclValueSegment(
            ctx: Self,
            /// * `[]const u8`
            /// * `Tokenizer.Range`
            text: anytype,
        ) !void {
            comptime if (checkSrcType(.not_optional, @TypeOf(text))) |err| @compileError(err);
            return ctx.inner.feedDTDAttlistDefaultDeclValueSegment(text);
        }

        pub fn feedDTDAttlistDefaultDeclValueReference(
            ctx: Self,
            /// * `[]const u8`
            /// * `Tokenizer.Range`
            name: anytype,
        ) !void {
            comptime if (checkSrcType(.not_optional, @TypeOf(name))) |err| @compileError(err);
            return ctx.inner.feedDTDAttlistDefaultDeclValueReference(name);
        }

        /// Followed by a call to `feedDTDAttlistDefStart` or `feedDTDAttlistEnd`.
        pub fn feedDTDAttlistDefaultDeclValueEnd(ctx: Self) !void {
            return ctx.inner.feedDTDAttlistDefaultDeclValueEnd();
        }

        pub fn feedDTDAttlistEnd(ctx: Self) !void {
            return ctx.inner.feedDTDAttlistEnd();
        }

        // --- DTD NOTATION ---

        /// Opens a DTD Notation declaration.
        pub fn feedDTDNotation(
            ctx: Self,
            /// * `[]const u8`
            /// * `Tokenizer.Range`
            name: anytype,
            ext_id_kind: ExternalIdKind,
            /// * `?[]const u8`
            /// * `?Tokenizer.Range`
            sys_literal: anytype,
            /// * `?[]const u8`
            /// * `?Tokenizer.Range`
            pubid_literal: anytype,
        ) !void {
            comptime if (checkSrcType(.not_optional, @TypeOf(name))) |err| @compileError(err);
            comptime if (checkSrcType(.is_optional, @TypeOf(sys_literal))) |err| @compileError(err);
            comptime if (checkSrcType(.is_optional, @TypeOf(pubid_literal))) |err| @compileError(err);
            return ctx.inner.feedDTDNotation(name, ext_id_kind, sys_literal, pubid_literal);
        }
    };
}

pub const ParseError = error{
    NonWhitespaceInProlog,
    AngleBracketLeftBang,
    InvalidReferenceEnd,
    EmptyReference,
    ExternalIdMissingSystemLiteral,

    EmptyPI,
    UnclosedPI,

    InvalidCommentStart,
    CommentDashDash,
    CommentEndTripleDash,

    InvalidDTDStart,
    UnclosedDTD,
    MissingDTDSpacing,
    UnexpectedDTDToken,

    AngleBracketLeftInAttributeValue,
    UnclosedPubidLiteral,
    InvalidPubidLiteral,
    UnclosedSystemLiteral,

    UnclosedDTDEntity,
    UnclosedDTDElement,
    UnclosedDTDAttlist,
    UnclosedDTDNotation,
};

pub inline fn parseSlice(
    /// Must be a non-streaming tokenizer.
    tokenizer: *Tokenizer,
    /// Must satisfy the interface described by `ParseCtx`.
    parse_ctx_impl: anytype,
) !Tokenizer.TokenType {
    return parseUntilAngleBracketLeftReaderOrFull(parse_ctx_impl, tokenizer, null, .{
        .mbr = .{
            .reader = {},
            .read_buffer = {},
        },
        .src_buffer = {},
    });
}

pub inline fn parseReader(
    /// `std.io.Reader(...)`
    reader: anytype,
    /// Used as a temporary buffer for certain reads.
    read_buffer: []u8,
    /// Used as a buffer for strings that will be forwarded to `parse_ctx_impl`.
    src_buffer: StrBuffer,
    /// Must satisfy the interface described by `ParseCtx`.
    parse_ctx_impl: anytype,
) !Tokenizer.TokenType {
    var tokenizer = Tokenizer.initStreaming();
    return parseUntilAngleBracketLeftReaderOrFull(parse_ctx_impl, &tokenizer, @TypeOf(reader), .{
        .mbr = .{
            .reader = reader,
            .read_buffer = read_buffer,
        },
        .src_buffer = src_buffer,
    });
}

fn MaybeBufferedReader(comptime MaybeReader: ?type) type {
    return struct {
        reader: (MaybeReader orelse void),
        read_buffer: if (MaybeReader != null) []u8 else void,
    };
}

fn MBRAndSrcBuf(comptime MaybeReader: ?type) type {
    return struct {
        mbr: MaybeBufferedReader(MaybeReader),
        src_buffer: if (MaybeReader != null) StrBuffer else void,
        const Self = @This();

        fn clearSrcBuffer(mbr_and_src: Self) void {
            if (MaybeReader == null) return;
            switch (mbr_and_src.src_buffer) {
                inline else => |buf| buf.clearRetainingCapacity(),
            }
        }
    };
}

fn parseUntilAngleBracketLeftReaderOrFull(
    parse_ctx_impl: anytype,
    tokenizer: *Tokenizer,
    comptime MaybeReader: ?type,
    mbr_and_src: MBRAndSrcBuf(MaybeReader),
) !Tokenizer.TokenType {
    const Impl = @TypeOf(parse_ctx_impl);
    const parse_ctx: ParseCtx(Impl) = .{ .inner = parse_ctx_impl };

    const mbr = mbr_and_src.mbr;
    mbr_and_src.clearSrcBuffer();

    while (true) {
        switch (try parse_helper.nextTokenType(tokenizer, .non_markup, MaybeReader, mbr)) {
            .eof, .angle_bracket_left => |tag| return tag,

            .text_data => switch (try parse_helper.skipWhitespaceTokenSrc(tokenizer, .non_markup, MaybeReader, mbr)) {
                .all_whitespace => {},
                .non_whitespace => return ParseError.NonWhitespaceInProlog,
            },
            .ampersand => return ParseError.NonWhitespaceInProlog,
            .cdata_start => return ParseError.NonWhitespaceInProlog,
            .invalid_angle_bracket_left_bang => return ParseError.AngleBracketLeftBang,

            .invalid_comment_start_single_dash => return ParseError.InvalidCommentStart,
            .comment_start => try parse_helper.handleCommentSkip(tokenizer, MaybeReader, mbr),
            .pi_start => try parse_helper.handlePi(Impl, parse_ctx, tokenizer, MaybeReader, mbr_and_src),

            .invalid_dtd_start => {
                try parse_helper.skipTokenStr(tokenizer, .non_markup, MaybeReader, mbr);
                return ParseError.InvalidDTDStart;
            },
            .dtd_start => {
                switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                    else => return ParseError.MissingDTDSpacing,
                    .eof => return ParseError.UnclosedDTD,
                    .tag_whitespace => {},
                }
                switch (try parse_helper.skipWhitespaceTokenSrc(tokenizer, .dtd, MaybeReader, mbr)) {
                    .all_whitespace => {},
                    .non_whitespace => unreachable,
                }

                switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                    else => return ParseError.UnexpectedDTDToken,
                    .eof => return ParseError.UnclosedDTD,
                    .tag_whitespace => unreachable,
                    .tag_token => {},
                }
                mbr_and_src.clearSrcBuffer();
                try parse_ctx.feedDTDName(try parse_helper.nextTokenFullStrOrRange(tokenizer, .dtd, MaybeReader, mbr_and_src));

                const secondary_tt = switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                    .tag_whitespace => switch (try parse_helper.skipWhitespaceTokenSrc(tokenizer, .dtd, MaybeReader, mbr)) {
                        .all_whitespace => try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr),
                        .non_whitespace => unreachable,
                    },
                    else => |tag| tag,
                };

                const tertiary_tt = switch (secondary_tt) {
                    else => return ParseError.UnexpectedDTDToken,
                    .eof => return ParseError.UnclosedDTD,
                    .tag_whitespace => unreachable,

                    // ExternalId
                    .tag_token => tt: {
                        mbr_and_src.clearSrcBuffer();
                        const ext_id_kind, //
                        const pubid_lit, //
                        const system_lit, //
                        const tt_after_str //
                        = try parse_helper.parseDTDExternalIdParts(tokenizer, MaybeReader, mbr_and_src);
                        try parse_ctx.feedDTDExternalId(ext_id_kind, pubid_lit, system_lit orelse return ParseError.ExternalIdMissingSystemLiteral);
                        break :tt tt_after_str;
                    },

                    .square_bracket_left,
                    .angle_bracket_right,
                    => |tag| tag,
                };

                const quartary_tt = switch (tertiary_tt) {
                    else => return ParseError.UnexpectedDTDToken,
                    .eof => return ParseError.UnclosedDTD,

                    .square_bracket_left => while (true) switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                        else => return ParseError.UnexpectedDTDToken,

                        .square_bracket_right => break try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr),

                        .tag_whitespace => switch (try parse_helper.skipWhitespaceTokenSrc(tokenizer, .dtd, MaybeReader, mbr)) {
                            .all_whitespace => {},
                            .non_whitespace => unreachable,
                        },

                        .comment_start => try parse_helper.handleCommentSkip(tokenizer, MaybeReader, mbr),
                        .pi_start => try parse_helper.handlePi(Impl, parse_ctx, tokenizer, MaybeReader, mbr_and_src),

                        .dtd_decl => {
                            const DtdDeclText = enum {
                                @"<!NOTATION",
                                @"<!ATTLIST",
                                @"<!ELEMENT",
                                @"<!ENTITY",
                            };
                            const dtd_type = (try parse_helper.nextTokenSrcAsEnum(tokenizer, .dtd, MaybeReader, mbr, DtdDeclText)) orelse
                                return ParseError.UnexpectedDTDToken;

                            switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                else => return ParseError.MissingDTDSpacing,
                                .eof => return ParseError.UnclosedDTD,
                                .tag_whitespace => {},
                            }
                            switch (try parse_helper.skipWhitespaceTokenSrc(tokenizer, .dtd, MaybeReader, mbr)) {
                                .all_whitespace => {},
                                .non_whitespace => unreachable,
                            }

                            switch (dtd_type) {
                                .@"<!ENTITY" => {
                                    const ent_def_kind: ReferenceKind = switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                        else => return ParseError.UnexpectedDTDToken,
                                        .eof => return ParseError.UnclosedDTDEntity,
                                        .tag_token => .general,
                                        .percent => pct: {
                                            switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                                else => return ParseError.MissingDTDSpacing,
                                                .eof => return ParseError.UnclosedDTDEntity,
                                                .tag_whitespace => {},
                                            }
                                            switch (try parse_helper.skipWhitespaceTokenSrc(tokenizer, .dtd, MaybeReader, mbr)) {
                                                .all_whitespace => {},
                                                .non_whitespace => unreachable,
                                            }

                                            switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                                else => return ParseError.UnexpectedDTDToken,
                                                .eof => return ParseError.UnclosedDTDEntity,
                                                .tag_token => {},
                                            }
                                            break :pct .parsed;
                                        },
                                    };

                                    mbr_and_src.clearSrcBuffer();
                                    try parse_ctx.feedDTDEntityStart(ent_def_kind, try parse_helper.nextTokenFullStrOrRange(tokenizer, .dtd, MaybeReader, mbr_and_src));

                                    switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                        else => return ParseError.MissingDTDSpacing,
                                        .eof => return ParseError.UnclosedDTDEntity,
                                        .tag_whitespace => {},
                                    }
                                    switch (try parse_helper.skipWhitespaceTokenSrc(tokenizer, .dtd, MaybeReader, mbr)) {
                                        .all_whitespace => {},
                                        .non_whitespace => unreachable,
                                    }

                                    switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                        else => return ParseError.UnexpectedDTDToken,
                                        .eof => return ParseError.UnclosedDTDEntity,

                                        .quote_single,
                                        .quote_double,
                                        => |open_quote| {
                                            const str_ctx: Tokenizer.Context = switch (open_quote) {
                                                .quote_single => .entity_value_quote_single,
                                                .quote_double => .entity_value_quote_double,
                                                else => unreachable,
                                            };

                                            while (true) switch (try parse_helper.nextTokenType(tokenizer, str_ctx, MaybeReader, mbr)) {
                                                else => unreachable,

                                                .quote_single,
                                                .quote_double,
                                                => |close_quote| {
                                                    assert(open_quote == close_quote);
                                                    break;
                                                },
                                                .text_data => if (MaybeReader != null) {
                                                    while (try parse_helper.nextTokenSegment(tokenizer, str_ctx, mbr)) |segment| {
                                                        try parse_ctx.feedDTDEntityValueTextSegment(segment);
                                                    }
                                                } else {
                                                    const range = tokenizer.nextSrcNoUnderrun(str_ctx);
                                                    try parse_ctx.feedDTDEntityValueTextSegment(range);
                                                },
                                                .ampersand, .percent => |ref_start| {
                                                    const ref_kind: ReferenceKind = switch (ref_start) {
                                                        .ampersand => .general,
                                                        .percent => .parsed,
                                                        else => unreachable,
                                                    };

                                                    // TODO: check that `.reference` behaves correctly here
                                                    switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
                                                        else => unreachable,
                                                        .invalid_reference_end => return ParseError.InvalidReferenceEnd,
                                                        .semicolon => return ParseError.EmptyReference,
                                                        .tag_token => {
                                                            mbr_and_src.clearSrcBuffer();
                                                            const ref_id = try parse_helper.nextTokenFullStrOrRange(tokenizer, .reference, MaybeReader, mbr_and_src);
                                                            switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
                                                                .invalid_reference_end => return ParseError.InvalidReferenceEnd,
                                                                .tag_token => unreachable,
                                                                .semicolon => {},
                                                                else => unreachable,
                                                            }
                                                            try parse_ctx.feedDTDEntityValueReference(ref_kind, ref_id);
                                                        },
                                                    }
                                                },
                                            };

                                            try parse_ctx.feedDTDEntityValueEnd();

                                            switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                                else => return ParseError.UnexpectedDTDToken,
                                                .eof => return ParseError.UnclosedDTDEntity,
                                                .angle_bracket_right => {},
                                            }
                                        },

                                        .tag_token => {
                                            mbr_and_src.clearSrcBuffer();

                                            const ext_id_kind, //
                                            const pubid_lit, //
                                            const maybe_system_lit, //
                                            const tt_after_str //
                                            = try parse_helper.parseDTDExternalIdParts(tokenizer, MaybeReader, mbr_and_src);
                                            const system_lit = maybe_system_lit orelse return ParseError.ExternalIdMissingSystemLiteral;

                                            const ndata_name: ?if (MaybeReader != null) []const u8 else Tokenizer.Range = switch (tt_after_str) {
                                                else => |tag| {
                                                    std.debug.print("\n{any}\n", .{.{ ext_id_kind, pubid_lit, system_lit, tag }});
                                                    return ParseError.MissingDTDSpacing;
                                                },
                                                .eof => return ParseError.UnclosedDTDEntity,
                                                .tag_whitespace => blk: {
                                                    switch (try parse_helper.skipWhitespaceTokenSrc(tokenizer, .dtd, MaybeReader, mbr)) {
                                                        .all_whitespace => {},
                                                        .non_whitespace => unreachable,
                                                    }

                                                    switch (ent_def_kind) {
                                                        .general => switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                                            else => return ParseError.UnexpectedDTDToken,
                                                            .eof => return ParseError.UnclosedDTDEntity,
                                                            .tag_whitespace => unreachable,
                                                            .tag_token => {
                                                                _ = (try parse_helper.nextTokenSrcAsEnum(tokenizer, .dtd, MaybeReader, mbr, enum { NDATA })) orelse
                                                                    return ParseError.UnexpectedDTDToken;

                                                                switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                                                    else => return ParseError.MissingDTDSpacing,
                                                                    .eof => return ParseError.UnclosedDTDEntity,
                                                                    .tag_whitespace => {},
                                                                }
                                                                switch (try parse_helper.skipWhitespaceTokenSrc(tokenizer, .dtd, MaybeReader, mbr)) {
                                                                    .all_whitespace => {},
                                                                    .non_whitespace => unreachable,
                                                                }

                                                                const ndata_name = try parse_helper.nextTokenFullStrOrRange(tokenizer, .dtd, MaybeReader, mbr_and_src);

                                                                switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                                                    else => return ParseError.UnexpectedDTDToken,
                                                                    .eof => return ParseError.UnclosedDTDEntity,
                                                                    .tag_whitespace => switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                                                        else => return ParseError.UnexpectedDTDToken,
                                                                        .eof => return ParseError.UnclosedDTDEntity,
                                                                        .tag_whitespace => unreachable,
                                                                        .angle_bracket_right => {},
                                                                    },
                                                                    .angle_bracket_right => {},
                                                                }

                                                                break :blk ndata_name;
                                                            },
                                                            .angle_bracket_right => break :blk null,
                                                        },
                                                        .parsed => switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                                            else => return ParseError.UnexpectedDTDToken,
                                                            .eof => return ParseError.UnclosedDTDEntity,
                                                            .tag_whitespace => unreachable,
                                                            .angle_bracket_right => break :blk null,
                                                        },
                                                    }
                                                },
                                                .angle_bracket_right => null,
                                            };

                                            try parse_ctx.feedDTDEntityValueExternalId(ext_id_kind, pubid_lit, system_lit, ndata_name);
                                        },
                                    }
                                },
                                .@"<!ELEMENT" => {
                                    switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                        else => return ParseError.UnexpectedDTDToken,
                                        .eof => return ParseError.UnclosedDTDElement,
                                        .tag_whitespace => unreachable,
                                        .tag_token => {},
                                    }
                                    mbr_and_src.clearSrcBuffer();
                                    try parse_ctx.feedDTDElementStart(try parse_helper.nextTokenFullStrOrRange(tokenizer, .dtd, MaybeReader, mbr_and_src));

                                    switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                        else => return ParseError.MissingDTDSpacing,
                                        .eof => return ParseError.UnclosedDTDElement,
                                        .tag_whitespace => {},
                                    }
                                    switch (try parse_helper.skipWhitespaceTokenSrc(tokenizer, .dtd, MaybeReader, mbr)) {
                                        .all_whitespace => {},
                                        .non_whitespace => unreachable,
                                    }

                                    switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                        else => return ParseError.UnexpectedDTDToken,
                                        .eof => return ParseError.UnclosedDTDElement,
                                        .tag_whitespace => unreachable,

                                        .tag_token => {
                                            mbr_and_src.clearSrcBuffer();
                                            const tag_tok = try parse_helper.nextTokenFullStrOrRange(tokenizer, .dtd, MaybeReader, mbr_and_src);
                                            const tag_tok_str: []const u8 = if (MaybeReader != null) tag_tok else tag_tok.toStr(tokenizer.src);
                                            const tt_after_tok = if (std.meta.stringToEnum(EmptyOrAny, tag_tok_str)) |empty_or_any| blk: {
                                                try parse_ctx.feedDTDElementEmptyOrAny(empty_or_any);
                                                break :blk try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr);
                                            } else blk: {
                                                const quantity, const tt_after_quantity = try parse_helper.parseDTDElementCpQuantity(tokenizer, MaybeReader, mbr);
                                                try parse_ctx.feedDTDElementIdentifier(tag_tok, quantity);
                                                break :blk tt_after_quantity;
                                            };

                                            switch (tt_after_tok) {
                                                else => return ParseError.UnexpectedDTDToken,
                                                .eof => return ParseError.UnclosedDTDElement,
                                                .tag_whitespace => unreachable,
                                                .angle_bracket_right => {},
                                            }
                                        },
                                        .lparen => {
                                            try parse_ctx.feedDTDElementLParen();

                                            var paren_depth: u64 = 1;
                                            var cached_tt: ?Tokenizer.TokenType = null;
                                            while (true) {
                                                const current_first_tt = blk: {
                                                    defer cached_tt = null;
                                                    break :blk cached_tt orelse try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr);
                                                };
                                                const non_whitespace_tt = switch (current_first_tt) {
                                                    .tag_whitespace => try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr),
                                                    else => |tt| tt,
                                                };
                                                switch (non_whitespace_tt) {
                                                    else => return ParseError.UnexpectedDTDToken,
                                                    .eof => return ParseError.UnclosedDTDElement,
                                                    .tag_whitespace => unreachable,

                                                    .comma => try parse_ctx.feedDTDElementSequenceSep(),
                                                    .pipe => try parse_ctx.feedDTDElementChoiceSep(),

                                                    .lparen => {
                                                        paren_depth += 1;
                                                        try parse_ctx.feedDTDElementLParen();
                                                    },

                                                    .rparen => {
                                                        paren_depth -= 1;
                                                        const quantity, const tt_after_quantity = try parse_helper.parseDTDElementCpQuantity(tokenizer, MaybeReader, mbr);
                                                        switch (tt_after_quantity) {
                                                            else => return ParseError.UnexpectedDTDToken,
                                                            .eof => return ParseError.UnclosedDTDElement,
                                                            .qmark, .asterisk, .plus => unreachable,
                                                            .tag_whitespace => unreachable,
                                                            .angle_bracket_right, .rparen, .comma, .pipe => |tt| cached_tt = tt,
                                                        }
                                                        const end = paren_depth == 0;
                                                        try parse_ctx.feedDTDElementRParen(quantity, end);
                                                        if (end) break;
                                                    },
                                                    .tag_token => blk: {
                                                        mbr_and_src.clearSrcBuffer();
                                                        const name = try parse_helper.nextTokenFullStrOrRange(tokenizer, .dtd, MaybeReader, mbr_and_src);
                                                        const name_str: []const u8 = if (MaybeReader != null) name else name.toStr(tokenizer.src);
                                                        if (std.mem.eql(u8, name_str, "#PCDATA")) {
                                                            try parse_ctx.feedDTDElementPCData();
                                                            break :blk;
                                                        }
                                                        const quantity, const tt_after_quantity = try parse_helper.parseDTDElementCpQuantity(tokenizer, MaybeReader, mbr);
                                                        switch (tt_after_quantity) {
                                                            else => return ParseError.UnexpectedDTDToken,
                                                            .eof => return ParseError.UnclosedDTDElement,
                                                            .qmark, .asterisk, .plus => unreachable,
                                                            .tag_whitespace => unreachable,
                                                            .angle_bracket_right, .rparen, .comma, .pipe => |tt| cached_tt = tt,
                                                        }
                                                        try parse_ctx.feedDTDElementIdentifier(name, quantity);
                                                    },
                                                }
                                            }

                                            switch (blk: {
                                                defer cached_tt = null;
                                                break :blk cached_tt orelse try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr);
                                            }) {
                                                else => return ParseError.UnexpectedDTDToken,
                                                .tag_whitespace => unreachable,
                                                .angle_bracket_right => {},
                                            }
                                        },
                                    }
                                },
                                .@"<!ATTLIST" => {
                                    switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                        else => return ParseError.UnexpectedDTDToken,
                                        .eof => return ParseError.UnclosedDTDAttlist,
                                        .tag_whitespace => unreachable,
                                        .tag_token => {},
                                    }
                                    mbr_and_src.clearSrcBuffer();
                                    try parse_ctx.feedDTDAttlistStart(try parse_helper.nextTokenFullStrOrRange(tokenizer, .dtd, MaybeReader, mbr_and_src));

                                    while (true) {
                                        switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                            else => return ParseError.MissingDTDSpacing,
                                            .eof => return ParseError.UnclosedDTDAttlist,
                                            .angle_bracket_right => break,
                                            .tag_whitespace => {},
                                        }
                                        switch (try parse_helper.skipWhitespaceTokenSrc(tokenizer, .dtd, MaybeReader, mbr)) {
                                            .all_whitespace => {},
                                            .non_whitespace => unreachable,
                                        }

                                        switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                            else => return ParseError.UnexpectedDTDToken,
                                            .eof => return ParseError.UnclosedDTDAttlist,
                                            .tag_whitespace => unreachable,
                                            .angle_bracket_right => break,
                                            .tag_token => {},
                                        }

                                        mbr_and_src.clearSrcBuffer();
                                        var attr_name = try parse_helper.nextTokenFullStrOrRange(tokenizer, .dtd, MaybeReader, mbr_and_src);

                                        switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                            else => return ParseError.MissingDTDSpacing,
                                            .eof => return ParseError.UnclosedDTDAttlist,
                                            .tag_whitespace => {},
                                        }
                                        switch (try parse_helper.skipWhitespaceTokenSrc(tokenizer, .dtd, MaybeReader, mbr)) {
                                            .all_whitespace => {},
                                            .non_whitespace => unreachable,
                                        }

                                        const attr_type: AttributeType = switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                            else => return ParseError.UnexpectedDTDToken,
                                            .eof => return ParseError.UnclosedDTDAttlist,
                                            .tag_whitespace => unreachable,

                                            // StringType | TokenizedType | NotationType
                                            .tag_token => blk: {
                                                const AttributeTypeTokenSubset = comptime @Type(.{ .Enum = e: {
                                                    var info = @typeInfo(AttributeType).Enum;
                                                    assert(std.mem.eql(u8, info.fields[info.fields.len - 1].name, "enumeration"));
                                                    info.fields = info.fields[0 .. info.fields.len - 1];
                                                    info.decls = &.{};
                                                    break :e info;
                                                } });

                                                const attr_type = (try parse_helper.nextTokenSrcAsEnum(tokenizer, .dtd, MaybeReader, mbr, AttributeTypeTokenSubset)) orelse
                                                    return ParseError.UnexpectedDTDToken;

                                                switch (attr_type) {
                                                    .CDATA,

                                                    .ID,
                                                    .IDREF,
                                                    .IDREFS,
                                                    .ENTITY,
                                                    .ENTITIES,
                                                    .NMTOKEN,
                                                    .NMTOKENS,
                                                    => {},

                                                    .NOTATION => {
                                                        switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                                            else => return ParseError.MissingDTDSpacing,
                                                            .eof => return ParseError.UnclosedDTDAttlist,
                                                            .tag_whitespace => {},
                                                        }
                                                        switch (try parse_helper.skipWhitespaceTokenSrc(tokenizer, .dtd, MaybeReader, mbr)) {
                                                            .all_whitespace => {},
                                                            .non_whitespace => unreachable,
                                                        }

                                                        switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                                            else => return ParseError.UnexpectedDTDToken,
                                                            .eof => return ParseError.UnclosedDTDAttlist,
                                                            .lparen => {},
                                                        }
                                                    },
                                                }

                                                break :blk switch (attr_type) {
                                                    inline else => |tag| @field(AttributeType, @tagName(tag)),
                                                };
                                            },

                                            // Enumeration
                                            .lparen => .enumeration,
                                        };

                                        try parse_ctx.feedDTDAttlistDefStart(attr_name, attr_type);
                                        attr_name = undefined;
                                        switch (attr_type) {
                                            .CDATA,

                                            .ID,
                                            .IDREF,
                                            .IDREFS,
                                            .ENTITY,
                                            .ENTITIES,
                                            .NMTOKEN,
                                            .NMTOKENS,
                                            => {},

                                            .NOTATION,
                                            .enumeration,
                                            => {
                                                const first_non_whitespace_tt = switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                                    else => |tt| tt,
                                                    .tag_whitespace => tt: {
                                                        switch (try parse_helper.skipWhitespaceTokenSrc(tokenizer, .dtd, MaybeReader, mbr)) {
                                                            .all_whitespace => {},
                                                            .non_whitespace => unreachable,
                                                        }
                                                        break :tt try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr);
                                                    },
                                                };
                                                switch (first_non_whitespace_tt) {
                                                    else => return ParseError.UnexpectedDTDToken,
                                                    .eof => return ParseError.UnclosedDTDAttlist,
                                                    .tag_whitespace => unreachable,
                                                    .tag_token => {},
                                                }
                                                mbr_and_src.clearSrcBuffer();
                                                try parse_ctx.feedDTDAttlistDefNmtoken(try parse_helper.nextTokenFullStrOrRange(tokenizer, .dtd, MaybeReader, mbr_and_src));

                                                while (true) {
                                                    const non_whitespace_tt_1 = switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                                        else => |tt| tt,
                                                        .tag_whitespace => tt: {
                                                            switch (try parse_helper.skipWhitespaceTokenSrc(tokenizer, .dtd, MaybeReader, mbr)) {
                                                                .all_whitespace => {},
                                                                .non_whitespace => unreachable,
                                                            }
                                                            break :tt try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr);
                                                        },
                                                    };
                                                    switch (non_whitespace_tt_1) {
                                                        else => return ParseError.UnexpectedDTDToken,
                                                        .eof => return ParseError.UnclosedDTDAttlist,
                                                        .tag_whitespace => unreachable,
                                                        .rparen => break,
                                                        .pipe => {},
                                                    }

                                                    const non_whitespace_tt_2 = switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                                        else => |tt| tt,
                                                        .tag_whitespace => tt: {
                                                            switch (try parse_helper.skipWhitespaceTokenSrc(tokenizer, .dtd, MaybeReader, mbr)) {
                                                                .all_whitespace => {},
                                                                .non_whitespace => unreachable,
                                                            }
                                                            break :tt try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr);
                                                        },
                                                    };
                                                    switch (non_whitespace_tt_2) {
                                                        else => return ParseError.UnexpectedDTDToken,
                                                        .eof => return ParseError.UnclosedDTDAttlist,
                                                        .tag_whitespace => unreachable,
                                                        .tag_token => {},
                                                    }

                                                    mbr_and_src.clearSrcBuffer();
                                                    try parse_ctx.feedDTDAttlistDefNmtoken(try parse_helper.nextTokenFullStrOrRange(tokenizer, .dtd, MaybeReader, mbr_and_src));
                                                }
                                            },
                                        }

                                        switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                            else => return ParseError.MissingDTDSpacing,
                                            .eof => return ParseError.UnclosedDTDAttlist,
                                            .tag_whitespace => {},
                                        }
                                        switch (try parse_helper.skipWhitespaceTokenSrc(tokenizer, .dtd, MaybeReader, mbr)) {
                                            .all_whitespace => {},
                                            .non_whitespace => unreachable,
                                        }

                                        const dd_kind: ?DefaultDeclKind, //
                                        const maybe_open_quote: ?Tokenizer.TokenType //
                                        = switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                            else => return ParseError.UnexpectedDTDToken,
                                            .eof => return ParseError.UnclosedDTDAttlist,
                                            .tag_whitespace => unreachable,
                                            .quote_single, .quote_double => |tag| .{ null, tag },
                                            .tag_token => blk: {
                                                const KindStr = enum { @"#REQUIRED", @"#IMPLIED", @"#FIXED" };
                                                const kind_str = (try parse_helper.nextTokenSrcAsEnum(tokenizer, .dtd, MaybeReader, mbr, KindStr)) orelse
                                                    return ParseError.UnexpectedDTDToken;
                                                const dd_kind: DefaultDeclKind = switch (kind_str) {
                                                    inline else => |tag| @field(DefaultDeclKind, @tagName(tag)["#".len..]),
                                                };
                                                break :blk .{ dd_kind, null };
                                            },
                                        };

                                        try parse_ctx.feedDTDAttlistDefaultDeclStart(dd_kind);
                                        if (dd_kind) |unwrapped| if (unwrapped != .FIXED) continue;
                                        const open_quote = maybe_open_quote orelse switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                            .tag_whitespace => tt: {
                                                switch (try parse_helper.skipWhitespaceTokenSrc(tokenizer, .dtd, MaybeReader, mbr)) {
                                                    .all_whitespace => {},
                                                    .non_whitespace => unreachable,
                                                }
                                                break :tt try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr);
                                            },
                                            else => |tag| tag,
                                        };
                                        const str_ctx: Tokenizer.Context = switch (open_quote) {
                                            .quote_single => .attribute_value_quote_single,
                                            .quote_double => .attribute_value_quote_double,
                                            else => unreachable,
                                        };

                                        while (true) switch (try parse_helper.nextTokenType(tokenizer, str_ctx, MaybeReader, mbr)) {
                                            else => unreachable,

                                            .quote_single,
                                            .quote_double,
                                            => |close_quote| {
                                                assert(open_quote == close_quote);
                                                break;
                                            },

                                            .angle_bracket_left => return ParseError.AngleBracketLeftInAttributeValue,
                                            .ampersand => {
                                                switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
                                                    .invalid_reference_end => return ParseError.InvalidReferenceEnd,
                                                    .semicolon => return ParseError.EmptyReference,
                                                    .tag_token => {},
                                                    else => unreachable,
                                                }
                                                mbr_and_src.clearSrcBuffer();
                                                const ref_name = try parse_helper.nextTokenFullStrOrRange(tokenizer, .reference, MaybeReader, mbr_and_src);
                                                try parse_ctx.feedDTDAttlistDefaultDeclValueReference(ref_name);
                                            },
                                            .text_data => {
                                                if (MaybeReader != null) {
                                                    while (try parse_helper.nextTokenSegment(tokenizer, str_ctx, mbr.reader, mbr.read_buffer)) |segment| {
                                                        try parse_ctx.feedDTDAttlistDefaultDeclValueSegment(segment);
                                                    }
                                                } else {
                                                    const range = parse_helper.nextTokenFullStrOrRange(tokenizer, str_ctx, MaybeReader, .{
                                                        .mbr = .{
                                                            .reader = {},
                                                            .read_buffer = {},
                                                        },
                                                        .src_buffer = {},
                                                    }) catch |e| switch (e) {};
                                                    try parse_ctx.feedDTDAttlistDefaultDeclValueSegment(range);
                                                }
                                            },
                                        };

                                        switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                            else => return ParseError.UnexpectedDTDToken,
                                            .eof => return ParseError.UnclosedDTDAttlist,
                                            .angle_bracket_right => break,
                                        }
                                        try parse_ctx.feedDTDAttlistDefaultDeclValueEnd();
                                    }
                                    try parse_ctx.feedDTDAttlistEnd();
                                },
                                .@"<!NOTATION" => {
                                    switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                        else => return ParseError.UnexpectedDTDToken,
                                        .eof => return ParseError.UnclosedDTDAttlist,
                                        .tag_whitespace => unreachable,
                                        .tag_token => {},
                                    }
                                    mbr_and_src.clearSrcBuffer();
                                    const name = try parse_helper.nextTokenFullStrOrRange(tokenizer, .dtd, MaybeReader, mbr_and_src);

                                    switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                        else => return ParseError.MissingDTDSpacing,
                                        .eof => return ParseError.UnclosedDTD,
                                        .tag_whitespace => {},
                                    }
                                    switch (try parse_helper.skipWhitespaceTokenSrc(tokenizer, .dtd, MaybeReader, mbr)) {
                                        .all_whitespace => {},
                                        .non_whitespace => unreachable,
                                    }

                                    switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                        else => return ParseError.UnexpectedDTDToken,
                                        .eof => return ParseError.UnclosedDTDAttlist,
                                        .tag_whitespace => unreachable,
                                        .tag_token => {},
                                    }

                                    const ext_id_kind, //
                                    const pubid_lit, //
                                    const system_lit, //
                                    const tt_after_str //
                                    = try parse_helper.parseDTDExternalIdParts(tokenizer, MaybeReader, mbr_and_src);
                                    try parse_ctx.feedDTDNotation(name, ext_id_kind, pubid_lit, system_lit);

                                    const non_whitespace_tt = switch (tt_after_str) {
                                        .tag_whitespace => tt: {
                                            switch (try parse_helper.skipWhitespaceTokenSrc(tokenizer, .dtd, MaybeReader, mbr)) {
                                                .all_whitespace => {},
                                                .non_whitespace => unreachable,
                                            }
                                            break :tt try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr);
                                        },
                                        else => |tt| tt,
                                    };
                                    switch (non_whitespace_tt) {
                                        else => return ParseError.UnexpectedDTDToken,
                                        .eof => return ParseError.UnclosedDTDNotation,
                                        .tag_whitespace => unreachable,
                                        .angle_bracket_right => {},
                                    }
                                },
                            }
                        },
                    },

                    .angle_bracket_right => |tag| tag,
                };

                switch (quartary_tt) {
                    else => return ParseError.UnexpectedDTDToken,
                    .eof => return ParseError.UnclosedDTD,
                    .angle_bracket_right => {},
                }
            },

            else => unreachable,
        }
    }
}

const parse_helper = struct {
    fn nextTokenType(
        tokenizer: *Tokenizer,
        context: Tokenizer.Context,
        comptime MaybeReader: ?type,
        mbr: MaybeBufferedReader(MaybeReader),
    ) !Tokenizer.TokenType {
        const reader = mbr.reader;
        const read_buffer = mbr.read_buffer;

        if (MaybeReader == null) {
            return tokenizer.nextTypeNoUnderrun(context);
        }
        return tokenizer.nextTypeStream(context) catch while (true) {
            const bytes_read = try reader.read(read_buffer);
            if (bytes_read != 0) {
                tokenizer.feedInput(read_buffer[0..bytes_read]);
            } else {
                tokenizer.feedEof();
            }
            break tokenizer.nextTypeStream(context) catch continue;
        };
    }

    fn nextTokenSegment(
        tokenizer: *Tokenizer,
        context: Tokenizer.Context,
        reader: anytype,
        read_buffer: []u8,
    ) !?[]const u8 {
        return tokenizer.nextSrcStream(context) catch while (true) {
            const bytes_read = try reader.read(read_buffer);
            if (bytes_read != 0) {
                tokenizer.feedInput(read_buffer[0..bytes_read]);
            } else {
                tokenizer.feedEof();
            }
            break tokenizer.nextSrcStream(context) catch continue;
        };
    }

    fn nextTokenSrcAsEnum(
        tokenizer: *Tokenizer,
        context: Tokenizer.Context,
        comptime MaybeReader: ?type,
        mbr: MaybeBufferedReader(MaybeReader),
        comptime E: type,
    ) !?E {
        var str: std.BoundedArray(u8, blk: {
            var longest = 0;
            const fields = @typeInfo(E).Enum.fields;
            @setEvalBranchQuota(fields.len);
            for (fields) |field| longest = @max(longest, field.name.len);
            break :blk longest;
        }) = .{};

        if (MaybeReader != null) {
            while (try parse_helper.nextTokenSegment(tokenizer, context, mbr.reader, mbr.read_buffer)) |segment| {
                str.appendSlice(segment) catch {
                    try skipTokenStr(tokenizer, context, MaybeReader, mbr);
                    return null;
                };
            }
        } else {
            const range = tokenizer.nextSrcNoUnderrun(context);
            str.appendSlice(range.toStr(tokenizer.src)) catch return null;
        }

        return std.meta.stringToEnum(E, str.constSlice());
    }

    fn nextTokenFullStrOrRange(
        tokenizer: *Tokenizer,
        context: Tokenizer.Context,
        comptime MaybeReader: ?type,
        mbr_and_src: MBRAndSrcBuf(MaybeReader),
    ) //
    if (MaybeReader) |Reader|
        (error{ SrcStaticBufferTooSmall, SrcDynamicBufferOutOfMemory } || Reader.Error)![]const u8
    else
        error{}!Tokenizer.Range //
    {
        if (MaybeReader == null) {
            return tokenizer.nextSrcNoUnderrun(context);
        }

        const src_buffer = mbr_and_src.src_buffer;

        const mbr = mbr_and_src.mbr;
        const reader = mbr.reader;
        const read_buffer = mbr.read_buffer;

        const start = switch (src_buffer) {
            inline else => |buf| buf.items.len,
        };
        while (true) {
            const maybe_str = tokenizer.nextSrcStream(context) catch {
                const bytes_read = try reader.read(read_buffer);
                if (bytes_read != 0) {
                    tokenizer.feedInput(read_buffer[0..bytes_read]);
                } else {
                    tokenizer.feedEof();
                }
                continue;
            };
            const str = maybe_str orelse break;
            switch (src_buffer) {
                .static => |static| if (str.len <= static.unusedCapacitySlice().len) {
                    static.appendSliceAssumeCapacity(str);
                } else return error.SrcStaticBufferTooSmall,
                .dyn => |dyn| dyn.appendSlice(str) catch return error.SrcDynamicBufferOutOfMemory,
            }
        }
        return switch (src_buffer) {
            inline else => |buf| buf.items[start..],
        };
    }

    fn skipWhitespaceTokenSrc(
        tokenizer: *Tokenizer,
        context: Tokenizer.Context,
        comptime MaybeReader: ?type,
        mbr: MaybeBufferedReader(MaybeReader),
    ) !enum { all_whitespace, non_whitespace } {
        var any_non_whitespace = false;
        if (MaybeReader != null) {
            while (try parse_helper.nextTokenSegment(tokenizer, context, mbr.reader, mbr.read_buffer)) |str| {
                if (std.mem.indexOfNone(u8, str, Tokenizer.whitespace_set) == null) continue;
                any_non_whitespace = false;
            }
        } else {
            const range = tokenizer.nextSrcNoUnderrun(context);
            any_non_whitespace = std.mem.indexOfNone(u8, range.toStr(tokenizer.src), Tokenizer.whitespace_set) != null;
        }
        return if (any_non_whitespace) .non_whitespace else .all_whitespace;
    }

    fn skipTokenStr(
        tokenizer: *Tokenizer,
        context: Tokenizer.Context,
        comptime MaybeReader: ?type,
        mbr: MaybeBufferedReader(MaybeReader),
    ) !void {
        if (MaybeReader == null) {
            _ = tokenizer.nextSrcNoUnderrun(context);
            return;
        }
        while (try parse_helper.nextTokenSegment(tokenizer, context, mbr.reader, mbr.read_buffer)) |_| {}
    }

    pub fn parseDTDElementCpQuantity(
        tokenizer: *Tokenizer,
        comptime MaybeReader: ?type,
        mbr: MaybeBufferedReader(MaybeReader),
    ) !struct { ?ContentParticleQuantity, Tokenizer.TokenType } {
        return switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
            else => |tag| .{ null, tag },
            .tag_whitespace => .{ null, try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr) },
            .qmark, .asterisk, .plus => |symbol| quant: {
                const quantity: ContentParticleQuantity = switch (symbol) {
                    .qmark => .none_or_one,
                    .asterisk => .none_or_many,
                    .plus => .one_or_many,
                    else => unreachable,
                };
                break :quant switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                    .tag_whitespace => switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                        .tag_whitespace => unreachable,
                        else => |tag| .{ quantity, tag },
                    },
                    else => |tag| .{ quantity, tag },
                };
            },
        };
    }

    fn parseDTDExternalIdParts(
        tokenizer: *Tokenizer,
        comptime MaybeReader: ?type,
        mbr_and_src: MBRAndSrcBuf(MaybeReader),
    ) ((if (MaybeReader) |Reader| Reader.Error else error{}) || ParseError)!struct {
        ExternalIdKind,
        ?if (MaybeReader != null) []const u8 else Tokenizer.Range,
        ?if (MaybeReader != null) []const u8 else Tokenizer.Range,
        Tokenizer.TokenType,
    } {
        const mbr = mbr_and_src.mbr;

        const ext_id_kind: ExternalIdKind = (try parse_helper.nextTokenSrcAsEnum(
            tokenizer,
            .dtd,
            MaybeReader,
            mbr,
            ExternalIdKind,
        )) orelse return ParseError.UnexpectedDTDToken;

        const pubid_lit: ?if (MaybeReader != null) []const u8 else Tokenizer.Range = switch (ext_id_kind) {
            .SYSTEM => null,
            .PUBLIC => lit: {
                switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                    else => return ParseError.MissingDTDSpacing,
                    .eof => return ParseError.UnclosedDTD,
                    .tag_whitespace => {},
                }
                switch (try parse_helper.skipWhitespaceTokenSrc(tokenizer, .dtd, MaybeReader, mbr)) {
                    .all_whitespace => {},
                    .non_whitespace => unreachable,
                }

                const open_quote = switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                    else => return ParseError.UnexpectedDTDToken,
                    .eof => return ParseError.UnclosedDTD,
                    .quote_single,
                    .quote_double,
                    => |open_quote| open_quote,
                };
                const str_ctx: Tokenizer.Context = switch (open_quote) {
                    .quote_single => .system_literal_quote_single,
                    .quote_double => .system_literal_quote_double,
                    else => unreachable,
                };

                switch (try parse_helper.nextTokenType(tokenizer, str_ctx, MaybeReader, mbr)) {
                    .quote_single,
                    .quote_double,
                    => |close_quote| {
                        assert(open_quote == close_quote);
                        break :lit if (MaybeReader != null) "" else .{ .start = 0, .end = 0 };
                    },
                    .text_data => {},
                    else => unreachable,
                }
                const pubid_lit = try parse_helper.nextTokenFullStrOrRange(tokenizer, str_ctx, MaybeReader, mbr_and_src);
                switch (try parse_helper.nextTokenType(tokenizer, str_ctx, MaybeReader, mbr)) {
                    .quote_single,
                    .quote_double,
                    => |close_quote| assert(open_quote == close_quote),
                    .eof => return ParseError.UnclosedPubidLiteral,
                    else => unreachable,
                }

                const pubid_lit_str = if (MaybeReader != null) pubid_lit else pubid_lit.toStr(tokenizer.src);
                for (pubid_lit_str) |pubid_char| switch (pubid_char) {
                    '\u{20}', '\u{D}', '\u{A}', 'a'...'z', 'A'...'Z', '0'...'9', '-' => {},
                    '\'' => assert(open_quote != .quote_single),
                    '(', ')', '+', ',', '.', '/', ':', '=', '?', ';', '!', '*', '#', '@', '$', '_', '%' => {},
                    else => return ParseError.InvalidPubidLiteral,
                };

                break :lit pubid_lit;
            },
        };
        const system_lit: ?if (MaybeReader != null) []const u8 else Tokenizer.Range, //
        const tt_after_str: Tokenizer.TokenType //
        = lit: {
            switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                else => |tag| break :lit .{ null, tag },
                .tag_whitespace => {},
            }
            switch (try parse_helper.skipWhitespaceTokenSrc(tokenizer, .dtd, MaybeReader, mbr)) {
                .all_whitespace => {},
                .non_whitespace => unreachable,
            }
            const open_quote = switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                else => return ParseError.UnexpectedDTDToken,
                .eof => return ParseError.UnclosedDTD,
                .quote_single,
                .quote_double,
                => |open_quote| open_quote,
            };
            const str_ctx: Tokenizer.Context = switch (open_quote) {
                .quote_single => .system_literal_quote_single,
                .quote_double => .system_literal_quote_double,
                else => unreachable,
            };

            switch (try parse_helper.nextTokenType(tokenizer, str_ctx, MaybeReader, mbr)) {
                .quote_single,
                .quote_double,
                => |close_quote| {
                    assert(open_quote == close_quote);
                    break :lit .{
                        if (MaybeReader != null) "" else .{ .start = 0, .end = 0 },
                        try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr),
                    };
                },
                .text_data => {},
                else => unreachable,
            }

            const system_lit = try parse_helper.nextTokenFullStrOrRange(tokenizer, str_ctx, MaybeReader, mbr_and_src);
            switch (try parse_helper.nextTokenType(tokenizer, str_ctx, MaybeReader, mbr)) {
                .quote_single,
                .quote_double,
                => |close_quote| assert(open_quote == close_quote),
                .eof => return ParseError.UnclosedSystemLiteral,
                else => unreachable,
            }

            break :lit .{ system_lit, try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr) };
        };

        return .{ ext_id_kind, pubid_lit, system_lit, tt_after_str };
    }

    fn handlePi(comptime Impl: type, parse_ctx: ParseCtx(Impl), tokenizer: *Tokenizer, comptime MaybeReader: ?type, mbr_and_src: MBRAndSrcBuf(MaybeReader)) !void {
        const mbr = mbr_and_src.mbr;
        switch (try parse_helper.nextTokenType(tokenizer, .pi, MaybeReader, mbr)) {
            .text_data => {
                const pi_data = try parse_helper.nextTokenFullStrOrRange(tokenizer, .pi, MaybeReader, mbr_and_src);
                try parse_ctx.feedPI(pi_data);
                switch (try parse_helper.nextTokenType(tokenizer, .pi, MaybeReader, mbr)) {
                    .pi_end => {},
                    .eof => return ParseError.UnclosedPI,
                    else => unreachable,
                }
            },
            .pi_end => return ParseError.EmptyPI,
            .eof => return ParseError.UnclosedPI,
            else => unreachable,
        }
    }

    fn handleCommentSkip(tokenizer: *Tokenizer, comptime MaybeReader: ?type, mbr: MaybeBufferedReader(MaybeReader)) !void {
        while (true) switch (try parse_helper.nextTokenType(tokenizer, .comment, MaybeReader, mbr)) {
            .text_data => try parse_helper.skipTokenStr(tokenizer, .comment, MaybeReader, mbr),
            .invalid_comment_dash_dash => return ParseError.CommentDashDash,
            .invalid_comment_end_triple_dash => return ParseError.CommentEndTripleDash,
            .comment_end => break,
            else => unreachable,
        };
    }
};

pub const IgnoreCtx = struct {
    pub fn feedPI(
        ctx: @This(),
        data: anytype,
    ) !void {
        _ = ctx;
        _ = data;
    }

    pub fn feedDTDName(
        ctx: @This(),
        name: anytype,
    ) !void {
        _ = ctx;
        _ = name;
    }

    pub fn feedDTDExternalId(
        ctx: @This(),
        kind: ExternalIdKind,
        pubid_lit: anytype,
        system_lit: anytype,
    ) !void {
        _ = ctx;
        _ = kind;
        _ = pubid_lit;
        _ = system_lit;
    }

    pub fn feedDTDEntityStart(
        ctx: @This(),
        kind: ReferenceKind,
        name: anytype,
    ) !void {
        _ = ctx;
        _ = kind;
        _ = name;
    }

    pub fn feedDTDEntityValueTextSegment(
        ctx: @This(),
        text: anytype,
    ) !void {
        _ = ctx;
        _ = text;
    }

    pub fn feedDTDEntityValueReference(
        ctx: @This(),
        kind: ReferenceKind,
        id: anytype,
    ) !void {
        _ = ctx;
        _ = kind;
        _ = id;
    }

    pub fn feedDTDEntityValueEnd(ctx: @This()) !void {
        _ = ctx;
    }

    pub fn feedDTDEntityValueExternalId(
        ctx: @This(),
        kind: ExternalIdKind,
        pubid_lit: anytype,
        system_lit: anytype,
        ndata_name: anytype,
    ) !void {
        _ = ctx;
        _ = kind;
        _ = pubid_lit;
        _ = system_lit;
        _ = ndata_name;
    }

    pub fn feedDTDElementStart(
        ctx: @This(),
        name: anytype,
    ) !void {
        _ = ctx;
        _ = name;
    }

    pub fn feedDTDElementEmptyOrAny(ctx: @This(), kind: EmptyOrAny) !void {
        _ = ctx;
        _ = kind;
    }

    pub fn feedDTDElementIdentifier(
        ctx: @This(),
        name: anytype,
        quantity: ?ContentParticleQuantity,
    ) !void {
        _ = ctx;
        _ = name;
        _ = quantity;
    }

    pub fn feedDTDElementLParen(ctx: @This()) !void {
        _ = ctx;
    }

    pub fn feedDTDElementRParen(ctx: @This(), quantity: ?ContentParticleQuantity, end: bool) !void {
        _ = ctx;
        _ = quantity;
        _ = end;
    }

    pub fn feedDTDElementPCData(ctx: @This()) !void {
        _ = ctx;
    }

    pub fn feedDTDElementChoiceSep(ctx: @This()) !void {
        _ = ctx;
    }

    pub fn feedDTDElementSequenceSep(ctx: @This()) !void {
        _ = ctx;
    }

    pub fn feedDTDAttlistStart(
        ctx: @This(),
        name: anytype,
    ) !void {
        _ = ctx;
        _ = name;
    }

    pub fn feedDTDAttlistDefStart(
        ctx: @This(),
        name: anytype,
        attr_type: AttributeType,
    ) !void {
        _ = ctx;
        _ = name;
        _ = attr_type;
    }

    pub fn feedDTDAttlistDefNmtoken(
        ctx: @This(),
        name: anytype,
    ) !void {
        _ = ctx;
        _ = name;
    }

    pub fn feedDTDAttlistDefaultDeclStart(ctx: @This(), kind: ?DefaultDeclKind) !void {
        _ = ctx;
        _ = kind;
    }

    pub fn feedDTDAttlistDefaultDeclValueSegment(
        ctx: @This(),
        text: anytype,
    ) !void {
        _ = ctx;
        _ = text;
    }

    pub fn feedDTDAttlistDefaultDeclValueReference(
        ctx: @This(),
        name: anytype,
    ) !void {
        _ = ctx;
        _ = name;
    }

    pub fn feedDTDAttlistDefaultDeclValueEnd(ctx: @This()) !void {
        _ = ctx;
    }

    pub fn feedDTDAttlistEnd(ctx: @This()) !void {
        _ = ctx;
    }

    pub fn feedDTDNotation(
        ctx: @This(),
        name: anytype,
        ext_id_kind: ExternalIdKind,
        sys_literal: anytype,
        pubid_literal: anytype,
    ) !void {
        _ = ctx;
        _ = name;
        _ = ext_id_kind;
        _ = sys_literal;
        _ = pubid_literal;
    }
};

test parseSlice {
    var tokenizer = Tokenizer.initComplete(
        \\<?xml version="1.0" encoding="UTF-8"?>
        \\
        \\<!DOCTYPE foo [
        \\ <?phi?>
        \\ <!-- foo -->
        \\ <!ELEMENT bar EMPTY>
        \\ <!ELEMENT baz (#PCDATA)*>
        \\ <!ATTLIST foo fizz CDATA #IMPLIED>
        \\ <!ATTLIST foo fizz NOTATION (eao|oae|aeo) #FIXED "eao">
        \\ <!ATTLIST foo fizz NOTATION (eao|oae|aeo) "eao">
        \\ <!ENTITY lorem "ipsum">
        \\ <!ENTITY % lorem "ipsum">
        \\ <!ENTITY lorem SYSTEM "c">
        \\ <!ENTITY % lorem SYSTEM "c">
        \\ <!NOTATION eee SYSTEM "d">
        \\ <!NOTATION eee PUBLIC "e">
        \\ <!NOTATION fff PUBLIC "f" "g">
        \\ <!NOTATION ggg SYSTEM "g">
        \\]>
    );
    try std.testing.expectEqual(.eof, parseSlice(&tokenizer, IgnoreCtx{}));
}
