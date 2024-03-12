const std = @import("std");
const assert = std.debug.assert;

const iksemel = @import("iksemel.zig");
const Tokenizer = iksemel.Tokenizer;
const StrBuffer = iksemel.StrBuffer;

const parse_helper = @import("parse_helper.zig");

pub const ExternalIdKind = enum { SYSTEM, PUBLIC };

pub const ReferenceKind = enum {
    /// Represents a General Entity reference ('&name;').
    general,
    /// Represents a Parsed Entity reference ('%name;').
    parsed,
};

pub const ContentSpec = enum {
    EMPTY,
    ANY,
    Mixed,
    children,
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

pub const ElementChildToken = enum {
    lparen,
    rparen,
    comma,
    pipe,
    name,
    qmark,
    asterisk,
    plus,
};

pub fn ParseCtx(comptime Inner: type) type {
    return struct {
        inner: Inner,
        const Self = @This();

        /// Called consecutively to construct the source.
        /// Terminated by `feedSrcEnd`.
        pub fn feedSrcSegment(
            ctx: Self,
            /// * `[]const u8`
            /// * `Tokenizer.Range`
            segment: anytype,
        ) !void {
            switch (@TypeOf(segment)) {
                []const u8, Tokenizer.Range => {},
                else => @compileError(
                    "" ++
                        "Expected one of " ++
                        @typeName([]const u8) ++
                        " or " ++
                        @typeName(Tokenizer.Range) ++
                        ", instead got " ++
                        @typeName(@TypeOf(segment)),
                ),
            }
            return ctx.inner.feedSrcSegment(segment);
        }
        pub fn feedSrcEnd(ctx: Self) !void {
            return ctx.inner.feedSrcEnd();
        }

        /// Followed by `feedSrc*`, in order to construct the reference
        /// id/name token source, and then by `feedReferenceEnd`.
        pub fn feedReferenceStart(ctx: Self, kind: ReferenceKind) !void {
            return ctx.inner.feedReferenceStart(kind);
        }
        pub fn feedReferenceEnd(ctx: Self) !void {
            return ctx.inner.feedReferenceEnd();
        }

        /// Followed by `feedSrc*`, in order to feed the Processing
        /// Instructions (parsing of the PI target and data are
        /// left to the context).
        pub fn feedPI(ctx: Self) !void {
            return ctx.inner.feedPI();
        }

        /// Followed by `feedSrc*`, in order to construct the DTD name.
        /// Afterwards, may be followed by one of:
        /// * `feedDTDExternalId`
        /// * `feedDTDEntityStart`
        /// * `feedDTDEnd`
        pub fn feedDTDStart(ctx: Self) !void {
            return ctx.inner.feedDTDStart();
        }

        /// If `kind == .PUBLIC`, this is followed by `feedSrc*` twice,
        /// first for the pubid literal, then for the system literal.
        ///
        /// If `kind == .SYSTEM`, this is followed by `feedSrc*` once,
        /// for the system literal.
        ///
        /// After either of the above, this may be followed by one of:
        /// * `feedDTDEntityStart`
        /// * `feedDTDEnd`
        pub fn feedDTDExternalId(ctx: Self, kind: ExternalIdKind) !void {
            return ctx.inner.feedDTDExternalId(kind);
        }

        /// If `kind == .general`, this may be followed by one of the
        /// following sequences:
        /// * `(feedSrc* | feedReference)*`, for a simple entity value.
        /// * `feedDTDExternalId feedDTDNDataDecl?`
        ///
        /// If `kind == .parsed`, this may be followed by one of the
        /// following sequences:
        /// * `(feedSrc* | feedReference)*`, for a simple entity value.
        /// * `feedDTDExternalId`
        ///
        /// After either of the above, this will be followed by `feedDTDEntityEnd`.
        pub fn feedDTDEntityStart(ctx: Self, kind: ReferenceKind) !void {
            return ctx.inner.feedDTDEntityStart(kind);
        }
        pub fn feedDTDEntityEnd(ctx: Self) !void {
            return ctx.inner.feedDTDEntityEnd();
        }
        /// Followed by `feedSrc*`, in order to construct the NDataDecl name.
        pub fn feedDTDNDataDecl(ctx: Self) !void {
            return ctx.feedDTDNDataDecl();
        }

        /// Followed by `feedSrc*`, in order to construct the element name.
        /// Afterwards, it will be followed by a call to `feedDTDElementContentSpec`.
        pub fn feedDTDElementStart(ctx: Self) !void {
            return ctx.inner.feedDTDElementStart();
        }
        /// If `content_spec == .EMPTY` or `content_spec == .ANY`, this is
        /// followed by `feedDTDElementEnd`.
        ///
        /// If `content_spec == .Mixed`, this is potentially followed by a
        /// series of calls to `feedSrc*` for each name that appears in the
        /// listing. Afterwards, it is followed by a call to `feedDTDElementMixedEnd`.
        pub fn feedDTDElementContentSpec(ctx: Self, content_spec: ContentSpec) !void {
            return ctx.inner.feedDTDElementContentSpec(content_spec);
        }

        /// If `zero_or_many`, the listing matches: '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*'.
        /// If `!zero_or_many`, the listing matches '(' S? '#PCDATA' S? ')'.
        /// Followed by a call to `feedDTDElementEnd`.
        pub fn feedDTDElementMixedEnd(ctx: Self, zero_or_many: bool) !void {
            return ctx.inner.feedDTDElementMixedEnd(zero_or_many);
        }

        /// If `child_tok == .name`, this is followed by `feedSrc*`, and then afterwards
        /// regardless of `child_tok`, what follows is a call to `feedElementEnd`.
        pub fn feedDTDElementChildToken(ctx: Self, child_tok: ElementChildToken) !void {
            return ctx.inner.feedDTDElementChildToken(child_tok);
        }

        pub fn feedDTDElementEnd(ctx: Self) !void {
            return ctx.inner.feedDTDElementEnd();
        }

        /// Followed by `feedSrc*`, in order to construct the attribute list name.
        /// Afterwards, it will be followed by a series of consecutive calls to
        /// `feedDTDAttlistDef`, terminated by  a call to `feedDTDAttlistEnd`.
        pub fn feedDTDAttlistStart(ctx: Self) !void {
            return ctx.inner.feedDTDAttlistStart();
        }

        /// Followed by `feedSrc*`, in order to construct the attribute defintion name.
        /// Afterwards, it will be followed by a call to `feedDTDAttlistDefType`.
        pub fn feedDTDAttlistDef(ctx: Self) !void {
            return ctx.inner.feedDTDAttlistDef();
        }

        /// If `attr_type == .NOTATION or attr_type == .enumeration`, this will
        /// be followed by a series of calls to `feedSrc*`, for the list of
        /// potential Name/Nmtokens.
        /// Followed by a call to `feedDTDAttlistDefaultDeclStart`.
        pub fn feedDTDAttlistDefType(ctx: Self, attr_type: AttributeType) !void {
            return ctx.inner.feedDTDAttlistDefType(attr_type);
        }

        /// If `default_decl_kind == null or default_decl_kind.? == .FIXED`, this
        /// will be followed by a sequence of interspersed calls to `feedSrc*` and
        /// `feedReference` for the attribute value. This will be followed
        /// by a call to `feedDTDAttlistDefaultDeclEnd`.
        pub fn feedDTDAttlistDefaultDeclStart(ctx: Self, default_decl_kind: ?DefaultDeclKind) !void {
            return ctx.inner.feedDTDAttlistDefaultDeclStart(default_decl_kind);
        }

        /// This will be followed by a call to `feedDTDAttlistDef` or `feedDTDAttlistEnd`.
        pub fn feedDTDAttlistDefaultDeclEnd(ctx: Self) !void {
            return ctx.inner.feedDTDAttlistDefaultDeclEnd();
        }

        pub fn feedDTDAttlistEnd(ctx: Self) !void {
            return ctx.inner.feedDTDAttlistEnd();
        }

        /// Followed by `feedSrc*` in order to  construct the Notation declaration name.
        /// Afterwards, it will be followed by a call to `feedExternalOrPublicId`.
        pub fn feedDTDNotationStart(ctx: Self) !void {
            return ctx.inner.feedDTDNotationStart();
        }

        /// If `kind == .SYSTEM`, this is followed by `feedSrc*` once,
        /// for the system literal.
        ///
        /// If `kind == .PUBLIC`, this is followed by `feedSrc*` at least once,
        /// and then optionally a second time.
        ///
        /// After either of the above, this will be followed by `feedDTDNotationEnd`.
        pub fn feedExternalOrPublicId(ctx: Self, kind: ExternalIdKind) !void {
            return ctx.inner.feedDTDExternalOrPublicId(kind);
        }

        pub fn feedDTDNotationEnd(ctx: Self) !void {
            return ctx.inner.feedDTDNotationEnd();
        }

        /// Ends the DTD Declaration.
        pub fn feedDTDEnd(ctx: Self) !void {
            return ctx.inner.feedDTDEnd();
        }
    };
}

pub const ParseError = error{
    NonWhitespaceInProlog,
    AngleBracketLeftBang,
    AngleBracketLeftInAttribute,

    UnclosedPI,
    EmptyPI,

    InvalidCommentStart,
    CommentDashDash,
    CommentEndTripleDash,

    InvalidDTDStart,
    UnexpectedDTDToken,
    MissingDTDSpacing,

    UnclosedDTDPubidLiteral,
    InvalidDTDPubidLiteral,
    UnclosedDTDSystemLiteral,
    InvalidDTDDecl,
    UnclosedDTDEntityValue,
    UnclosedAttributeValue,
    MissingAsteriskForManyPCDATAOptions,

    InvalidReferenceEnd,
    EmptyReference,

    UnclosedDTD,
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
    return parseUntilLABReaderOrSlice(parse_ctx_impl, tokenizer, null, .{
        .reader = {},
        .read_buffer = {},
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
    return parseUntilLABReaderOrSlice(parse_ctx_impl, &tokenizer, @TypeOf(reader), .{
        .mbr = .{
            .reader = reader,
            .read_buffer = read_buffer,
        },
        .src_buffer = src_buffer,
    });
}

fn parseUntilLABReaderOrSlice(
    parse_ctx_impl: anytype,
    tokenizer: *Tokenizer,
    comptime MaybeReader: ?type,
    mbr: parse_helper.MaybeBufferedReader(MaybeReader),
) !Tokenizer.TokenType {
    const Impl = @TypeOf(parse_ctx_impl);
    const parse_ctx: ParseCtx(Impl) = .{ .inner = parse_ctx_impl };

    while (true) switch (try parse_helper.nextTokenType(tokenizer, .non_markup, MaybeReader, mbr)) {
        .eof, .angle_bracket_left => |tag| return tag,

        .pi_start => try handlePi(Impl, parse_ctx, tokenizer, MaybeReader, mbr),

        .dtd_start => {
            try handleDTD(Impl, parse_ctx, tokenizer, MaybeReader, mbr);
            try parse_ctx.feedDTDEnd();
        },

        .comment_start => switch (try parse_helper.handleCommentSkip(tokenizer, MaybeReader, mbr)) {
            .normal_end => {},
            .invalid_end_triple_dash => return ParseError.CommentEndTripleDash,
            .invalid_dash_dash => return ParseError.CommentDashDash,
        },

        .text_data => switch (try parse_helper.skipWhitespaceTokenSrc(tokenizer, .non_markup, MaybeReader, mbr)) {
            .all_whitespace => {},
            .non_whitespace => return ParseError.NonWhitespaceInProlog,
        },

        .ampersand => return ParseError.NonWhitespaceInProlog,
        .cdata_start => return ParseError.NonWhitespaceInProlog,
        .invalid_angle_bracket_left_bang => return ParseError.AngleBracketLeftBang,
        .invalid_comment_start_single_dash => return ParseError.InvalidCommentStart,

        .invalid_dtd_start => {
            try parse_helper.skipTokenStr(tokenizer, .non_markup, MaybeReader, mbr);
            return ParseError.InvalidDTDStart;
        },

        else => unreachable,
    };
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
    while (try iter.next(tokenizer, context, mbr)) |segment| {
        try parse_ctx.feedSrcSegment(segment);
    }
    try parse_ctx.feedSrcEnd();
}

fn handlePi(
    comptime Impl: type,
    parse_ctx: ParseCtx(Impl),
    tokenizer: *Tokenizer,
    comptime MaybeReader: ?type,
    mbr: parse_helper.MaybeBufferedReader(MaybeReader),
) !void {
    switch (try parse_helper.nextTokenType(tokenizer, .pi, MaybeReader, mbr)) {
        else => unreachable,
        .eof => return ParseError.UnclosedPI,
        .pi_end => return ParseError.EmptyPI,
        .text_data => {},
    }
    try parse_ctx.feedPI();
    try feedTokenSrc(Impl, parse_ctx, tokenizer, .pi, MaybeReader, mbr);
    switch (try parse_helper.nextTokenType(tokenizer, .pi, MaybeReader, mbr)) {
        else => unreachable,
        .eof => return ParseError.UnclosedPI,
        .text_data => unreachable,
        .pi_end => {},
    }
}

fn handleDTD(
    comptime Impl: type,
    parse_ctx: ParseCtx(Impl),
    tokenizer: *Tokenizer,
    comptime MaybeReader: ?type,
    mbr: parse_helper.MaybeBufferedReader(MaybeReader),
) !void {
    if (try parse_helper.expectAndSkipIfTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
        else => return ParseError.MissingDTDSpacing,
        .eof => return ParseError.UnclosedDTD,
        .tag_whitespace => unreachable,
    };

    switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
        else => return ParseError.UnexpectedDTDToken,
        .tag_whitespace => unreachable,
        .tag_token => {},
    }
    try parse_ctx.feedDTDStart();
    try feedTokenSrc(Impl, parse_ctx, tokenizer, .dtd, MaybeReader, mbr);

    const maybe_internal_subset: bool = switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
        else => return ParseError.UnexpectedDTDToken,

        // '<!DOCTYPE' Name S?'>'
        // This will never be a valid document if the DTD is respected,
        // but it can be correctly parsed.
        .angle_bracket_right => return,

        .square_bracket_left => true,

        // ExternalId
        .tag_token => ext_id: {
            const maybe_ext_id_kind = try parse_helper.nextTokenSrcAsEnum(tokenizer, .dtd, MaybeReader, mbr, ExternalIdKind);
            const ext_id_kind = maybe_ext_id_kind orelse return ParseError.UnexpectedDTDToken;
            try parse_ctx.feedDTDExternalId(ext_id_kind);
            assert((try handleExternalOrPublicId(Impl, parse_ctx, tokenizer, MaybeReader, mbr, ext_id_kind, .require_system_literal)) == null);
            break :ext_id false;
        },
    };

    if (!maybe_internal_subset) switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
        else => return ParseError.UnexpectedDTDToken,
        .eof => return ParseError.UnclosedDTD,
        .angle_bracket_right => return,
        .square_bracket_left => {},
    };

    while (true) switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
        else => return ParseError.UnexpectedDTDToken,
        .tag_whitespace => unreachable,
        .square_bracket_right => break,

        .pi_start => try handlePi(Impl, parse_ctx, tokenizer, MaybeReader, mbr),

        .dtd_decl => {
            const DeclStr = enum {
                @"<!ENTITY",
                @"<!ELEMENT",
                @"<!ATTLIST",
                @"<!NOTATION",
            };
            const maybe_decl_str = try parse_helper.nextTokenSrcAsEnum(tokenizer, .dtd, MaybeReader, mbr, DeclStr);
            const decl_str = maybe_decl_str orelse return ParseError.InvalidDTDDecl;

            if (try parse_helper.expectAndSkipIfTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
                else => return ParseError.MissingDTDSpacing,
                .eof => return switch (decl_str) {
                    .@"<!ENTITY" => ParseError.UnclosedDTDEntity,
                    .@"<!ELEMENT" => ParseError.UnclosedDTDElement,
                    .@"<!ATTLIST" => ParseError.UnclosedDTDAttlist,
                    .@"<!NOTATION" => ParseError.UnclosedDTDNotation,
                },
                .tag_whitespace => unreachable,
            };

            switch (decl_str) {
                .@"<!ENTITY" => {
                    try handleDTDEntity(Impl, parse_ctx, tokenizer, MaybeReader, mbr);
                    try parse_ctx.feedDTDEntityEnd();
                },
                .@"<!ELEMENT" => {
                    try handleDTDElement(Impl, parse_ctx, tokenizer, MaybeReader, mbr);
                    try parse_ctx.feedDTDElementEnd();
                },
                .@"<!ATTLIST" => {
                    try handleDTDAttlist(Impl, parse_ctx, tokenizer, MaybeReader, mbr);
                    try parse_ctx.feedDTDAttlistEnd();
                },
                .@"<!NOTATION" => {
                    try handleDTDNotation(Impl, parse_ctx, tokenizer, MaybeReader, mbr);
                    try parse_ctx.feedDTDNotationEnd();
                },
            }
        },

        .comment_start => switch (try parse_helper.handleCommentSkip(tokenizer, MaybeReader, mbr)) {
            .normal_end => {},
            .invalid_end_triple_dash => return ParseError.CommentEndTripleDash,
            .invalid_dash_dash => return ParseError.CommentDashDash,
        },

        .invalid_comment_start_single_dash => return ParseError.InvalidCommentStart,
    };

    switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
        else => return ParseError.UnexpectedDTDToken,
        .eof => return ParseError.UnclosedDTD,
        .angle_bracket_right => {},
    }
}

fn handleDTDEntity(
    comptime Impl: type,
    parse_ctx: ParseCtx(Impl),
    tokenizer: *Tokenizer,
    comptime MaybeReader: ?type,
    mbr: parse_helper.MaybeBufferedReader(MaybeReader),
) !void {
    const ref_kind: ReferenceKind = switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
        else => return ParseError.UnexpectedDTDToken,
        .eof => return ParseError.UnclosedDTDEntity,
        .tag_whitespace => unreachable,
        .tag_token => .general,
        .percent => rk: {
            if (try parse_helper.expectAndSkipIfTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
                else => return ParseError.MissingDTDSpacing,
                .eof => return ParseError.UnclosedDTDEntity,
                .tag_whitespace => unreachable,
            };
            switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                else => return ParseError.UnexpectedDTDToken,
                .eof => return ParseError.UnclosedDTDEntity,
                .tag_whitespace => unreachable,
                .tag_token => {},
            }
            break :rk .parsed;
        },
    };
    try parse_ctx.feedDTDEntityStart(ref_kind);
    try feedTokenSrc(Impl, parse_ctx, tokenizer, .dtd, MaybeReader, mbr); // the declared name

    if (try parse_helper.expectAndSkipIfTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
        else => return ParseError.MissingDTDSpacing,
        .eof => return ParseError.UnclosedDTDEntity,
        .tag_whitespace => unreachable,
    };

    switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
        else => return ParseError.UnexpectedDTDToken,
        .eof => return ParseError.UnclosedDTDEntity,
        .tag_whitespace => unreachable,

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
                .eof => return ParseError.UnclosedDTDEntityValue,
                .text_data => try feedTokenSrc(Impl, parse_ctx, tokenizer, str_ctx, MaybeReader, mbr),
                .percent, .ampersand => |tag| {
                    const ref_val_kind: ReferenceKind = switch (tag) {
                        .percent => .parsed,
                        .ampersand => .general,
                        else => unreachable,
                    };
                    try parse_ctx.feedReferenceStart(ref_val_kind);

                    switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
                        else => unreachable,
                        .invalid_reference_end => return ParseError.InvalidReferenceEnd,
                        .semicolon => return ParseError.EmptyReference,
                        .tag_token => {},
                    }
                    try feedTokenSrc(Impl, parse_ctx, tokenizer, .reference, MaybeReader, mbr);

                    switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
                        else => unreachable,
                        .invalid_reference_end => return ParseError.InvalidReferenceEnd,
                        .tag_token => unreachable,
                        .semicolon => {},
                    }
                    try parse_ctx.feedReferenceEnd();
                },
                .quote_single,
                .quote_double,
                => |close_quote| {
                    assert(open_quote == close_quote);
                    break;
                },
            };
        },

        .tag_token => {
            const maybe_ext_id_kind = try parse_helper.nextTokenSrcAsEnum(tokenizer, .dtd, MaybeReader, mbr, ExternalIdKind);
            const ext_id_kind = maybe_ext_id_kind orelse return ParseError.UnexpectedDTDToken;

            try parse_ctx.feedDTDExternalId(ext_id_kind);
            assert((try handleExternalOrPublicId(Impl, parse_ctx, tokenizer, MaybeReader, mbr, ext_id_kind, .require_system_literal)) == null);

            if (try parse_helper.expectAndSkipIfTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
                else => return ParseError.MissingDTDSpacing,
                .eof => return ParseError.UnclosedDTDEntity,
                .tag_whitespace => unreachable,
                .angle_bracket_right => return,
            };

            switch (ref_kind) {
                .general => {
                    switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                        else => return ParseError.UnexpectedDTDToken,
                        .eof => return ParseError.UnclosedDTDEntity,
                        .tag_whitespace => unreachable,
                        .angle_bracket_right => return,
                        .tag_token => {},
                    }
                    _ = (try parse_helper.nextTokenSrcAsEnum(tokenizer, .dtd, MaybeReader, mbr, enum { NDATA })) orelse {
                        return ParseError.UnexpectedDTDToken;
                    };
                    if (try parse_helper.expectAndSkipIfTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
                        else => return ParseError.MissingDTDSpacing,
                        .eof => return ParseError.UnclosedDTDEntity,
                        .tag_whitespace => unreachable,
                    };
                    switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                        else => return ParseError.UnexpectedDTDToken,
                        .eof => return ParseError.UnclosedDTDEntity,
                        .tag_token => {},
                    }

                    try parse_ctx.feedDTDNDataDecl();
                    try feedTokenSrc(Impl, parse_ctx, tokenizer, .dtd, MaybeReader, mbr);
                },
                .parsed => {},
            }
        },
    }

    switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
        else => return ParseError.UnexpectedDTDToken,
        .eof => return ParseError.UnclosedDTD,
        .angle_bracket_right => {},
    }
}

fn handleDTDElement(
    comptime Impl: type,
    parse_ctx: ParseCtx(Impl),
    tokenizer: *Tokenizer,
    comptime MaybeReader: ?type,
    mbr: parse_helper.MaybeBufferedReader(MaybeReader),
) !void {
    switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
        else => return ParseError.UnexpectedDTDToken,
        .eof => return ParseError.UnclosedDTDElement,
        .tag_token => {},
    }
    try parse_ctx.feedDTDElementStart();
    try feedTokenSrc(Impl, parse_ctx, tokenizer, .dtd, MaybeReader, mbr); // the declared name

    if (try parse_helper.expectAndSkipIfTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
        else => return ParseError.MissingDTDSpacing,
        .eof => return ParseError.UnclosedDTDElement,
        .tag_whitespace => unreachable,
    };

    switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
        else => return ParseError.UnexpectedDTDToken,
        .eof => return ParseError.UnclosedDTDElement,

        .tag_token => {
            const EmptyOrAny = enum { EMPTY, ANY };
            const maybe_empty_or_any = try parse_helper.nextTokenSrcAsEnum(tokenizer, .dtd, MaybeReader, mbr, EmptyOrAny);
            const empty_or_any = maybe_empty_or_any orelse return ParseError.UnexpectedDTDToken;
            try parse_ctx.feedDTDElementContentSpec(switch (empty_or_any) {
                .EMPTY => .EMPTY,
                .ANY => .ANY,
            });
            switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                else => return ParseError.UnexpectedDTDToken,
                .eof => return ParseError.UnclosedDTDElement,
                .angle_bracket_right => {},
            }
        },
        .lparen => switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
            else => return ParseError.UnexpectedDTDToken,
            .eof => return ParseError.UnclosedDTDElement,
            .hashtag => {
                switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                    else => return ParseError.UnexpectedDTDToken,
                    .eof => return ParseError.UnclosedDTDElement,
                    .tag_token => {},
                }
                _ = (try parse_helper.nextTokenSrcAsEnum(tokenizer, .dtd, MaybeReader, mbr, enum { PCDATA })) orelse {
                    return ParseError.UnexpectedDTDToken;
                };

                var many_opts = false;
                while (true) : (many_opts = true) {
                    switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                        else => return ParseError.UnexpectedDTDToken,
                        .eof => return ParseError.UnclosedDTDElement,
                        .rparen => break,
                        .pipe => {},
                    }

                    switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                        else => return ParseError.UnexpectedDTDToken,
                        .eof => return ParseError.UnclosedDTDElement,
                        .tag_token => {},
                    }

                    try feedTokenSrc(Impl, parse_ctx, tokenizer, .dtd, MaybeReader, mbr);
                }

                const many_opts_specifier = if (try parse_helper.expectAndSkipIfTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
                    else => return ParseError.UnexpectedDTDToken,
                    .tag_whitespace => unreachable,
                    .asterisk => true,
                    .angle_bracket_right => false,
                } else switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                    else => return ParseError.UnexpectedDTDToken,
                    .eof => return ParseError.UnclosedDTDElement,
                    .tag_whitespace => unreachable,
                    .angle_bracket_right => false,
                };

                if (many_opts and !many_opts_specifier) {
                    return ParseError.MissingAsteriskForManyPCDATAOptions;
                }
                if (many_opts_specifier) switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                    else => return ParseError.UnexpectedDTDToken,
                    .eof => return ParseError.UnclosedDTDElement,
                    .tag_whitespace => unreachable,
                    .angle_bracket_right => {},
                };
            },
            .lparen, .tag_token => |first_cached_tt| {
                var depth: u64 = 1;
                var prev: enum { lparen, rparen, name, comma, pipe } = .lparen;

                var cached_tt: ?Tokenizer.TokenType = first_cached_tt;
                while (true) switch (blk: {
                    defer cached_tt = null;
                    break :blk cached_tt orelse try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr);
                }) {
                    else => return ParseError.UnexpectedDTDToken,
                    .eof => return ParseError.UnclosedDTDEntity,
                    .tag_whitespace => unreachable,

                    .comma => {
                        try parse_ctx.feedDTDElementChildToken(.comma);
                        switch (prev) {
                            .lparen, .comma, .pipe => return ParseError.UnexpectedDTDToken,
                            .rparen, .name => {},
                        }
                        prev = .comma;
                    },

                    .pipe => {
                        try parse_ctx.feedDTDElementChildToken(.pipe);
                        switch (prev) {
                            .lparen, .comma, .pipe => return ParseError.UnexpectedDTDToken,
                            .rparen, .name => {},
                        }
                        prev = .pipe;
                    },

                    .lparen => {
                        try parse_ctx.feedDTDElementChildToken(.lparen);
                        switch (prev) {
                            .rparen, .name => return ParseError.UnexpectedDTDToken,
                            .lparen, .comma, .pipe => {},
                        }
                        prev = .lparen;
                        depth += 1;
                    },
                    .rparen => {
                        try parse_ctx.feedDTDElementChildToken(.rparen);
                        const maybe_cpq: ?ElementChildToken = switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                            .qmark => .qmark,
                            .asterisk => .asterisk,
                            .plus => .plus,
                            else => |tag| blk: {
                                cached_tt = tag;
                                break :blk null;
                            },
                        };
                        if (maybe_cpq) |cpq| {
                            try parse_ctx.feedDTDElementChildToken(cpq);
                        }

                        switch (prev) {
                            .lparen => return ParseError.UnexpectedDTDToken,
                            .rparen, .name, .comma, .pipe => {},
                        }
                        prev = .rparen;
                        depth -= 1;
                        if (depth == 0) break;
                    },

                    .tag_token => {
                        try parse_ctx.feedDTDElementChildToken(.name);
                        try feedTokenSrc(Impl, parse_ctx, tokenizer, .dtd, MaybeReader, mbr);

                        switch (prev) {
                            .rparen, .name => return ParseError.UnexpectedDTDToken,
                            .lparen, .comma, .pipe => {},
                        }
                        prev = .name;
                    },
                };

                const non_whitespace_tt = switch (cached_tt orelse try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                    .tag_whitespace => try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr),
                    else => |tt| tt,
                };
                switch (non_whitespace_tt) {
                    else => return ParseError.UnexpectedDTDToken,
                    .eof => return ParseError.UnclosedDTDEntity,
                    .tag_whitespace => unreachable,
                    .angle_bracket_right => {},
                }
            },
        },
    }
}

fn handleDTDAttlist(
    comptime Impl: type,
    parse_ctx: ParseCtx(Impl),
    tokenizer: *Tokenizer,
    comptime MaybeReader: ?type,
    mbr: parse_helper.MaybeBufferedReader(MaybeReader),
) !void {
    switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
        else => return ParseError.UnexpectedDTDToken,
        .eof => return ParseError.UnclosedDTDAttlist,
        .tag_token => {},
    }
    try parse_ctx.feedDTDAttlistStart();
    try feedTokenSrc(Impl, parse_ctx, tokenizer, .dtd, MaybeReader, mbr); // the declared name

    while (true) {
        if (try parse_helper.expectAndSkipIfTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
            else => return ParseError.MissingDTDSpacing,
            .eof => return ParseError.UnclosedDTDAttlist,
            .tag_whitespace => unreachable,
            .angle_bracket_right => break,
        };

        switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
            else => return ParseError.UnexpectedDTDToken,
            .eof => return ParseError.UnclosedDTDAttlist,
            .angle_bracket_right => break,
            .tag_token => {},
        }
        try parse_ctx.feedDTDAttlistDef();
        try feedTokenSrc(Impl, parse_ctx, tokenizer, .dtd, MaybeReader, mbr);

        if (try parse_helper.expectAndSkipIfTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
            else => return ParseError.MissingDTDSpacing,
            .eof => return ParseError.UnclosedDTDAttlist,
            .tag_whitespace => unreachable,
        };

        const attr_type: AttributeType = switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
            else => return ParseError.UnexpectedDTDToken,
            .eof => return ParseError.UnclosedDTDAttlist,
            .lparen => .enumeration,
            .tag_token => att: {
                const TypeStr = comptime @Type(.{ .Enum = blk: {
                    var info = @typeInfo(AttributeType).Enum;
                    var fields = info.fields;
                    assert(std.mem.eql(u8, fields[fields.len - 1].name, "enumeration"));
                    fields.len -= 1;
                    info.fields = fields;
                    break :blk info;
                } });
                const maybe_attr_type = try parse_helper.nextTokenSrcAsEnum(tokenizer, .dtd, MaybeReader, mbr, TypeStr);
                const attr_type = maybe_attr_type orelse return ParseError.UnexpectedDTDToken;

                switch (attr_type) {
                    .CDATA => {},
                    .ID => {},
                    .IDREF => {},
                    .IDREFS => {},
                    .ENTITY => {},
                    .ENTITIES => {},
                    .NMTOKEN => {},
                    .NMTOKENS => {},

                    .NOTATION => {
                        if (try parse_helper.expectAndSkipIfTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
                            else => return ParseError.MissingDTDSpacing,
                            .eof => return ParseError.UnclosedDTDAttlist,
                            .tag_whitespace => unreachable,
                        };
                        switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                            else => return ParseError.UnexpectedDTDToken,
                            .eof => return ParseError.UnclosedDTDAttlist,
                            .lparen => {},
                        }
                    },
                }

                break :att switch (attr_type) {
                    inline else => |tag| @field(AttributeType, @tagName(tag)),
                };
            },
        };
        try parse_ctx.feedDTDAttlistDefType(attr_type);

        switch (attr_type) {
            .CDATA => {},
            .ID => {},
            .IDREF => {},
            .IDREFS => {},
            .ENTITY => {},
            .ENTITIES => {},
            .NMTOKEN => {},
            .NMTOKENS => {},

            .NOTATION,
            .enumeration,
            => {
                switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                    else => return ParseError.UnexpectedDTDToken,
                    .eof => return ParseError.UnclosedDTDAttlist,
                    .tag_whitespace => unreachable,
                    .tag_token => {},
                }
                try feedTokenSrc(Impl, parse_ctx, tokenizer, .dtd, MaybeReader, mbr);

                while (true) {
                    switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                        else => return ParseError.UnexpectedDTDToken,
                        .eof => return ParseError.UnclosedDTDAttlist,
                        .tag_whitespace => unreachable,
                        .rparen => break,
                        .pipe => {},
                    }
                    switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                        else => return ParseError.UnexpectedDTDToken,
                        .eof => return ParseError.UnclosedDTDAttlist,
                        .tag_whitespace => unreachable,
                        .tag_token => {},
                    }
                    try feedTokenSrc(Impl, parse_ctx, tokenizer, .dtd, MaybeReader, mbr);
                }
            },
        }

        if (try parse_helper.expectAndSkipIfTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
            else => return ParseError.MissingDTDSpacing,
            .eof => return ParseError.UnclosedDTDAttlist,
            .tag_whitespace => unreachable,
        };

        const maybe_open_quote: ?Tokenizer.TokenType, //
        const dd_kind: ?DefaultDeclKind //
        = switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
            else => return ParseError.UnexpectedDTDToken,
            .eof => return ParseError.UnclosedDTDAttlist,
            .tag_whitespace => unreachable,
            .quote_single,
            .quote_double,
            => |oq| .{ oq, null },
            .hashtag => blk: {
                switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                    else => return ParseError.UnexpectedDTDToken,
                    .eof => return ParseError.UnclosedDTDAttlist,
                    .tag_whitespace => unreachable,
                    .tag_token => {},
                }
                const maybe_dd_kind = try parse_helper.nextTokenSrcAsEnum(tokenizer, .dtd, MaybeReader, mbr, DefaultDeclKind);
                const dd_kind = maybe_dd_kind orelse return ParseError.UnexpectedDTDToken;
                break :blk .{ null, dd_kind };
            },
        };

        try parse_ctx.feedDTDAttlistDefaultDeclStart(dd_kind);

        if (dd_kind == null or dd_kind.? == .FIXED) {
            const open_quote: Tokenizer.TokenType = maybe_open_quote orelse oq: {
                if (try parse_helper.expectAndSkipIfTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
                    else => return ParseError.MissingDTDSpacing,
                    .eof => return ParseError.UnclosedDTDAttlist,
                    .tag_whitespace => unreachable,
                };
                break :oq try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr);
            };
            const str_ctx: Tokenizer.Context = switch (open_quote) {
                else => return ParseError.UnexpectedDTDToken,
                .eof => return ParseError.UnclosedDTDAttlist,
                .quote_single => .attribute_value_quote_single,
                .quote_double => .attribute_value_quote_double,
            };
            while (true) switch (try parse_helper.nextTokenType(tokenizer, str_ctx, MaybeReader, mbr)) {
                .eof => return ParseError.UnclosedAttributeValue,
                .ampersand => {
                    try parse_ctx.feedReferenceStart(.general);

                    switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
                        else => unreachable,
                        .invalid_reference_end => return ParseError.InvalidReferenceEnd,
                        .semicolon => return ParseError.EmptyReference,
                        .tag_token => {},
                    }
                    try feedTokenSrc(Impl, parse_ctx, tokenizer, .reference, MaybeReader, mbr);

                    switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
                        else => unreachable,
                        .invalid_reference_end => return ParseError.InvalidReferenceEnd,
                        .tag_token => unreachable,
                        .semicolon => {},
                    }
                    try parse_ctx.feedReferenceEnd();
                },
                .text_data => try feedTokenSrc(Impl, parse_ctx, tokenizer, str_ctx, MaybeReader, mbr),
                .angle_bracket_left => return ParseError.AngleBracketLeftInAttribute,
                .quote_single,
                .quote_double,
                => |close_quote| {
                    assert(open_quote == close_quote);
                    break;
                },

                else => unreachable,
            };
        }

        try parse_ctx.feedDTDAttlistDefaultDeclEnd();
    }
}

fn handleDTDNotation(
    comptime Impl: type,
    parse_ctx: ParseCtx(Impl),
    tokenizer: *Tokenizer,
    comptime MaybeReader: ?type,
    mbr: parse_helper.MaybeBufferedReader(MaybeReader),
) !void {
    switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
        else => return ParseError.UnexpectedDTDToken,
        .eof => return ParseError.UnclosedDTDNotation,
        .tag_token => {},
    }
    try parse_ctx.feedDTDNotationStart();
    try feedTokenSrc(Impl, parse_ctx, tokenizer, .dtd, MaybeReader, mbr); // the declared name

    if (try parse_helper.expectAndSkipIfTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
        else => return ParseError.MissingDTDSpacing,
        .eof => return ParseError.UnclosedDTDNotation,
        .tag_whitespace => unreachable,
    };

    switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
        else => return ParseError.UnexpectedDTDToken,
        .eof => return ParseError.UnclosedDTDNotation,
        .tag_token => {},
    }
    const maybe_ext_id_kind = try parse_helper.nextTokenSrcAsEnum(tokenizer, .dtd, MaybeReader, mbr, ExternalIdKind);
    const ext_id_kind = maybe_ext_id_kind orelse return ParseError.UnexpectedDTDToken;

    const terminating_tt = (try handleExternalOrPublicId(Impl, parse_ctx, tokenizer, MaybeReader, mbr, ext_id_kind, .dont_need_system_literal)) orelse
        try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr);

    switch (terminating_tt) {
        else => return ParseError.UnexpectedDTDToken,
        .eof => return ParseError.UnclosedDTDNotation,
        .tag_whitespace => unreachable,
        .angle_bracket_right => {},
    }
}

fn handleExternalOrPublicId(
    comptime Impl: type,
    parse_ctx: ParseCtx(Impl),
    tokenizer: *Tokenizer,
    comptime MaybeReader: ?type,
    mbr: parse_helper.MaybeBufferedReader(MaybeReader),
    ext_id_kind: ExternalIdKind,
    config: enum {
        require_system_literal,
        dont_need_system_literal,
    },
) !?Tokenizer.TokenType {
    switch (ext_id_kind) {
        .SYSTEM => {},
        .PUBLIC => pubid: {
            if (try parse_helper.expectAndSkipIfTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
                else => return ParseError.MissingDTDSpacing,
                .eof => return ParseError.UnclosedDTD,
                .tag_whitespace => unreachable,
            };

            const open_quote: Tokenizer.TokenType, //
            const str_ctx: Tokenizer.Context //
            = switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                else => return ParseError.UnexpectedDTDToken,
                .eof => return ParseError.UnclosedDTD,
                .quote_single => |oq| .{ oq, .system_literal_quote_single },
                .quote_double => |oq| .{ oq, .system_literal_quote_double },
            };

            switch (try parse_helper.nextTokenType(tokenizer, str_ctx, MaybeReader, mbr)) {
                else => return ParseError.UnexpectedDTDToken,
                .eof => return ParseError.UnclosedDTDPubidLiteral,
                .quote_single,
                .quote_double,
                => |close_quote| {
                    assert(open_quote == close_quote);
                    try parse_ctx.feedSrcEnd();
                    break :pubid;
                },
                .text_data => {},
            }

            var iter: parse_helper.TokenSrcIter(MaybeReader) = .{};
            while (try iter.next(tokenizer, str_ctx, mbr)) |segment| {
                if (anyNonPubidChars(
                    if (MaybeReader != null) segment else segment.toStr(tokenizer.src),
                    switch (open_quote) {
                        .quote_single => .single,
                        .quote_double => .double,
                        else => unreachable,
                    },
                )) return ParseError.InvalidDTDPubidLiteral;
                try parse_ctx.feedSrcSegment(segment);
            }
            try parse_ctx.feedSrcEnd();

            switch (try parse_helper.nextTokenType(tokenizer, str_ctx, MaybeReader, mbr)) {
                .eof => return ParseError.UnclosedDTDPubidLiteral,
                .quote_single,
                .quote_double,
                => |close_quote| assert(open_quote == close_quote),
                .text_data => unreachable,
                else => unreachable,
            }
        },
    }

    if (try parse_helper.expectAndSkipIfTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| {
        switch (config) {
            .require_system_literal => return ParseError.MissingDTDSpacing,
            .dont_need_system_literal => {},
        }
        return non_ws;
    }

    const open_quote: Tokenizer.TokenType, //
    const str_ctx: Tokenizer.Context //
    = switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
        .quote_single => |oq| .{ oq, .system_literal_quote_single },
        .quote_double => |oq| .{ oq, .system_literal_quote_double },
        .tag_whitespace => unreachable,
        else => |tt| return switch (ext_id_kind) {
            .SYSTEM => ParseError.UnexpectedDTDToken,
            .PUBLIC => switch (config) {
                .require_system_literal => ParseError.UnexpectedDTDToken,
                .dont_need_system_literal => tt,
            },
        },
    };

    switch (try parse_helper.nextTokenType(tokenizer, str_ctx, MaybeReader, mbr)) {
        else => return ParseError.UnexpectedDTDToken,
        .eof => return ParseError.UnclosedDTDPubidLiteral,
        .quote_single,
        .quote_double,
        => |close_quote| {
            assert(open_quote == close_quote);
            try parse_ctx.feedSrcEnd();
        },
        .text_data => try feedTokenSrc(Impl, parse_ctx, tokenizer, str_ctx, MaybeReader, mbr),
    }
    switch (try parse_helper.nextTokenType(tokenizer, str_ctx, MaybeReader, mbr)) {
        .eof => return ParseError.UnclosedDTDPubidLiteral,
        .quote_single,
        .quote_double,
        => |close_quote| assert(open_quote == close_quote),
        .text_data => unreachable,
        else => unreachable,
    }
    return null;
}

fn anyNonPubidChars(str: []const u8, quote_type: enum { single, double }) bool {
    return for (str) |char| switch (char) {
        '\u{20}', '\u{D}', '\u{A}' => {},
        'a'...'z', 'A'...'Z', '0'...'9' => {},
        '-' => {},
        '\'' => switch (quote_type) {
            .single => break true,
            .double => {},
        },
        '(', ')', '+', ',', '.', '/', ':', '=', '?', ';', '!', '*', '#', '@', '$', '_', '%' => {},
        else => break true,
    } else false;
}

pub const IgnoreCtx = struct {
    pub fn feedSrcSegment(
        _: @This(),
        segment: anytype,
    ) !void {
        _ = segment;
    }
    pub fn feedSrcEnd(_: @This()) !void {}

    pub fn feedReferenceStart(_: @This(), kind: ReferenceKind) !void {
        _ = kind;
    }
    pub fn feedReferenceEnd(_: @This()) !void {}

    pub fn feedPI(_: @This()) !void {}

    pub fn feedDTDStart(_: @This()) !void {}

    pub fn feedDTDExternalId(_: @This(), kind: ExternalIdKind) !void {
        _ = kind;
    }

    pub fn feedDTDEntityStart(_: @This(), kind: ReferenceKind) !void {
        _ = kind;
    }
    pub fn feedDTDEntityEnd(_: @This()) !void {}
    pub fn feedDTDNDataDecl(_: @This()) !void {}

    pub fn feedDTDElementStart(_: @This()) !void {}
    pub fn feedDTDElementContentSpec(_: @This(), content_spec: ContentSpec) !void {
        _ = content_spec;
    }

    pub fn feedDTDElementMixedEnd(_: @This(), zero_or_many: bool) !void {
        _ = zero_or_many;
    }

    pub fn feedDTDElementChildToken(_: @This(), child_tok: ElementChildToken) !void {
        _ = child_tok;
    }

    pub fn feedDTDElementEnd(_: @This()) !void {}

    pub fn feedDTDAttlistStart(_: @This()) !void {}

    pub fn feedDTDAttlistDef(_: @This()) !void {}

    pub fn feedDTDAttlistDefType(_: @This(), attr_type: AttributeType) !void {
        _ = attr_type;
    }

    pub fn feedDTDAttlistDefaultDeclStart(_: @This(), default_decl_kind: ?DefaultDeclKind) !void {
        _ = default_decl_kind;
    }

    pub fn feedDTDAttlistDefaultDeclEnd(_: @This()) !void {}

    pub fn feedDTDAttlistEnd(_: @This()) !void {}

    pub fn feedDTDNotationStart(_: @This()) !void {}

    pub fn feedExternalOrPublicId(_: @This(), kind: ExternalIdKind) !void {
        _ = kind;
    }

    pub fn feedDTDNotationEnd(_: @This()) !void {}

    pub fn feedDTDEnd(_: @This()) !void {}
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
        \\ <!ELEMENT baz (#PCDATA|todo)*>
        \\ <!ELEMENT baz (phoe,be)*>
        \\ <!ELEMENT baz (a,(e|i),o,u)+>
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
