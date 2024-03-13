const std = @import("std");
const assert = std.debug.assert;

const iksemel = @import("iksemel.zig");
const Tokenizer = iksemel.Tokenizer;

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

pub const ContentSpecToken = enum {
    lparen,
    rparen,
    name,

    qmark,
    asterisk,
    plus,

    comma,
    pipe,
};

pub const ContentParticleQuantifier = enum {
    /// zero or one
    qmark,
    /// zero or many
    asterisk,
    /// one or many
    plus,
};

pub fn ParseCtx(comptime Inner: type) type {
    return struct {
        inner: Inner,
        const Self = @This();

        /// Called consecutively to construct the source.
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

pub const ParseMarker = union(enum) {
    /// Followed by `feedSrc*`, in order to construct the reference
    /// id/name token source.
    reference: ReferenceKind,

    /// Followed by `feedSrc*`, in order to feed the Processing
    /// Instructions (parsing of the PI target and data are
    /// left to the context).
    pi,

    /// Followed by `feedSrc*`, in order to construct the DTD name.
    /// Afterwards, may be followed by one of:
    /// * `external_id`
    /// * `entity_start`
    /// * `element_start`
    /// * `attlist_start`
    /// * `notation_start`
    /// * `dtd_end`
    dtd_start,

    /// If `== .PUBLIC`, this is followed by `feedSrc*` twice,
    /// first for the pubid literal, then for the system literal.
    ///
    /// If `== .SYSTEM`, this is followed by `feedSrc*` once,
    /// for the system literal.
    ///
    /// After either of the above, this may be followed by one of:
    /// * `entity_start`
    /// * `element_start`
    /// * `attlist_start`
    /// * `notation_start`
    /// * `dtd_end`
    external_id: ExternalIdKind,

    /// If `== .general`, this may be followed by one of the
    /// following sequences:
    /// * `(feedSrc* | reference)*`, for a simple entity value.
    /// * First `external_id`, and then optionally `feedSrc*` for the NDataDecl name.
    ///
    /// If `== .parsed`, this may be followed by one of the
    /// following sequences:
    /// * `(feedSrc* | reference)*`, for a simple entity value.
    /// * `external_id`
    ///
    /// After either of the above, this will be followed by `entity_end`.
    entity_start: ReferenceKind,
    /// Signifies the end of the entity declaration.
    /// May be followed by:
    /// * `entity_start`
    /// * `element_start`
    /// * `attlist_start`
    /// * `notation_start`
    /// * `dtd_end`
    entity_end,

    /// Followed by `feedSrc*`, in order to construct the element name.
    /// Afterwards, it will be followed by `element_content_spec`.
    element_start,
    /// If `== .EMPTY` or `content_spec == .ANY`, this is
    /// followed by `element_end`, with quantifier = null.
    ///
    /// If `== .Mixed`, this is potentially followed by a
    /// series of calls to `feedSrc*` for each name that appears in the
    /// listing. Afterwards, it is followed by `element_end`, with a quantifier
    /// which will either be null or `.asterisk`; if the number of names
    /// in the listing was non-zero, it will be `.asterisk`.
    ///
    /// If `== .children`, this is followed by `element_content_spec_token = .lparen`.
    element_content_spec: ContentSpec,
    /// Each value can be followed by a set of values:
    /// * `.lparen`:   `.lparen`, `.name`
    /// * `.rparen`:   `.rparen`, `.comma`, `.pipe`, `.qmark`, `.asterisk`, `.plus`
    /// * `.name`:     `.rparen`, `.comma`, `.pipe`, `.qmark`, `.asterisk`, `.plus`
    /// * `.qmark`:    `.rparen`, `.comma`, `.pipe`
    /// * `.asterisk`: `.rparen`, `.comma`, `.pipe`
    /// * `.plus`:     `.rparen`, `.comma`, `.pipe`
    /// * `.comma`:    `.lparen`, `.name`
    /// * `.pipe`:     `.lparen`, `.name`
    ///
    /// The `.name` token is always followed by `feedSrc*`.
    ///
    /// This marker is returned one or more times to form Content Particles, where
    /// a Content Particle is defined (recursively) as being one of:
    /// * A name (`.name`), optionally followed by a quantifier (`.qmark`, `.asterisk`, or `.plus`).
    /// * A choice, which is a `.pipe`-delimited list of Content Particles, enclosed by parentheses (`.lparen` & `.rparen`).
    /// * A sequence, which is a `.comma`-delimited list of Content Particles, enclosed by parentheses (`.lparen` & `.rparen`).
    ///
    /// NOTE: the context is responsible for verifying that a Content Particle's delimiters are
    /// not a mixture of `.pipe`s and `.comma`s.
    ///
    /// The Content Particle which is ultimately formed will either be a choice or a sequence;
    /// the parsing procedure does not verify this due to the recursive nature of its definition.
    ///
    /// This is followed by `element_end`.
    element_content_spec_token: ContentSpecToken,
    /// Signifies the end of the element declaration, and indicates
    /// the quantifier of the content specification if present.
    /// May be followed by:
    /// * `entity_start`
    /// * `element_start`
    /// * `attlist_start`
    /// * `notation_start`
    /// * `dtd_end`
    element_end: ?ContentParticleQuantifier,

    /// Followed by `feedSrc*`, in order to construct the attribute list name.
    /// Afterwards, it will be followed by a series of consecutive `attlist_def_start`,
    /// terminated by `attlist_end`.
    attlist_start,
    /// Followed by `feedSrc*`, in order to construct the attribute defintion name.
    /// Afterwards, it will be followed by `attlist_def_type`.
    attlist_def_start,
    /// If `== .NOTATION` or `== .enumeration`, this will be followed by a
    /// series of calls to `feedSrc*`, for the list of potential Name/Nmtokens.
    /// Followed by `attlist_def_default_decl_start`.
    attlist_def_type: AttributeType,
    /// If ` == null` or `== .FIXED`, this will be followed by a sequence of interspersed
    /// calls to `feedSrc` and `reference*` for the attribute value. This will be
    /// followed by `attlist_def_default_decl_end`.
    attlist_def_default_decl_start: ?DefaultDeclKind,
    /// This will be followed by a call to `attlist_def_start` or `attlist_end`.
    attlist_def_default_decl_end,
    /// Signifies the end of the attribute list declaration.
    /// May be followed by:
    /// * `entity_start`
    /// * `element_start`
    /// * `attlist_start`
    /// * `notation_start`
    /// * `dtd_end`
    attlist_end,

    /// Followed by `feedSrc*` in order to  construct the Notation declaration name.
    /// Afterwards, it will be followed by `external_or_public_id`.
    notation_start,
    /// If `== .SYSTEM`, this is followed by `feedSrc*` once, for the system literal.
    ///
    /// If `== .PUBLIC`, this is followed by `feedSrc*` at least once for the pubid literal,
    /// and then optionally a second time for the system literal.
    ///
    /// After either of the above, this will be followed by `notation_end`.
    external_or_public_id: ExternalIdKind,
    /// Signifies the end of the notation declaration.
    /// May be followed by:
    /// * `entity_start`
    /// * `element_start`
    /// * `attlist_start`
    /// * `notation_start`
    /// * `dtd_end`
    notation_end,

    /// Ends the DTD Declaration.
    dtd_end,
};

pub const ParseError = error{
    MoreThanOneDTD,
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
    /// Must satisfy the interface described by `ParseCtx`.
    parse_ctx_impl: anytype,
) !Tokenizer.TokenType {
    var tokenizer = Tokenizer.initStreaming();
    return parseUntilLABReaderOrSlice(parse_ctx_impl, &tokenizer, @TypeOf(reader), .{
        .reader = reader,
        .read_buffer = read_buffer,
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

    var dtd_already_parsed = false;
    while (true) switch (try parse_helper.nextTokenType(tokenizer, .non_markup, MaybeReader, mbr)) {
        .eof, .angle_bracket_left => |tag| return tag,

        .pi_start => try handlePi(Impl, parse_ctx, tokenizer, MaybeReader, mbr),

        .dtd_start => {
            if (dtd_already_parsed) return ParseError.MoreThanOneDTD;
            dtd_already_parsed = true;
            try handleDTD(Impl, parse_ctx, tokenizer, MaybeReader, mbr);
            try parse_ctx.feedMarker(.dtd_end);
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
    while (true) {
        const segment = try iter.next(tokenizer, context, mbr);
        try parse_ctx.feedSrc(segment);
        if (segment == null) break;
    }
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
    try parse_ctx.feedMarker(.pi);
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
    if (try parse_helper.skipIfTagWhitespaceOrGetNextTokType(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
        .tag_whitespace => unreachable,
        .eof => return ParseError.UnclosedDTD,
        else => return ParseError.MissingDTDSpacing,
    };

    switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
        else => return ParseError.UnexpectedDTDToken,
        .tag_whitespace => unreachable,
        .tag_token => {},
    }
    try parse_ctx.feedMarker(.dtd_start);
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
            try parse_ctx.feedMarker(.{ .external_id = ext_id_kind });
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

            if (try parse_helper.skipIfTagWhitespaceOrGetNextTokType(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
                .tag_whitespace => unreachable,
                .eof => return switch (decl_str) {
                    .@"<!ENTITY" => ParseError.UnclosedDTDEntity,
                    .@"<!ELEMENT" => ParseError.UnclosedDTDElement,
                    .@"<!ATTLIST" => ParseError.UnclosedDTDAttlist,
                    .@"<!NOTATION" => ParseError.UnclosedDTDNotation,
                },
                else => return ParseError.MissingDTDSpacing,
            };

            switch (decl_str) {
                .@"<!ENTITY" => {
                    try handleDTDEntity(Impl, parse_ctx, tokenizer, MaybeReader, mbr);
                    try parse_ctx.feedMarker(.entity_end);
                },
                .@"<!ELEMENT" => {
                    const cpq = try handleDTDElement(Impl, parse_ctx, tokenizer, MaybeReader, mbr);
                    try parse_ctx.feedMarker(.{ .element_end = cpq });
                },
                .@"<!ATTLIST" => {
                    try handleDTDAttlist(Impl, parse_ctx, tokenizer, MaybeReader, mbr);
                    try parse_ctx.feedMarker(.attlist_end);
                },
                .@"<!NOTATION" => {
                    try handleDTDNotation(Impl, parse_ctx, tokenizer, MaybeReader, mbr);
                    try parse_ctx.feedMarker(.notation_end);
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
            if (try parse_helper.skipIfTagWhitespaceOrGetNextTokType(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
                .tag_whitespace => unreachable,
                .eof => return ParseError.UnclosedDTDEntity,
                else => return ParseError.MissingDTDSpacing,
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
    try parse_ctx.feedMarker(.{ .entity_start = ref_kind });
    try feedTokenSrc(Impl, parse_ctx, tokenizer, .dtd, MaybeReader, mbr); // the declared name

    if (try parse_helper.skipIfTagWhitespaceOrGetNextTokType(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
        .tag_whitespace => unreachable,
        .eof => return ParseError.UnclosedDTDEntity,
        else => return ParseError.MissingDTDSpacing,
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
                    try parse_ctx.feedMarker(.{ .reference = ref_val_kind });

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

            try parse_ctx.feedMarker(.{ .external_id = ext_id_kind });
            assert((try handleExternalOrPublicId(Impl, parse_ctx, tokenizer, MaybeReader, mbr, ext_id_kind, .require_system_literal)) == null);

            switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                else => return ParseError.UnexpectedDTDToken,
                .eof => return ParseError.UnclosedDTDEntity,
                .angle_bracket_right => return,
                .tag_whitespace => try parse_helper.skipWhitespaceSrcUnchecked(tokenizer, .dtd, MaybeReader, mbr),
            }

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
                    switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                        else => return ParseError.UnexpectedDTDToken,
                        .eof => return ParseError.UnclosedDTDEntity,
                        .tag_whitespace => try parse_helper.skipWhitespaceSrcUnchecked(tokenizer, .dtd, MaybeReader, mbr),
                    }
                    switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                        else => return ParseError.UnexpectedDTDToken,
                        .eof => return ParseError.UnclosedDTDEntity,
                        .tag_whitespace => unreachable,
                        .tag_token => {},
                    }

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
) !?ContentParticleQuantifier {
    switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
        else => return ParseError.UnexpectedDTDToken,
        .eof => return ParseError.UnclosedDTDElement,
        .tag_token => {},
    }
    try parse_ctx.feedMarker(.element_start);
    try feedTokenSrc(Impl, parse_ctx, tokenizer, .dtd, MaybeReader, mbr); // the declared name

    if (try parse_helper.skipIfTagWhitespaceOrGetNextTokType(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
        .tag_whitespace => unreachable,
        .eof => return ParseError.UnclosedDTDElement,
        else => return ParseError.MissingDTDSpacing,
    };

    switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
        else => return ParseError.UnexpectedDTDToken,
        .eof => return ParseError.UnclosedDTDElement,

        .tag_token => {
            const EmptyOrAny = enum { EMPTY, ANY };
            const maybe_empty_or_any = try parse_helper.nextTokenSrcAsEnum(tokenizer, .dtd, MaybeReader, mbr, EmptyOrAny);
            const empty_or_any = maybe_empty_or_any orelse return ParseError.UnexpectedDTDToken;
            try parse_ctx.feedMarker(.{ .element_content_spec = switch (empty_or_any) {
                .EMPTY => .EMPTY,
                .ANY => .ANY,
            } });
            switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                else => return ParseError.UnexpectedDTDToken,
                .eof => return ParseError.UnclosedDTDElement,
                .angle_bracket_right => {},
            }
            return null;
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
                try parse_ctx.feedMarker(.{ .element_content_spec = .Mixed });

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

                const many_opts_specifier = switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                    .asterisk => switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                        .angle_bracket_right => true,

                        .tag_whitespace => unreachable,
                        .eof => return ParseError.UnclosedDTDElement,
                        else => return ParseError.UnexpectedDTDToken,
                    },
                    .tag_whitespace => switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                        .angle_bracket_right => false,

                        .tag_whitespace => unreachable,
                        .eof => return ParseError.UnclosedDTDElement,
                        else => return ParseError.UnexpectedDTDToken,
                    },
                    .angle_bracket_right => false,

                    .eof => return ParseError.UnclosedDTDElement,
                    else => return ParseError.UnexpectedDTDToken,
                };

                if (many_opts and !many_opts_specifier) {
                    return ParseError.MissingAsteriskForManyPCDATAOptions;
                }

                return if (many_opts_specifier) .asterisk else null;
            },
            .lparen, .tag_token => |first_cached_tt| {
                try parse_ctx.feedMarker(.{ .element_content_spec = .children });
                try parse_ctx.feedMarker(.{ .element_content_spec_token = .lparen });

                var depth: u64 = 1;
                var prev: enum { lparen, rparen, name, comma, pipe, quantifier } = .lparen;

                var cached_tt: ?Tokenizer.TokenType = first_cached_tt;
                while (true) switch (blk: {
                    defer cached_tt = null;
                    break :blk cached_tt orelse try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr);
                }) {
                    else => return ParseError.UnexpectedDTDToken,
                    .eof => return ParseError.UnclosedDTDEntity,
                    .tag_whitespace => try parse_helper.skipWhitespaceSrcUnchecked(tokenizer, .dtd, MaybeReader, mbr),

                    .qmark, .asterisk, .plus => |tt| {
                        switch (prev) {
                            .lparen, .comma, .pipe, .quantifier => return ParseError.UnexpectedDTDToken,
                            .rparen, .name => {},
                        }
                        prev = .quantifier;
                        try parse_ctx.feedMarker(.{ .element_content_spec_token = switch (tt) {
                            inline .qmark, .asterisk, .plus => |tag| @field(ContentSpecToken, @tagName(tag)),
                            else => unreachable,
                        } });
                    },

                    .comma => {
                        switch (prev) {
                            .lparen, .comma, .pipe => return ParseError.UnexpectedDTDToken,
                            .rparen, .name, .quantifier => {},
                        }
                        prev = .comma;
                        try parse_ctx.feedMarker(.{ .element_content_spec_token = .comma });
                    },

                    .pipe => {
                        switch (prev) {
                            .lparen, .comma, .pipe => return ParseError.UnexpectedDTDToken,
                            .rparen, .name, .quantifier => {},
                        }
                        prev = .pipe;
                        try parse_ctx.feedMarker(.{ .element_content_spec_token = .pipe });
                    },

                    .lparen => {
                        switch (prev) {
                            .rparen, .name, .quantifier => return ParseError.UnexpectedDTDToken,
                            .lparen, .comma, .pipe => {},
                        }
                        prev = .lparen;
                        try parse_ctx.feedMarker(.{ .element_content_spec_token = .lparen });

                        depth += 1;
                    },
                    .rparen => {
                        switch (prev) {
                            .lparen, .comma, .pipe => return ParseError.UnexpectedDTDToken,
                            .rparen, .name, .quantifier => {},
                        }
                        prev = .rparen;
                        try parse_ctx.feedMarker(.{ .element_content_spec_token = .rparen });

                        depth -= 1;
                        if (depth == 0) break;
                    },

                    .tag_token => {
                        switch (prev) {
                            .rparen, .name, .quantifier => return ParseError.UnexpectedDTDToken,
                            .lparen, .comma, .pipe => {},
                        }
                        prev = .name;
                        try parse_ctx.feedMarker(.{ .element_content_spec_token = .name });
                        try feedTokenSrc(Impl, parse_ctx, tokenizer, .dtd, MaybeReader, mbr);
                    },
                };

                const cpq: ?ContentParticleQuantifier = switch (cached_tt orelse try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                    else => return ParseError.UnexpectedDTDToken,
                    .eof => return ParseError.UnclosedDTDEntity,

                    .qmark, .asterisk, .plus => |tt| cpq: {
                        const cpq: ContentParticleQuantifier = switch (tt) {
                            .qmark => .qmark,
                            .asterisk => .asterisk,
                            .plus => .plus,
                            else => unreachable,
                        };
                        switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                            else => return ParseError.UnexpectedDTDToken,
                            .eof => return ParseError.UnclosedDTDElement,
                            .tag_whitespace => unreachable,
                            .angle_bracket_right => {},
                        }
                        break :cpq cpq;
                    },
                    .angle_bracket_right => null,
                    .tag_whitespace => switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                        else => return ParseError.UnexpectedDTDToken,
                        .eof => return ParseError.UnclosedDTDEntity,

                        .tag_whitespace => unreachable,
                        .angle_bracket_right => null,
                    },
                };
                return cpq;
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
    try parse_ctx.feedMarker(.attlist_start);
    try feedTokenSrc(Impl, parse_ctx, tokenizer, .dtd, MaybeReader, mbr); // the declared name

    while (true) {
        switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
            else => return ParseError.MissingDTDSpacing,
            .eof => return ParseError.UnclosedDTDAttlist,
            .angle_bracket_right => break,
            .tag_whitespace => try parse_helper.skipWhitespaceSrcUnchecked(tokenizer, .dtd, MaybeReader, mbr),
        }

        switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
            else => return ParseError.UnexpectedDTDToken,
            .eof => return ParseError.UnclosedDTDAttlist,
            .tag_whitespace => unreachable,
            .angle_bracket_right => break,
            .tag_token => {},
        }
        try parse_ctx.feedMarker(.attlist_def_start);
        try feedTokenSrc(Impl, parse_ctx, tokenizer, .dtd, MaybeReader, mbr);

        if (try parse_helper.skipIfTagWhitespaceOrGetNextTokType(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
            .tag_whitespace => unreachable,
            .eof => return ParseError.UnclosedDTDAttlist,
            else => return ParseError.MissingDTDSpacing,
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
                        if (try parse_helper.skipIfTagWhitespaceOrGetNextTokType(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
                            .tag_whitespace => unreachable,
                            .eof => return ParseError.UnclosedDTDAttlist,
                            else => return ParseError.MissingDTDSpacing,
                        };
                        switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                            else => return ParseError.UnexpectedDTDToken,
                            .eof => return ParseError.UnclosedDTDAttlist,
                            .tag_whitespace => unreachable,
                            .lparen => {},
                        }
                    },
                }

                break :att switch (attr_type) {
                    inline else => |tag| @field(AttributeType, @tagName(tag)),
                };
            },
        };
        try parse_ctx.feedMarker(.{ .attlist_def_type = attr_type });

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

        if (try parse_helper.skipIfTagWhitespaceOrGetNextTokType(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
            .tag_whitespace => unreachable,
            .eof => return ParseError.UnclosedDTDAttlist,
            else => return ParseError.MissingDTDSpacing,
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

        try parse_ctx.feedMarker(.{ .attlist_def_default_decl_start = dd_kind });

        if (dd_kind == null or dd_kind.? == .FIXED) {
            const open_quote: Tokenizer.TokenType = maybe_open_quote orelse oq: {
                if (try parse_helper.skipIfTagWhitespaceOrGetNextTokType(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
                    .tag_whitespace => unreachable,
                    .eof => return ParseError.UnclosedDTDAttlist,
                    else => return ParseError.MissingDTDSpacing,
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
                    try parse_ctx.feedMarker(.{ .reference = .general });

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

        try parse_ctx.feedMarker(.attlist_def_default_decl_end);
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
    try parse_ctx.feedMarker(.notation_start);
    try feedTokenSrc(Impl, parse_ctx, tokenizer, .dtd, MaybeReader, mbr); // the declared name

    if (try parse_helper.skipIfTagWhitespaceOrGetNextTokType(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
        .tag_whitespace => unreachable,
        .eof => return ParseError.UnclosedDTDNotation,
        else => return ParseError.MissingDTDSpacing,
    };

    switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
        else => return ParseError.UnexpectedDTDToken,
        .eof => return ParseError.UnclosedDTDNotation,
        .tag_token => {},
    }
    const maybe_ext_id_kind = try parse_helper.nextTokenSrcAsEnum(tokenizer, .dtd, MaybeReader, mbr, ExternalIdKind);
    const ext_id_kind = maybe_ext_id_kind orelse return ParseError.UnexpectedDTDToken;
    try parse_ctx.feedMarker(.{ .external_or_public_id = ext_id_kind });

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
            if (try parse_helper.skipIfTagWhitespaceOrGetNextTokType(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
                .tag_whitespace => unreachable,
                .eof => return ParseError.UnclosedDTD,
                else => return ParseError.MissingDTDSpacing,
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
                    try parse_ctx.feedSrc(@as(?if (MaybeReader != null) []const u8 else Tokenizer.Range, null));
                    break :pubid;
                },
                .text_data => {},
            }

            var iter: parse_helper.TokenSrcIter(MaybeReader) = .{};
            while (true) {
                const maybe_segment = try iter.next(tokenizer, str_ctx, mbr);
                if (maybe_segment) |segment| {
                    if (anyNonPubidChars(
                        if (MaybeReader != null) segment else segment.toStr(tokenizer.src),
                        switch (open_quote) {
                            .quote_single => .single,
                            .quote_double => .double,
                            else => unreachable,
                        },
                    )) return ParseError.InvalidDTDPubidLiteral;
                }
                try parse_ctx.feedSrc(maybe_segment);
                if (maybe_segment == null) break;
            }

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

    switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
        else => |tt| return switch (config) {
            .require_system_literal => ParseError.MissingDTDSpacing,
            .dont_need_system_literal => tt,
        },
        .tag_whitespace => try parse_helper.skipWhitespaceSrcUnchecked(tokenizer, .dtd, MaybeReader, mbr),
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
        else => unreachable,
        .eof => return ParseError.UnclosedDTDPubidLiteral,
        .text_data => try feedTokenSrc(Impl, parse_ctx, tokenizer, str_ctx, MaybeReader, mbr),
        .quote_single,
        .quote_double,
        => |close_quote| {
            assert(open_quote == close_quote);
            try parse_ctx.feedSrc(@as(?if (MaybeReader != null) []const u8 else Tokenizer.Range, null));
        },
    }
    switch (try parse_helper.nextTokenType(tokenizer, str_ctx, MaybeReader, mbr)) {
        else => unreachable,
        .eof => return ParseError.UnclosedDTDPubidLiteral,
        .text_data => unreachable,
        .quote_single,
        .quote_double,
        => |close_quote| assert(open_quote == close_quote),
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
    pub fn feedSrc(_: @This(), segment: anytype) !void {
        _ = segment;
    }

    pub fn feedMarker(_: @This(), marker: ParseMarker) !void {
        _ = marker;
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
        \\
    );
    try std.testing.expectEqual(.eof, parseSlice(&tokenizer, IgnoreCtx{}));
}

test parseReader {
    var fbs = std.io.fixedBufferStream(
        \\<?xml version="1.0" encoding="UTF-8"?>
        \\
        \\<!DOCTYPE foo [
        \\ <?phi?>
        \\ <!-- foo -->
        \\ <!ELEMENT bar EMPTY>
        \\ <!ELEMENT baz (#PCDATA)*>
        \\ <!ELEMENT baz (#PCDATA|todo)*>
        \\ <!ELEMENT baz (phoe,be)*>
        \\ <!ELEMENT baz (a,(b|c),d,e,(f,g)?,(h|(i,j,k)*))+>
        \\
        \\ <!ATTLIST foo fizz CDATA #IMPLIED>
        \\ <!ATTLIST foo fizz NOTATION (eao|oae|aeo) #FIXED "eao">
        \\ <!ATTLIST foo fizz NOTATION (eao|oae|aeo) "eao">
        \\
        \\ <!ENTITY lorem "ipsum">
        \\ <!ENTITY % lorem "ipsum">
        \\ <!ENTITY lorem SYSTEM "c">
        \\ <!ENTITY % lorem SYSTEM "c">
        \\
        \\ <!NOTATION eee SYSTEM "d">
        \\ <!NOTATION eee PUBLIC "e">
        \\ <!NOTATION fff PUBLIC "f" "g">
        \\ <!NOTATION ggg SYSTEM "g">
        \\]>
        \\
    );
    var read_buffer: [1]u8 = undefined;
    try std.testing.expectEqual(.eof, parseReader(fbs.reader(), &read_buffer, IgnoreCtx{}));
}

const TestParseItem = union(enum) {
    marker: ParseMarker,
    str: []const u8,

    pub fn format(
        tpi: TestParseItem,
        comptime fmt_str: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt_str;
        _ = options;
        switch (tpi) {
            .marker => |marker| {
                try writer.writeAll("TPI(");
                switch (marker) {
                    inline else => |payload, tag| {
                        const T = @TypeOf(payload);
                        switch (@typeInfo(T)) {
                            .Void => try writer.print(".{s}", .{@tagName(tag)}),
                            .Enum => try writer.print(".{s} = .{s}", .{ @tagName(tag), @tagName(payload) }),
                            .Optional => |optional| opt: {
                                const unwrapped = payload orelse {
                                    try writer.print(".{s} = null", .{@tagName(tag)});
                                    break :opt;
                                };
                                switch (@typeInfo(optional.child)) {
                                    .Enum => try writer.print(".{s} = .{s}", .{ @tagName(tag), @tagName(unwrapped) }),
                                    else => try writer.print(".{s} = {any}", .{ @tagName(tag), unwrapped }),
                                }
                            },
                            else => comptime unreachable,
                        }
                    },
                }
                try writer.writeAll(")");
            },
            .str => |str| try writer.print("TPI('{s}')", .{str}),
        }
    }
};

fn testParse(
    expected_result: (ParseError || std.mem.Allocator.Error)!Tokenizer.TokenType,
    buffer_sizes: []const usize,
    src: []const u8,
    expected_outs: []const TestParseItem,
) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var actual_outs_buffer = std.ArrayList(TestParseItem).init(allocator);
    defer actual_outs_buffer.deinit();

    {
        const full_read_buffer = try std.testing.allocator.alloc(u8, if (buffer_sizes.len != 0) std.mem.max(usize, buffer_sizes) else 0);
        defer std.testing.allocator.free(full_read_buffer);

        var current_str_buf = std.ArrayList(u8).init(std.testing.allocator);
        defer current_str_buf.deinit();

        for (buffer_sizes) |buf_size| {
            var fbs = std.io.fixedBufferStream(src);
            const ctx: struct {
                allocator: std.mem.Allocator,
                actual: *@TypeOf(actual_outs_buffer),
                current_str: *std.ArrayList(u8),

                pub fn feedSrc(ctx: @This(), segment: ?[]const u8) !void {
                    try ctx.current_str.appendSlice(segment orelse {
                        try ctx.actual.append(.{ .str = try ctx.allocator.dupe(u8, ctx.current_str.items) });
                        ctx.current_str.clearRetainingCapacity();
                        return;
                    });
                }

                pub fn feedMarker(ctx: @This(), marker: ParseMarker) !void {
                    try ctx.actual.append(.{ .marker = marker });
                }
            } = .{
                .allocator = allocator,
                .actual = &actual_outs_buffer,
                .current_str = &current_str_buf,
            };

            const actual_result = parseReader(fbs.reader(), full_read_buffer[0..buf_size], ctx);
            if (expected_result) |expected_result_value| {
                try std.testing.expectEqualDeep(expected_result_value, actual_result);
            } else |expected_result_err| {
                try std.testing.expectError(expected_result_err, actual_result);
            }
            errdefer std.log.err("\nExpected: {any}\n\nGot:      {any}", .{ expected_outs, actual_outs_buffer.items });
            try std.testing.expectEqualDeep(expected_outs, actual_outs_buffer.items);
            actual_outs_buffer.clearRetainingCapacity();
        }
    }

    var tokenizer = Tokenizer.initComplete(src);
    var ctx: struct {
        src: []const u8,
        actual: *@TypeOf(actual_outs_buffer),
        /// Parsing a slice guarantees that segments are always whole, despite
        /// passing through the same iterative API as when parsing a reader.
        /// This flag indicates whether or not a range has been fed.
        range_end_pending: bool = false,

        pub fn feedSrc(ctx: *@This(), maybe_range: ?Tokenizer.Range) !void {
            const range = maybe_range orelse {
                try std.testing.expect(ctx.range_end_pending);
                ctx.range_end_pending = false;
                return;
            };
            try std.testing.expect(!ctx.range_end_pending);
            ctx.range_end_pending = true;
            try ctx.actual.append(.{ .str = range.toStr(ctx.src) });
        }

        pub fn feedMarker(ctx: @This(), marker: ParseMarker) !void {
            try std.testing.expect(!ctx.range_end_pending);
            try ctx.actual.append(.{ .marker = marker });
        }
    } = .{
        .src = src,
        .actual = &actual_outs_buffer,
    };
    const actual_result = parseSlice(&tokenizer, &ctx);
    if (expected_result) |expected_result_value| {
        try std.testing.expectEqualDeep(expected_result_value, actual_result);
    } else |expected_result_err| {
        try std.testing.expectError(expected_result_err, actual_result);
    }
    errdefer std.log.err("\nExpected: {any}\nGot: {any}", .{ expected_outs, actual_outs_buffer.items });
    try std.testing.expectEqualDeep(expected_outs, actual_outs_buffer.items);
    actual_outs_buffer.clearRetainingCapacity();
}

test "1" {
    try testParse(
        .eof,
        &.{ 1, 2, 3, 4, 5, 6, 7, 8, 16, 32, 64 },
        \\<?xml version="1.0" encoding="UTF-8"?>
        \\
        \\<!DOCTYPE foo PUBLIC "fizz" 'buzz' [
        \\    <!ELEMENT foo (bar)>
        \\    <!ELEMENT bar (#PCDATA)>
        \\    <!ELEMENT baz EMPTY>
        \\]>
    ,
        &[_]TestParseItem{
            .{ .marker = .pi },
            .{ .str = "xml version=\"1.0\" encoding=\"UTF-8\"" },

            .{ .marker = .dtd_start },
            .{ .str = "foo" },

            .{ .marker = .{ .external_id = .PUBLIC } },
            .{ .str = "fizz" },
            .{ .str = "buzz" },

            .{ .marker = .element_start },
            .{ .str = "foo" },
            .{ .marker = .{ .element_content_spec = .children } },
            .{ .marker = .{ .element_content_spec_token = .lparen } },
            .{ .marker = .{ .element_content_spec_token = .name } },
            .{ .str = "bar" },
            .{ .marker = .{ .element_content_spec_token = .rparen } },
            .{ .marker = .{ .element_end = null } },

            .{ .marker = .element_start },
            .{ .str = "bar" },
            .{ .marker = .{ .element_content_spec = .Mixed } },
            .{ .marker = .{ .element_end = null } },

            .{ .marker = .element_start },
            .{ .str = "baz" },
            .{ .marker = .{ .element_content_spec = .EMPTY } },
            .{ .marker = .{ .element_end = null } },

            .{ .marker = .dtd_end },
        },
    );
    try testParse(
        .eof,
        &@as([34 - 1]usize, std.simd.iota(usize, 34 - 1) + std.simd.repeat(34 - 1, [_]usize{1})),
        \\<?xml version="1.0" encoding="UTF-8"?>
        \\
        \\<!DOCTYPE foo [
        \\ <?phi?>
        \\ <!-- foo -->
        \\
        \\ <!ELEMENT bar EMPTY>
        \\ <!ELEMENT baz (#PCDATA)*>
        \\ <!ELEMENT baz (#PCDATA|todo)*>
        \\ <!ELEMENT baz (phoe,be)+>
        \\ <!ELEMENT baz (a,(e|i),o,u)+>
        \\
        \\ <!ATTLIST foo fizz CDATA #IMPLIED>
        \\ <!ATTLIST foo fizz NOTATION (eao|oae|aeo) #FIXED "eao" >
        \\ <!ATTLIST foo fizz (eao|oae|aeo) "fizz&lt;buzz" buzz IDREF #REQUIRED >
        \\
        \\ <!ENTITY lorem "ipsum">
        \\ <!ENTITY % lorem "ipsum">
        \\ <!ENTITY lorem SYSTEM "c">
        \\ <!ENTITY % lorem SYSTEM "c">
        \\
        \\ <!NOTATION eee SYSTEM "d">
        \\ <!NOTATION eee PUBLIC "e">
        \\ <!NOTATION fff PUBLIC "f" "g">
        \\ <!NOTATION ggg SYSTEM "g">
        \\]>
        \\
    ,
        &[_]TestParseItem{
            .{ .marker = .pi },
            .{ .str = "xml version=\"1.0\" encoding=\"UTF-8\"" },

            .{ .marker = .dtd_start },
            .{ .str = "foo" },

            .{ .marker = .pi },
            .{ .str = "phi" },

            .{ .marker = .element_start },
            .{ .str = "bar" },
            .{ .marker = .{ .element_content_spec = .EMPTY } },
            .{ .marker = .{ .element_end = null } },

            .{ .marker = .element_start },
            .{ .str = "baz" },
            .{ .marker = .{ .element_content_spec = .Mixed } },
            .{ .marker = .{ .element_end = .asterisk } },

            .{ .marker = .element_start },
            .{ .str = "baz" },
            .{ .marker = .{ .element_content_spec = .Mixed } },
            .{ .str = "todo" },
            .{ .marker = .{ .element_end = .asterisk } },

            .{ .marker = .element_start },
            .{ .str = "baz" },
            .{ .marker = .{ .element_content_spec = .children } },
            .{ .marker = .{ .element_content_spec_token = .lparen } },
            .{ .marker = .{ .element_content_spec_token = .name } },
            .{ .str = "phoe" },
            .{ .marker = .{ .element_content_spec_token = .comma } },
            .{ .marker = .{ .element_content_spec_token = .name } },
            .{ .str = "be" },
            .{ .marker = .{ .element_content_spec_token = .rparen } },
            .{ .marker = .{ .element_end = .plus } },

            .{ .marker = .element_start },
            .{ .str = "baz" },
            .{ .marker = .{ .element_content_spec = .children } },
            .{ .marker = .{ .element_content_spec_token = .lparen } },
            .{ .marker = .{ .element_content_spec_token = .name } },
            .{ .str = "a" },

            .{ .marker = .{ .element_content_spec_token = .comma } },

            .{ .marker = .{ .element_content_spec_token = .lparen } },
            .{ .marker = .{ .element_content_spec_token = .name } },
            .{ .str = "e" },
            .{ .marker = .{ .element_content_spec_token = .pipe } },
            .{ .marker = .{ .element_content_spec_token = .name } },
            .{ .str = "i" },
            .{ .marker = .{ .element_content_spec_token = .rparen } },

            .{ .marker = .{ .element_content_spec_token = .comma } },

            .{ .marker = .{ .element_content_spec_token = .name } },
            .{ .str = "o" },

            .{ .marker = .{ .element_content_spec_token = .comma } },

            .{ .marker = .{ .element_content_spec_token = .name } },
            .{ .str = "u" },

            .{ .marker = .{ .element_content_spec_token = .rparen } },
            .{ .marker = .{ .element_end = .plus } },

            .{ .marker = .attlist_start },
            .{ .str = "foo" },
            .{ .marker = .attlist_def_start },
            .{ .str = "fizz" },
            .{ .marker = .{ .attlist_def_type = .CDATA } },
            .{ .marker = .{ .attlist_def_default_decl_start = .IMPLIED } },
            .{ .marker = .attlist_def_default_decl_end },
            .{ .marker = .attlist_end },

            .{ .marker = .attlist_start },
            .{ .str = "foo" },
            .{ .marker = .attlist_def_start },
            .{ .str = "fizz" },
            .{ .marker = .{ .attlist_def_type = .NOTATION } },
            .{ .str = "eao" },
            .{ .str = "oae" },
            .{ .str = "aeo" },
            .{ .marker = .{ .attlist_def_default_decl_start = .FIXED } },
            .{ .str = "eao" },
            .{ .marker = .attlist_def_default_decl_end },
            .{ .marker = .attlist_end },

            .{ .marker = .attlist_start },
            .{ .str = "foo" },
            .{ .marker = .attlist_def_start },
            .{ .str = "fizz" },
            .{ .marker = .{ .attlist_def_type = .enumeration } },
            .{ .str = "eao" },
            .{ .str = "oae" },
            .{ .str = "aeo" },
            .{ .marker = .{ .attlist_def_default_decl_start = null } },
            .{ .str = "fizz" },
            .{ .marker = .{ .reference = .general } },
            .{ .str = "lt" },
            .{ .str = "buzz" },
            .{ .marker = .attlist_def_default_decl_end },
            .{ .marker = .attlist_def_start },
            .{ .str = "buzz" },
            .{ .marker = .{ .attlist_def_type = .IDREF } },
            .{ .marker = .{ .attlist_def_default_decl_start = .REQUIRED } },
            .{ .marker = .attlist_def_default_decl_end },
            .{ .marker = .attlist_end },

            .{ .marker = .{ .entity_start = .general } },
            .{ .str = "lorem" },
            .{ .str = "ipsum" },
            .{ .marker = .entity_end },

            .{ .marker = .{ .entity_start = .parsed } },
            .{ .str = "lorem" },
            .{ .str = "ipsum" },
            .{ .marker = .entity_end },

            .{ .marker = .{ .entity_start = .general } },
            .{ .str = "lorem" },
            .{ .marker = .{ .external_id = .SYSTEM } },
            .{ .str = "c" },
            .{ .marker = .entity_end },

            .{ .marker = .{ .entity_start = .parsed } },
            .{ .str = "lorem" },
            .{ .marker = .{ .external_id = .SYSTEM } },
            .{ .str = "c" },
            .{ .marker = .entity_end },

            .{ .marker = .notation_start },
            .{ .str = "eee" },
            .{ .marker = .{ .external_or_public_id = .SYSTEM } },
            .{ .str = "d" },
            .{ .marker = .notation_end },

            .{ .marker = .notation_start },
            .{ .str = "eee" },
            .{ .marker = .{ .external_or_public_id = .PUBLIC } },
            .{ .str = "e" },
            .{ .marker = .notation_end },

            .{ .marker = .notation_start },
            .{ .str = "fff" },
            .{ .marker = .{ .external_or_public_id = .PUBLIC } },
            .{ .str = "f" },
            .{ .str = "g" },
            .{ .marker = .notation_end },

            .{ .marker = .notation_start },
            .{ .str = "ggg" },
            .{ .marker = .{ .external_or_public_id = .SYSTEM } },
            .{ .str = "g" },
            .{ .marker = .notation_end },

            .{ .marker = .dtd_end },
        },
    );
}
