//! Provides the means to scan through the Internal Subset of a DTD Declaration,
//! starting after the '[' token, and ending with the ']' token.
//! This isn't made to scan the contents of a DTD file.

/// Expects that the last value returned by `tokenizer.nextType(.dtd)`
/// to have been `.square_bracket_left`.
pub inline fn sliceScanner(tokenizer: *Tokenizer) Scanner(null) {
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
        state: State = .blank,
        const Self = @This();

        pub const Src = if (MaybeReader != null)
            []const u8
        else
            Tokenizer.Range;

        pub inline fn nextMarker(scanner: *Self) !ScanMarker {
            return nextMarkerOrSrcImpl(MaybeReader, scanner, .marker);
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
        pub fn nextSrc(scanner: *Self) !?Src {
            return nextMarkerOrSrcImpl(MaybeReader, scanner, .src);
        }
    };
}

pub const ScanError = error{
    UnexpectedToken,
    UnexpectedEof,
    MissingAsterisk,
    EmptyPI,
    EmptyReference,
    InvalidPubidLiteral,
};

pub const ScanMarker = union(enum) {
    /// Encountered ']', terminating the internal subset.
    end,

    /// `nextSrc` will return the PI data.
    pi,

    /// `nextSrc` will return the text data.
    text,

    /// `nextSrc` will return the number string.
    /// Character Reference:
    /// `('&#' [0-9]+ ';') | ('&#x' [0-9a-fA-F]+ ';')`
    reference_char,
    /// `nextSrc` will return the name string.
    /// General Entity Reference:
    /// `'&' Name ';'`
    reference_ge,

    /// `nextSrc` will return the name string.
    /// Parameter Entity Reference:
    /// `'%' Name ';'`
    reference_pe,

    /// First `nextSrc` will return the declared entity's name.
    /// Then `nextMarker` will return either `.entity_def_value` or `.entity_def_external_id`
    entity_decl: EntityKind,
    /// Expect a series of `.text`, `.reference_char`, and/or `.reference_ge`, terminated by `.entity_end`.
    entity_def_value,
    /// Follow the procedure described by the payload to obtain the External ID.
    /// Afterwards, one of the following will occur:
    /// * If `.entity_decl == .general`, `nextMarker` will return `.entity_def_ndata_decl`, or `.entity_end`.
    /// * If `.entity_decl == .parameter`, `nextMarker` will return `.entity_end`.
    entity_def_external_id: ExternalId,
    /// `'NDATA' Name`
    /// `nextSrc` will return the NDataDecl Name.
    entity_def_ndata_decl,
    entity_end,

    /// First `nextSrc` will return the declared element's name.
    /// Then `nextMarker` will return `.content_spec`, with a payload
    /// describing the procedure to obtain the content specification.
    element_decl,
    content_spec: ContentSpec,
    children_tok: ?ChildrenToken,
    mixed_content_spec_end: MixedContentSpecEnd,

    /// First `nextSrc` will return the target element's name.
    /// Then `nextMarker` will either return `.attlist_end`, closing
    /// the attribute list declaration, or `.attribute_name`, beginning
    /// an attribute definition. In the latter case, the second step
    /// described here will repeat after acquiring the attribute definition.
    attlist_decl,
    /// First `nextSrc` will return the declared attribute's name.
    /// Then `nextMarker` will return `.attribute_type`
    attribute_name,
    /// Follow the procedure described by the attribute type tag to acquire
    /// any available information. After that, `nextMarker` will return
    /// `.attribute_default_decl`.
    attribute_type: AttType,
    /// Follow the procedure described by the default decl tag to acquire
    /// the default decl value. Whatever it is, it will be terminated when
    /// `nextMarker` returns `.attribute_name` or `.attlist_end`.
    attribute_default_decl: DefaultDecl,
    attlist_end,

    /// First `nextSrc` will return the declared notation's name.
    notation_decl,
    notation_end,

    pub const EntityKind = enum {
        /// '<!ENTITY Name'
        general,
        /// '<!ENTITY % Name'
        parameter,
    };
    pub const ExternalId = enum {
        /// `'PUBLIC' S PubidLiteral S SystemLiteral`
        /// First `nextSrc` will return the pubid literal.
        /// Second, `nextSrc` will return the system literal.
        public,
        /// `'SYSTEM' S SystemLiteral`
        /// `nextSrc` will return the system literal.
        system,
    };

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
        /// returned in order by `nextSrc`.
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
        /// `nextSrc` returns the source for the name.
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

    pub const AttType = enum {
        //! AttType        ::= StringType | TokenizedType | EnumeratedType
        //! StringType     ::= 'CDATA'
        //! TokenizedType  ::= 'ID'
        //!                  | 'IDREF'
        //!                  | 'IDREFS'
        //!                  | 'ENTITY'
        //!                  | 'ENTITIES'
        //!                  | 'NMTOKEN'
        //!                  | 'NMTOKENS'
        //! EnumeratedType ::= NotationType | Enumeration
        //! NotationType   ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
        //! Enumeration    ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'

        cdata,

        id,
        idref,
        idrefs,
        entity,
        entities,
        nmtoken,
        nmtokens,

        /// `nextSrc` will return a list of `Name`s.
        notation,
        /// `nextSrc` will return a list of `Nmtoken`s.
        enumeration,
    };

    pub const DefaultDecl = enum {
        /// `'#REQUIRED'`
        required,
        /// `'#IMPLIED'`
        implied,
        /// `'#FIXED' S AttValue`
        /// Followed by a series of `.text`, `.ge_reference`, and `.char_reference`, terminated by `.attribute_name` or `.attlist_end`.
        fixed,
        /// `AttValue`
        /// Followed by a series of `.text`, `.ge_reference`, and `.char_reference`, terminated by `.attribute_name` or `.attlist_end`.
        value,
    };
};

const State = union(enum) {
    end,
    blank,

    pi: enum { in_progress, done },

    pe_reference_name,
    pe_reference_end,

    entity: struct {
        data: union {
            none: void,

            maybe_need_ent_kind: ScanMarker.EntityKind,

            value_quoted_cached_tt: struct { Tokenizer.QuoteType, ?Tokenizer.TokenType },
            value_quoted: Tokenizer.QuoteType,

            extid_lit: struct { Tokenizer.QuoteType, ScanMarker.EntityKind },
        },
        state: enum {
            name_start,
            name_end,
            def_detect_type,

            def_empty_start,
            def_empty_end,

            def_value_detect_type,

            def_value_text_start,
            def_value_text_end,

            def_value_ref_start,
            def_value_ref_end,

            def_extid_pub_literal_empty_start,
            def_extid_pub_literal_empty_end,
            def_extid_pub_literal_start,
            def_extid_pub_literal_end,

            def_extid_sys_literal_empty_start,
            def_extid_sys_literal_empty_end,
            def_extid_sys_literal_start,
            def_extid_sys_literal_end,

            def_ndata_decl_start,
            def_ndata_decl_end,
        },
    },
    element: enum { start, name_end },
    attlist: enum { start, name_end },
    notation: enum { start, name_end },
};

fn nextMarkerOrSrcImpl(
    comptime MaybeReader: ?type,
    scanner: *Scanner(MaybeReader),
    comptime ret_type: enum { marker, src },
) !switch (ret_type) {
    .marker => ScanMarker,
    .src => ?Scanner(MaybeReader).Src,
} {
    const tokenizer: *Tokenizer = scanner.tokenizer;
    const mbr = scanner.mbr;
    return while (true) break switch (scanner.state) {
        .end => switch (ret_type) {
            .marker => break .end,
            .src => unreachable,
        },
        .blank => switch (ret_type) {
            .marker => switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,
                .tag_whitespace => unreachable,

                .comment_start => switch (try parse_helper.handleCommentSkip(tokenizer, MaybeReader, mbr)) {
                    .normal_end => continue,
                    .invalid_end_triple_dash => return ScanError.UnexpectedToken,
                    .invalid_dash_dash => return ScanError.UnexpectedToken,
                    .eof => return ScanError.UnexpectedEof,
                },

                .square_bracket_right => {
                    scanner.state = .end;
                    break .end;
                },

                .pi_start => switch (try parse_helper.nextTokenType(tokenizer, .pi, MaybeReader, mbr)) {
                    else => unreachable,
                    .eof => return ScanError.UnexpectedEof,
                    .pi_end => return ScanError.EmptyPI,
                    .text_data => {
                        scanner.state = .{ .pi = .in_progress };
                        break .pi;
                    },
                },

                .percent => {
                    switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
                        else => unreachable,
                        .eof => return ScanError.UnexpectedEof,
                        .hashtag => return ScanError.UnexpectedToken,
                        .invalid_reference_end => return ScanError.UnexpectedToken,
                        .semicolon => return ScanError.EmptyPI,
                        .tag_token => {},
                    }
                    scanner.state = .pe_reference_name;
                    break .reference_pe;
                },

                .dtd_decl => {
                    const DeclStr = enum { @"<!ENTITY", @"<!ELEMENT", @"<!ATTLIST", @"<!NOTATION" };
                    const decl_str = (try parse_helper.nextTokenSrcAsEnum(tokenizer, .dtd, MaybeReader, mbr, DeclStr)) orelse return ScanError.UnexpectedToken;

                    if (try parse_helper.skipIfTagWhitespaceOrGetNextTokType(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
                        .tag_whitespace => unreachable,
                        .tag_token => unreachable,
                        .eof => return ScanError.UnexpectedEof,
                        else => return ScanError.UnexpectedToken,
                    };

                    // true if we find `.percent`, `.tag_token` ('%' Name),
                    // false if we just find `.tag_token` (Name).
                    // this is an error in all cases except for '<!ENTITY',
                    // it's just simpler to get this information and ensure the
                    // presence of the name before going forward.
                    const percent: bool = percent: {
                        switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                            else => return ScanError.UnexpectedToken,
                            .eof => return ScanError.UnexpectedEof,
                            .tag_whitespace => unreachable,
                            .tag_token => break :percent false,
                            .percent => {},
                        }
                        if (try parse_helper.skipIfTagWhitespaceOrGetNextTokType(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
                            .tag_whitespace => unreachable,
                            .tag_token => unreachable,
                            .eof => return ScanError.UnexpectedEof,
                            else => return ScanError.UnexpectedToken,
                        };
                        switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                            else => return ScanError.UnexpectedToken,
                            .eof => return ScanError.UnexpectedEof,
                            .tag_whitespace => unreachable,
                            .tag_token => {},
                        }
                        break :percent true;
                    };

                    switch (decl_str) {
                        .@"<!ENTITY" => {
                            const ent_kind: ScanMarker.EntityKind = if (percent) .parameter else .general;
                            scanner.state = .{ .entity = .{
                                .data = .{ .maybe_need_ent_kind = ent_kind },
                                .state = .name_start,
                            } };
                            break .{ .entity_decl = ent_kind };
                        },
                        .@"<!ELEMENT" => {
                            if (percent) return ScanError.UnexpectedToken;
                            scanner.state = .{ .element = .start };
                            break .element_decl;
                        },
                        .@"<!ATTLIST" => {
                            if (percent) return ScanError.UnexpectedToken;
                            scanner.state = .{ .attlist = .start };
                            break .attlist_decl;
                        },
                        .@"<!NOTATION" => {
                            if (percent) return ScanError.UnexpectedToken;
                            scanner.state = .{ .notation = .start };
                            break .notation_decl;
                        },
                    }
                },
            },
            .src => unreachable,
        },

        .pi => |*state| switch (state.*) {
            .in_progress => {
                if (ret_type != .src) unreachable;
                if (MaybeReader != null) {
                    if (try parse_helper.nextTokenSegment(tokenizer, .pi, mbr.reader, mbr.read_buffer)) |segment| break segment;
                    state.* = .done;
                    continue;
                } else {
                    state.* = .done;
                    break tokenizer.nextSrcNoUnderrun(.pi);
                }
            },
            .done => {
                if (ret_type != .src) unreachable;
                switch (try parse_helper.nextTokenType(tokenizer, .pi, MaybeReader, mbr)) {
                    else => return ScanError.UnexpectedToken,
                    .eof => return ScanError.UnexpectedEof,
                    .pi_end => {},
                }
                scanner.state = .blank;
                break null;
            },
        },

        .pe_reference_name => {
            if (ret_type != .src) unreachable;
            if (MaybeReader != null) {
                if (try parse_helper.nextTokenSegment(tokenizer, .reference, mbr.reader, mbr.read_buffer)) |segment| break segment;
                scanner.state = .pe_reference_end;
                continue;
            } else {
                scanner.state = .pe_reference_end;
                break tokenizer.nextSrcNoUnderrun(.reference);
            }
        },
        .pe_reference_end => {
            if (ret_type != .src) unreachable;
            switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
                else => unreachable,
                .tag_token => unreachable,
                .eof => return ScanError.UnexpectedEof,
                .hashtag => return ScanError.UnexpectedToken,
                .invalid_reference_end => return ScanError.UnexpectedToken,
                .semicolon => {},
            }
            scanner.state = .blank;
            break null;
        },

        .entity => |*payload| switch (payload.state) {
            .name_start => {
                if (ret_type != .src) unreachable;
                _ = payload.data.maybe_need_ent_kind;
                if (MaybeReader != null) {
                    if (try parse_helper.nextTokenSegment(tokenizer, .dtd, mbr.reader, mbr.read_buffer)) |segment| break segment;
                    payload.state = .name_end;
                    continue;
                } else {
                    payload.state = .name_end;
                    break tokenizer.nextSrcNoUnderrun(.dtd);
                }
            },
            .name_end => {
                if (ret_type != .src) unreachable;
                _ = payload.data.maybe_need_ent_kind;
                payload.state = .def_detect_type;
                break null;
            },

            .def_detect_type => {
                if (ret_type != .marker) unreachable;
                const ent_kind = payload.data.maybe_need_ent_kind;

                if (try parse_helper.skipIfTagWhitespaceOrGetNextTokType(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
                    .tag_whitespace => unreachable,
                    .tag_token => unreachable,
                    .eof => return ScanError.UnexpectedEof,
                    else => return ScanError.UnexpectedToken,
                };

                switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                    else => return ScanError.UnexpectedToken,
                    .eof => return ScanError.UnexpectedEof,
                    .tag_whitespace => unreachable,

                    inline .quote_single, .quote_double => |quote_tt| {
                        const quote_type = comptime Tokenizer.QuoteType.fromTokenType(quote_tt).?;
                        payload.state, //
                        payload.data //
                        = switch (try parse_helper.nextTokenType(tokenizer, quote_type.entityValueCtx(), MaybeReader, mbr)) {
                            else => unreachable,
                            .eof => return ScanError.UnexpectedEof,
                            .quote_single, .quote_double => .{
                                .def_empty_start,
                                .{ .none = {} },
                            },
                            .text_data, .ampersand => |cached_tt| .{
                                .def_value_detect_type,
                                .{ .value_quoted_cached_tt = .{ quote_type, cached_tt } },
                            },
                        };
                        break .entity_def_value;
                    },

                    .tag_token => {
                        const external_id_tag: ScanMarker.ExternalId = extid_tag: {
                            const TagStr = enum { PUBLIC, SYSTEM };
                            const maybe_tag_str = (try parse_helper.nextTokenSrcAsEnum(tokenizer, .dtd, MaybeReader, mbr, TagStr));
                            const tag_str = maybe_tag_str orelse return ScanError.UnexpectedToken;
                            break :extid_tag switch (tag_str) {
                                .PUBLIC => .public,
                                .SYSTEM => .system,
                            };
                        };

                        if (try parse_helper.skipIfTagWhitespaceOrGetNextTokType(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
                            .tag_whitespace => unreachable,
                            .tag_token => unreachable,
                            .eof => return ScanError.UnexpectedEof,
                            else => return ScanError.UnexpectedToken,
                        };

                        const quote_type: Tokenizer.QuoteType = switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                            else => return ScanError.UnexpectedToken,
                            .eof => return ScanError.UnexpectedEof,
                            .tag_whitespace => unreachable,

                            .quote_single => .single,
                            .quote_double => .double,
                        };

                        payload.state, payload.data = switch (try parse_helper.nextTokenType(tokenizer, quote_type.systemLiteralCtx(), MaybeReader, mbr)) {
                            else => unreachable,
                            .eof => return ScanError.UnexpectedEof,
                            .quote_single, .quote_double => switch (external_id_tag) {
                                .public => .{ .def_extid_pub_literal_empty_start, .{ .maybe_need_ent_kind = ent_kind } },
                                .system => .{ .def_extid_sys_literal_empty_start, .{ .maybe_need_ent_kind = ent_kind } },
                            },
                            .text_data => switch (external_id_tag) {
                                .public => .{ .def_extid_pub_literal_start, .{ .extid_lit = .{ quote_type, ent_kind } } },
                                .system => .{ .def_extid_sys_literal_start, .{ .extid_lit = .{ quote_type, ent_kind } } },
                            },
                        };
                        break .{ .entity_def_external_id = external_id_tag };
                    },
                }
            },

            .def_empty_start => switch (ret_type) {
                .marker => break .text,
                .src => {
                    payload.state = .def_empty_end;
                    break if (MaybeReader != null) "" else .{ .start = tokenizer.index - 1, .end = tokenizer.index - 1 };
                },
            },
            .def_empty_end => switch (ret_type) {
                .src => break null,
                .marker => {
                    switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                        else => return ScanError.UnexpectedToken,
                        .eof => return ScanError.UnexpectedEof,
                        .tag_whitespace => unreachable,
                        .angle_bracket_right => {},
                    }
                    scanner.state = .blank;
                    break .entity_end;
                },
            },

            .def_value_detect_type => {
                if (ret_type != .marker) unreachable;
                const quote_type, var maybe_cached_tt = payload.data.value_quoted_cached_tt;
                payload.data = .{ .value_quoted = quote_type };

                switch (blk: {
                    defer maybe_cached_tt = null;
                    break :blk maybe_cached_tt orelse try parse_helper.nextTokenType(tokenizer, quote_type.entityValueCtx(), MaybeReader, mbr);
                }) {
                    else => unreachable,
                    .eof => return ScanError.UnexpectedEof,
                    .text_data => {
                        payload.state = .def_value_text_start;
                        break .text;
                    },
                    .ampersand => {
                        payload.state = .def_value_ref_start;
                        break switch (try getReferenceTypeDeterminedUpToTagToken(tokenizer, MaybeReader, mbr)) {
                            .eof => return ScanError.UnexpectedEof,
                            .empty => return ScanError.EmptyReference,
                            .invalid_end => return ScanError.UnexpectedToken,
                            .unexpected_hashtag => return ScanError.UnexpectedToken,

                            .char_reference => .reference_char,
                            .ge_reference => .reference_ge,
                        };
                    },
                    .quote_single, .quote_double => {
                        switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                            else => unreachable,
                            .eof => return ScanError.UnexpectedEof,
                            .tag_whitespace => unreachable,
                            .angle_bracket_right => {},
                        }
                        scanner.state = .blank;
                        break .entity_end;
                    },
                }
            },

            .def_value_text_start => {
                if (ret_type != .src) unreachable;
                const quote_type = payload.data.value_quoted;
                const str_ctx = quote_type.entityValueCtx();

                if (MaybeReader != null) {
                    if (try parse_helper.nextTokenSegment(tokenizer, str_ctx, mbr.reader, mbr.read_buffer)) |segment| break segment;
                    payload.state = .def_value_text_end;
                    continue;
                } else {
                    payload.state = .def_value_text_end;
                    break tokenizer.nextSrcNoUnderrun(str_ctx);
                }
            },
            .def_value_text_end => {
                if (ret_type != .src) unreachable;
                const quote_type = payload.data.value_quoted;
                payload.data = .{ .value_quoted_cached_tt = .{ quote_type, null } };
                payload.state = .def_value_detect_type;
                break null;
            },

            .def_value_ref_start => {
                if (ret_type != .src) unreachable;
                _ = payload.data.value_quoted;

                if (MaybeReader != null) {
                    if (try parse_helper.nextTokenSegment(tokenizer, .reference, mbr.reader, mbr.read_buffer)) |segment| break segment;
                    payload.state = .def_value_ref_end;
                    continue;
                } else {
                    payload.state = .def_value_ref_end;
                    break tokenizer.nextSrcNoUnderrun(.reference);
                }
            },
            .def_value_ref_end => {
                if (ret_type != .src) unreachable;
                const quote_type = payload.data.value_quoted;
                switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
                    else => unreachable,
                    .tag_token => unreachable,
                    .eof => return ScanError.UnexpectedEof,
                    .hashtag => return ScanError.UnexpectedToken,
                    .invalid_reference_end => return ScanError.UnexpectedToken,
                    .semicolon => {},
                }
                payload.data = .{ .value_quoted_cached_tt = .{ quote_type, null } };
                payload.state = .def_value_detect_type;
                break null;
            },

            .def_extid_pub_literal_empty_start => {
                if (ret_type != .src) unreachable;
                _ = payload.data.maybe_need_ent_kind;
                payload.state = .def_extid_pub_literal_empty_end;
                break if (MaybeReader != null) "" else .{ .start = tokenizer.index - 1, .end = tokenizer.index - 1 };
            },
            .def_extid_pub_literal_empty_end => {
                if (ret_type != .src) unreachable;
                const ent_kind = payload.data.maybe_need_ent_kind;

                if (try parse_helper.skipIfTagWhitespaceOrGetNextTokType(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
                    .tag_whitespace => unreachable,
                    .eof => return ScanError.UnexpectedEof,
                    else => return ScanError.UnexpectedToken,
                };

                const quote_type = switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                    else => return ScanError.UnexpectedToken,
                    .eof => return ScanError.UnexpectedEof,
                    inline //
                    .quote_single,
                    .quote_double,
                    => |quote_tt| comptime Tokenizer.QuoteType.fromTokenType(quote_tt).?,
                };

                payload.state, payload.data = switch (try parse_helper.nextTokenType(tokenizer, quote_type.systemLiteralCtx(), MaybeReader, mbr)) {
                    else => unreachable,
                    .eof => return ScanError.UnexpectedEof,
                    .quote_single, .quote_double => .{ .def_extid_sys_literal_empty_start, .{ .maybe_need_ent_kind = ent_kind } },
                    .text_data => .{ .def_extid_sys_literal_start, .{ .extid_lit = .{ quote_type, ent_kind } } },
                };

                break null;
            },

            .def_extid_pub_literal_start => {
                if (ret_type != .src) unreachable;
                const quote_type, _ = payload.data.extid_lit;
                if (MaybeReader != null) {
                    if (try parse_helper.nextTokenSegment(tokenizer, quote_type.systemLiteralCtx(), mbr.reader, mbr.read_buffer)) |segment| {
                        if (!validPubidLiteralSegment(segment, quote_type)) return ScanError.InvalidPubidLiteral;
                        break segment;
                    }
                    payload.state = .def_extid_pub_literal_end;
                    continue;
                } else {
                    const range = tokenizer.nextSrcNoUnderrun(quote_type.systemLiteralCtx());
                    if (!validPubidLiteralSegment(range.toStr(tokenizer.src), quote_type)) return ScanError.InvalidPubidLiteral;
                    payload.state = .def_extid_pub_literal_end;
                    break range;
                }
            },
            .def_extid_pub_literal_end => {
                if (ret_type != .src) unreachable;
                const quote_type, const ent_kind = payload.data.extid_lit;

                switch (try parse_helper.nextTokenType(tokenizer, quote_type.systemLiteralCtx(), MaybeReader, mbr)) {
                    else => unreachable,
                    .text_data => unreachable,
                    .eof => return ScanError.UnexpectedEof,
                    .quote_single, .quote_double => |close_quote_tt| assert(quote_type.toTokenType() == close_quote_tt),
                }

                // we re-use the `.def_extid_pub_literal_empty_end` prong for the same purpose, as the code is identical
                // to what would otherwise be here.
                payload.data = .{ .maybe_need_ent_kind = ent_kind };
                payload.state = .def_extid_pub_literal_empty_end;
                continue;
            },

            .def_extid_sys_literal_empty_start => {
                if (ret_type != .src) unreachable;
                _ = payload.data.maybe_need_ent_kind;
                payload.state = .def_extid_sys_literal_empty_end;
                break if (MaybeReader != null) "" else .{ .start = tokenizer.index - 1, .end = tokenizer.index - 1 };
            },
            .def_extid_sys_literal_empty_end => {
                if (ret_type != .src) unreachable;
                // we skip the `.src` branch of the `.def_extid_sys_literal_end` prong, going
                // right to the logic for detecting the end, with the same quoteless data.
                _ = payload.data.maybe_need_ent_kind;
                payload.state = .def_extid_sys_literal_end;
                break null;
            },

            .def_extid_sys_literal_start => {
                if (ret_type != .src) unreachable;
                const quote_type, _ = payload.data.extid_lit;
                if (MaybeReader != null) {
                    if (try parse_helper.nextTokenSegment(tokenizer, quote_type.systemLiteralCtx(), mbr.reader, mbr.read_buffer)) |segment| break segment;
                    payload.state = .def_extid_sys_literal_end;
                    continue;
                } else {
                    payload.state = .def_extid_sys_literal_end;
                    break tokenizer.nextSrcNoUnderrun(quote_type.systemLiteralCtx());
                }
            },
            .def_extid_sys_literal_end => switch (ret_type) {
                .src => {
                    const quote_type, const ent_kind = payload.data.extid_lit;
                    switch (try parse_helper.nextTokenType(tokenizer, quote_type.systemLiteralCtx(), MaybeReader, mbr)) {
                        else => unreachable,
                        .text_data => unreachable,
                        .eof => return ScanError.UnexpectedEof,
                        .quote_single, .quote_double => |close_quote_tt| assert(quote_type.toTokenType() == close_quote_tt),
                    }
                    payload.data = .{ .maybe_need_ent_kind = ent_kind };
                    break null;
                },

                .marker => {
                    const ent_kind = payload.data.maybe_need_ent_kind;
                    const EncounteredTokenTypes = enum { angle_bracket_right, @"tag_whitespace,tag_token" };
                    const encountered_tts: EncounteredTokenTypes = switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                        else => return ScanError.UnexpectedToken,
                        .eof => return ScanError.UnexpectedEof,
                        .tag_whitespace => blk: {
                            try parse_helper.skipWhitespaceSrcUnchecked(tokenizer, .dtd, MaybeReader, mbr);
                            break :blk switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                else => return ScanError.UnexpectedToken,
                                .eof => return ScanError.UnexpectedEof,
                                .tag_whitespace => unreachable,

                                .tag_token => .@"tag_whitespace,tag_token",
                                .angle_bracket_right => .angle_bracket_right,
                            };
                        },
                        .angle_bracket_right => .angle_bracket_right,
                    };

                    switch (encountered_tts) {
                        .@"tag_whitespace,tag_token" => {
                            switch (ent_kind) {
                                .parameter => return ScanError.UnexpectedToken,
                                .general => {},
                            }

                            if (try parse_helper.nextTokenSrcAsEnum(tokenizer, .dtd, MaybeReader, mbr, enum { NDATA }) == null) {
                                return ScanError.UnexpectedToken;
                            }
                            if (try parse_helper.skipIfTagWhitespaceOrGetNextTokType(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
                                .tag_whitespace => unreachable,
                                .tag_token => unreachable,
                                .eof => return ScanError.UnexpectedEof,
                                else => return ScanError.UnexpectedToken,
                            };

                            switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                else => return ScanError.UnexpectedToken,
                                .eof => return ScanError.UnexpectedEof,
                                .tag_whitespace => unreachable,
                                .tag_token => {},
                            }

                            payload.data = .{ .none = {} };
                            payload.state = .def_ndata_decl_start;
                            break .entity_def_ndata_decl;
                        },

                        .angle_bracket_right => {
                            scanner.state = .blank;
                            break .entity_end;
                        },
                    }
                },
            },

            .def_ndata_decl_start => {
                if (ret_type != .src) unreachable;
                _ = payload.data.none;

                if (MaybeReader != null) {
                    if (try parse_helper.nextTokenSegment(tokenizer, .dtd, mbr.reader, mbr.read_buffer)) |segment| break segment;
                    payload.state = .def_ndata_decl_end;
                    continue;
                } else {
                    payload.state = .def_ndata_decl_end;
                    break tokenizer.nextSrcNoUnderrun(.dtd);
                }
            },
            .def_ndata_decl_end => {
                _ = payload.data.none;
                switch (ret_type) {
                    .src => break null,
                    .marker => {
                        switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                            else => unreachable,
                            .tag_whitespace => unreachable,
                            .eof => return ScanError.UnexpectedEof,
                            .angle_bracket_right => {},
                        }
                        scanner.state = .blank;
                        break .entity_end;
                    },
                }
            },
        },

        .element => |*state| switch (state.*) {
            .start => @panic("TODO"),
            .name_end => @panic("TODO"),
        },

        .attlist => |*state| switch (state.*) {
            .start => @panic("TODO"),
            .name_end => @panic("TODO"),
        },

        .notation => |*state| switch (state.*) {
            .start => @panic("TODO"),
            .name_end => @panic("TODO"),
        },
    };
}

fn validPubidLiteralSegment(
    str: []const u8,
    /// The surrounding quote type.
    quote: Tokenizer.QuoteType,
) bool {
    return for (str) |char| switch (char) {
        '\u{20}', '\u{D}', '\u{A}' => {},
        'a'...'z', 'A'...'Z', '0'...'9' => {},
        '-' => {},
        '\'' => switch (quote) {
            .double => {},
            .single => break false,
        },
        '(', ')', '+', ',', '.', '/', ':', '=', '?', ';', '!', '*', '#', '@', '$', '_', '%' => {},
        else => break false,
    } else true;
}

const ReferenceTypeDeterminedUpToTagToken = enum {
    eof,
    empty,
    invalid_end,
    unexpected_hashtag,

    ge_reference,
    char_reference,
};
/// Based the token types returned up until getting `.tag_token`,
/// returns the reference kind, or another tag in case of an error
/// before obtaining `.tag_token`.
fn getReferenceTypeDeterminedUpToTagToken(
    tokenizer: *Tokenizer,
    comptime MaybeReader: ?type,
    mbr: parse_helper.MaybeBufferedReader(MaybeReader),
) !ReferenceTypeDeterminedUpToTagToken {
    return switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
        .eof => .eof,
        .semicolon => .empty,
        .invalid_reference_end => .invalid_end,
        .hashtag => switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
            else => unreachable,
            .eof => .eof,
            .semicolon => .empty,
            .invalid_reference_end => .invalid_end,
            .hashtag => .unexpected_hashtag,

            .tag_token => .char_reference,
        },
        .tag_token => .ge_reference,
        else => unreachable,
    };
}

const ScannerTestItem = union(enum) {
    marker: ScanMarker,
    str: ?[]const u8,
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
            assert(parse_helper.nextTokenType(&tokenizer, .dtd, @TypeOf(fbs.reader()), .{
                .reader = fbs.reader(),
                .read_buffer = read_buffer,
            }) catch unreachable == .square_bracket_left);

            var scanner = readerScanner(&tokenizer, fbs.reader(), read_buffer);
            for (expected_items, 0..) |expected_item, i| {
                errdefer std.log.err("Error occurred on item {d}", .{i});
                switch (expected_item) {
                    .marker => |marker| try std.testing.expectEqual(marker, scanner.nextMarker()),
                    .str => |expected_str| {
                        const actual_str: ?[]const u8 = blk: {
                            try str_buffer.appendSlice((try scanner.nextSrc()) orelse break :blk null);
                            while (try scanner.nextSrc()) |segment| {
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

    var scanner = sliceScanner(&tokenizer);
    for (expected_items, 0..) |expected_item, i| {
        errdefer std.log.err("Error occurred on item {d}", .{i});
        switch (expected_item) {
            .marker => |marker| try std.testing.expectEqual(marker, scanner.nextMarker()),
            .str => |expected_str| {
                const actual_str: ?[]const u8 = blk: {
                    try str_buffer.appendSlice(((try scanner.nextSrc()) orelse break :blk null).toStr(tokenizer.src));
                    while (try scanner.nextSrc()) |segment| {
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

test "ENTITY" {
    const buf_sizes = [_]usize{
        1,  2,  3,  4,  5,  6,  7,  8,  10,  12,  14,  16,  20,  24,
        28, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256,
    };
    try testScanner(&buf_sizes, "[ <!ENTITY % empty ''> <!ENTITY lt '&#38;#60;'> ]", &[_]ScannerTestItem{
        .{ .marker = .{ .entity_decl = .parameter } },
        .{ .str = "empty" },
        .{ .marker = .entity_def_value },
        .{ .marker = .text },
        .{ .str = "" },
        .{ .marker = .entity_end },

        .{ .marker = .{ .entity_decl = .general } },
        .{ .str = "lt" },
        .{ .marker = .entity_def_value },
        .{ .marker = .reference_char },
        .{ .str = "38" },
        .{ .marker = .text },
        .{ .str = "#60;" },
        .{ .marker = .entity_end },
    });

    try testScanner(
        &buf_sizes,
        \\[
        \\  <!ENTITY ent_a 'foo&bar;fizz'>
        \\  <!ENTITY ent_b '&foo;bar&fizz;'>
        \\  <!ENTITY ent_c "&bar;fizz">
        \\  <!ENTITY ent_d "fizz&bar;">
        \\  <!ENTITY % ent_e "">
        \\]
    ,
        &[_]ScannerTestItem{
            .{ .marker = .{ .entity_decl = .general } },
            .{ .str = "ent_a" },
            .{ .marker = .entity_def_value },
            .{ .marker = .text },
            .{ .str = "foo" },
            .{ .marker = .reference_ge },
            .{ .str = "bar" },
            .{ .marker = .text },
            .{ .str = "fizz" },
            .{ .marker = .entity_end },

            .{ .marker = .{ .entity_decl = .general } },
            .{ .str = "ent_b" },
            .{ .marker = .entity_def_value },
            .{ .marker = .reference_ge },
            .{ .str = "foo" },
            .{ .marker = .text },
            .{ .str = "bar" },
            .{ .marker = .reference_ge },
            .{ .str = "fizz" },
            .{ .marker = .entity_end },

            .{ .marker = .{ .entity_decl = .general } },
            .{ .str = "ent_c" },
            .{ .marker = .entity_def_value },
            .{ .marker = .reference_ge },
            .{ .str = "bar" },
            .{ .marker = .text },
            .{ .str = "fizz" },
            .{ .marker = .entity_end },

            .{ .marker = .{ .entity_decl = .general } },
            .{ .str = "ent_d" },
            .{ .marker = .entity_def_value },
            .{ .marker = .text },
            .{ .str = "fizz" },
            .{ .marker = .reference_ge },
            .{ .str = "bar" },
            .{ .marker = .entity_end },

            .{ .marker = .{ .entity_decl = .parameter } },
            .{ .str = "ent_e" },
            .{ .marker = .entity_def_value },
            .{ .marker = .text },
            .{ .str = "" },
            .{ .marker = .entity_end },
        },
    );

    try testScanner(
        &buf_sizes,
        \\[
        \\  <!ENTITY ent_a SYSTEM "foo">
        \\  <!ENTITY ent_b SYSTEM "bar" NDATA baz>
        \\  <!ENTITY ent_c SYSTEM "">
        \\  <!ENTITY ent_d SYSTEM "" NDATA abc>
        \\  <!ENTITY % ent_e SYSTEM "foo">
        \\  <!ENTITY % ent_f SYSTEM "">
        \\]
    ,
        &[_]ScannerTestItem{
            .{ .marker = .{ .entity_decl = .general } },
            .{ .str = "ent_a" },
            .{ .marker = .{ .entity_def_external_id = .system } },
            .{ .str = "foo" },
            .{ .marker = .entity_end },

            .{ .marker = .{ .entity_decl = .general } },
            .{ .str = "ent_b" },
            .{ .marker = .{ .entity_def_external_id = .system } },
            .{ .str = "bar" },
            .{ .marker = .entity_def_ndata_decl },
            .{ .str = "baz" },
            .{ .marker = .entity_end },

            .{ .marker = .{ .entity_decl = .general } },
            .{ .str = "ent_c" },
            .{ .marker = .{ .entity_def_external_id = .system } },
            .{ .str = "" },
            .{ .marker = .entity_end },

            .{ .marker = .{ .entity_decl = .general } },
            .{ .str = "ent_d" },
            .{ .marker = .{ .entity_def_external_id = .system } },
            .{ .str = "" },
            .{ .marker = .entity_def_ndata_decl },
            .{ .str = "abc" },
            .{ .marker = .entity_end },

            .{ .marker = .{ .entity_decl = .parameter } },
            .{ .str = "ent_e" },
            .{ .marker = .{ .entity_def_external_id = .system } },
            .{ .str = "foo" },
            .{ .marker = .entity_end },

            .{ .marker = .{ .entity_decl = .parameter } },
            .{ .str = "ent_f" },
            .{ .marker = .{ .entity_def_external_id = .system } },
            .{ .str = "" },
            .{ .marker = .entity_end },
        },
    );

    try testScanner(
        &buf_sizes,
        \\[
        \\  <!ENTITY ent_a PUBLIC "foo" "bar">
        \\  <!ENTITY ent_b PUBLIC "fizz" "buzz" NDATA baz>
        \\
        \\  <!ENTITY ent_c PUBLIC "" "bar">
        \\  <!ENTITY ent_d PUBLIC "" "buzz" NDATA baz>
        \\
        \\  <!ENTITY ent_e PUBLIC "foo" "">
        \\  <!ENTITY ent_f PUBLIC "fizz" "" NDATA baz>
        \\
        \\  <!ENTITY ent_g PUBLIC "" "">
        \\  <!ENTITY ent_h PUBLIC "" "" NDATA baz>
        \\
        \\  <!ENTITY % ent_i PUBLIC "foo" "bar">
        \\  <!ENTITY % ent_j PUBLIC "foo" "">
        \\  <!ENTITY % ent_k PUBLIC "" "bar">
        \\  <!ENTITY % ent_l PUBLIC "" "">
        \\]
    ,
        &[_]ScannerTestItem{
            .{ .marker = .{ .entity_decl = .general } },
            .{ .str = "ent_a" },
            .{ .marker = .{ .entity_def_external_id = .public } },
            .{ .str = "foo" },
            .{ .str = "bar" },
            .{ .marker = .entity_end },

            .{ .marker = .{ .entity_decl = .general } },
            .{ .str = "ent_b" },
            .{ .marker = .{ .entity_def_external_id = .public } },
            .{ .str = "fizz" },
            .{ .str = "buzz" },
            .{ .marker = .entity_def_ndata_decl },
            .{ .str = "baz" },
            .{ .marker = .entity_end },

            .{ .marker = .{ .entity_decl = .general } },
            .{ .str = "ent_c" },
            .{ .marker = .{ .entity_def_external_id = .public } },
            .{ .str = "" },
            .{ .str = "bar" },
            .{ .marker = .entity_end },

            .{ .marker = .{ .entity_decl = .general } },
            .{ .str = "ent_d" },
            .{ .marker = .{ .entity_def_external_id = .public } },
            .{ .str = "" },
            .{ .str = "buzz" },
            .{ .marker = .entity_def_ndata_decl },
            .{ .str = "baz" },
            .{ .marker = .entity_end },

            .{ .marker = .{ .entity_decl = .general } },
            .{ .str = "ent_e" },
            .{ .marker = .{ .entity_def_external_id = .public } },
            .{ .str = "foo" },
            .{ .str = "" },
            .{ .marker = .entity_end },

            .{ .marker = .{ .entity_decl = .general } },
            .{ .str = "ent_f" },
            .{ .marker = .{ .entity_def_external_id = .public } },
            .{ .str = "fizz" },
            .{ .str = "" },
            .{ .marker = .entity_def_ndata_decl },
            .{ .str = "baz" },
            .{ .marker = .entity_end },

            .{ .marker = .{ .entity_decl = .general } },
            .{ .str = "ent_g" },
            .{ .marker = .{ .entity_def_external_id = .public } },
            .{ .str = "" },
            .{ .str = "" },
            .{ .marker = .entity_end },

            .{ .marker = .{ .entity_decl = .general } },
            .{ .str = "ent_h" },
            .{ .marker = .{ .entity_def_external_id = .public } },
            .{ .str = "" },
            .{ .str = "" },
            .{ .marker = .entity_def_ndata_decl },
            .{ .str = "baz" },
            .{ .marker = .entity_end },

            .{ .marker = .{ .entity_decl = .parameter } },
            .{ .str = "ent_i" },
            .{ .marker = .{ .entity_def_external_id = .public } },
            .{ .str = "foo" },
            .{ .str = "bar" },
            .{ .marker = .entity_end },

            .{ .marker = .{ .entity_decl = .parameter } },
            .{ .str = "ent_j" },
            .{ .marker = .{ .entity_def_external_id = .public } },
            .{ .str = "foo" },
            .{ .str = "" },
            .{ .marker = .entity_end },

            .{ .marker = .{ .entity_decl = .parameter } },
            .{ .str = "ent_k" },
            .{ .marker = .{ .entity_def_external_id = .public } },
            .{ .str = "" },
            .{ .str = "bar" },
            .{ .marker = .entity_end },

            .{ .marker = .{ .entity_decl = .parameter } },
            .{ .str = "ent_l" },
            .{ .marker = .{ .entity_def_external_id = .public } },
            .{ .str = "" },
            .{ .str = "" },
            .{ .marker = .entity_end },
        },
    );
}

test "ELEMENT" {
    if (true) return error.SkipZigTest;
    const buf_sizes = [_]usize{
        1,  2,  3,  4,  5,  6,  7,  8,  10,  12,  14,  16,  20,  24,
        28, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256,
    };
    try testScanner(
        &buf_sizes,
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

test "ATTLIST" {
    if (true) return error.SkipZigTest;
    const buf_sizes = [_]usize{
        1,  2,  3,  4,  5,  6,  7,  8,  10,  12,  14,  16,  20,  24,
        28, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256,
    };
    try testScanner(&buf_sizes, "[  <!ATTLIST abc   lorem NOTATION (x| y| z) #IMPLIED   ipsum (j|k |l) #FIXED '&quot;'   fizz ID #REQUIRED  >]", &[_]ScannerTestItem{
        .{ .marker = .attlist_decl },
        .{ .str = "abc" },

        .{ .marker = .attribute_name },
        .{ .str = "lorem" },
        .{ .marker = .{ .attribute_type = .notation } },
        .{ .str = "x" },
        .{ .str = "y" },
        .{ .str = "z" },
        .{ .str = null },
        .{ .marker = .{ .attribute_default_decl = .implied } },

        .{ .marker = .attribute_name },
        .{ .str = "ipsum" },
        .{ .marker = .{ .attribute_type = .enumeration } },
        .{ .str = "j" },
        .{ .str = "k" },
        .{ .str = "l" },
        .{ .str = null },
        .{ .marker = .{ .attribute_default_decl = .fixed } },
        .{ .marker = .reference_ge },
        .{ .str = "quot" },

        .{ .marker = .attribute_name },
        .{ .str = "fizz" },
        .{ .marker = .{ .attribute_type = .id } },
        .{ .marker = .{ .attribute_default_decl = .required } },

        .{ .marker = .attlist_end },
    });
    try testScanner(
        &buf_sizes,
        \\[
        \\  <!-- no attributes -->
        \\  <!ATTLIST foo>
        \\
        \\  <!-- CDATA & ID examples - most of the other kinds work the exact same -->
        \\  <!ATTLIST a b CDATA #REQUIRED>
        \\  <!ATTLIST c d CDATA #IMPLIED>
        \\  <!ATTLIST e f CDATA #FIXED "fizz&lt;buzz">
        \\  <!ATTLIST g h CDATA "fizz&lt;buzz">
        \\
        \\  <!ATTLIST i j ID #REQUIRED>
        \\  <!ATTLIST k l ID #IMPLIED>
        \\  <!ATTLIST m n ID #FIXED "fizz&lt;buzz">
        \\  <!ATTLIST o p ID "fizz&lt;buzz">
        \\
        \\  <!-- NOTATION & Enumeration - these work the same, just different validation rules for the listed strings (Name vs Nmtoken) -->
        \\  <!ATTLIST q r NOTATION (foo | bar| baz ) #REQUIRED>
        \\  <!ATTLIST s t (fizz| buzz) #IMPLIED>
        \\  <!ATTLIST u v NOTATION (fizz) #IMPLIED>
        \\  <!ATTLIST w x (buzz) #IMPLIED>
        \\]
    ,
        &[_]ScannerTestItem{
            .{ .marker = .attlist_decl },
            .{ .str = "foo" },
            .{ .marker = .attlist_end },

            .{ .marker = .attlist_decl },
            .{ .str = "a" },
            .{ .marker = .attribute_name },
            .{ .str = "b" },
            .{ .marker = .{ .attribute_type = .cdata } },
            .{ .marker = .{ .attribute_default_decl = .required } },
            .{ .marker = .attlist_end },

            .{ .marker = .attlist_decl },
            .{ .str = "c" },
            .{ .marker = .attribute_name },
            .{ .str = "d" },
            .{ .marker = .{ .attribute_type = .cdata } },
            .{ .marker = .{ .attribute_default_decl = .implied } },
            .{ .marker = .attlist_end },

            .{ .marker = .attlist_decl },
            .{ .str = "e" },
            .{ .marker = .attribute_name },
            .{ .str = "f" },
            .{ .marker = .{ .attribute_type = .cdata } },
            .{ .marker = .{ .attribute_default_decl = .fixed } },
            .{ .marker = .text },
            .{ .str = "fizz" },
            .{ .marker = .reference_ge },
            .{ .str = "lt" },
            .{ .marker = .text },
            .{ .str = "buzz" },
            .{ .marker = .attlist_end },

            .{ .marker = .attlist_decl },
            .{ .str = "g" },
            .{ .marker = .attribute_name },
            .{ .str = "h" },
            .{ .marker = .{ .attribute_type = .cdata } },
            .{ .marker = .{ .attribute_default_decl = .value } },
            .{ .marker = .text },
            .{ .str = "fizz" },
            .{ .marker = .reference_ge },
            .{ .str = "lt" },
            .{ .marker = .text },
            .{ .str = "buzz" },
            .{ .marker = .attlist_end },

            .{ .marker = .attlist_decl },
            .{ .str = "i" },
            .{ .marker = .attribute_name },
            .{ .str = "j" },
            .{ .marker = .{ .attribute_type = .id } },
            .{ .marker = .{ .attribute_default_decl = .required } },
            .{ .marker = .attlist_end },

            .{ .marker = .attlist_decl },
            .{ .str = "k" },
            .{ .marker = .attribute_name },
            .{ .str = "l" },
            .{ .marker = .{ .attribute_type = .id } },
            .{ .marker = .{ .attribute_default_decl = .implied } },
            .{ .marker = .attlist_end },

            .{ .marker = .attlist_decl },
            .{ .str = "m" },
            .{ .marker = .attribute_name },
            .{ .str = "n" },
            .{ .marker = .{ .attribute_type = .id } },
            .{ .marker = .{ .attribute_default_decl = .fixed } },
            .{ .marker = .text },
            .{ .str = "fizz" },
            .{ .marker = .reference_ge },
            .{ .str = "lt" },
            .{ .marker = .text },
            .{ .str = "buzz" },
            .{ .marker = .attlist_end },

            .{ .marker = .attlist_decl },
            .{ .str = "o" },
            .{ .marker = .attribute_name },
            .{ .str = "p" },
            .{ .marker = .{ .attribute_type = .id } },
            .{ .marker = .{ .attribute_default_decl = .value } },
            .{ .marker = .text },
            .{ .str = "fizz" },
            .{ .marker = .reference_ge },
            .{ .str = "lt" },
            .{ .marker = .text },
            .{ .str = "buzz" },
            .{ .marker = .attlist_end },

            .{ .marker = .attlist_decl },
            .{ .str = "q" },
            .{ .marker = .attribute_name },
            .{ .str = "r" },
            .{ .marker = .{ .attribute_type = .notation } },
            .{ .str = "foo" },
            .{ .str = "bar" },
            .{ .str = "baz" },
            .{ .str = null },
            .{ .marker = .{ .attribute_default_decl = .required } },
            .{ .marker = .attlist_end },

            .{ .marker = .attlist_decl },
            .{ .str = "s" },
            .{ .marker = .attribute_name },
            .{ .str = "t" },
            .{ .marker = .{ .attribute_type = .enumeration } },
            .{ .str = "fizz" },
            .{ .str = "buzz" },
            .{ .str = null },
            .{ .marker = .{ .attribute_default_decl = .implied } },
            .{ .marker = .attlist_end },

            .{ .marker = .attlist_decl },
            .{ .str = "u" },
            .{ .marker = .attribute_name },
            .{ .str = "v" },
            .{ .marker = .{ .attribute_type = .notation } },
            .{ .str = "fizz" },
            .{ .str = null },
            .{ .marker = .{ .attribute_default_decl = .implied } },
            .{ .marker = .attlist_end },

            .{ .marker = .attlist_decl },
            .{ .str = "w" },
            .{ .marker = .attribute_name },
            .{ .str = "x" },
            .{ .marker = .{ .attribute_type = .enumeration } },
            .{ .str = "buzz" },
            .{ .str = null },
            .{ .marker = .{ .attribute_default_decl = .implied } },
            .{ .marker = .attlist_end },
        },
    );
}

const std = @import("std");
const assert = std.debug.assert;

const builtin = @import("builtin");

const iksemel = @import("../iksemel.zig");
const Tokenizer = iksemel.Tokenizer;

const parse_helper = @import("../parse_helper.zig");
