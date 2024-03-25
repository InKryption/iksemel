//! Scanner which provides the means to scan through the Internal Subset
//! of a DTD Declaration, starting after the '[' token, and ending with
//! the ']' token.
//! Is default initialised, with behaviour based upon the utilized `Tokenizer`,
/// and stream source.
const Scanner = @This();
state: State = .blank,

/// Should never be directly copied by value.
/// This is a namespace field for the full source API.
/// A pointer to this field can be used as a means of communicating
/// intent to exclusively use the full source API.
full: Full = .{},
/// Should never be directly copied by value.
/// This is a namespace field for the stream source API.
/// A pointer to this field can be used as a means of communicating
/// intent to exclusively use the stream source API.
stream: Stream = .{},

pub const Full = struct {
    pub inline fn asScanner(full: *Full) *Scanner {
        return @fieldParentPtr(Scanner, "full", full);
    }

    pub fn nextMarker(
        full: *Scanner.Full,
        /// Full source tokenizer.
        tokenizer: *Tokenizer,
    ) ScanError!ScanMarker {
        return nextMarkerOrSrcImpl(
            full.asScanner(),
            tokenizer,
            null,
            .{ .reader = {}, .read_buffer = {} },
            .marker,
        );
    }

    pub fn nextSrc(
        full: *Scanner.Full,
        /// Full source tokenizer.
        tokenizer: *Tokenizer,
    ) ScanError!?Tokenizer.Range {
        return nextMarkerOrSrcImpl(
            full.asScanner(),
            tokenizer,
            null,
            .{ .reader = {}, .read_buffer = {} },
            .src,
        );
    }
};
pub const Stream = struct {
    pub inline fn asScanner(stream: *Scanner.Stream) *Scanner {
        return @fieldParentPtr(Scanner, "stream", stream);
    }
    pub fn nextMarker(
        stream: *Scanner.Stream,
        /// Streaming source tokenizer.
        tokenizer: *Tokenizer,
        reader: anytype,
        read_buffer: []u8,
    ) (@TypeOf(reader).Error || ScanError)!ScanMarker {
        return nextMarkerOrSrcImpl(
            stream.asScanner(),
            tokenizer,
            @TypeOf(reader),
            .{ .reader = reader, .read_buffer = read_buffer },
            .marker,
        );
    }

    pub fn nextSrc(
        stream: *Scanner.Stream,
        /// Streaming source tokenizer.
        tokenizer: *Tokenizer,
        reader: anytype,
        read_buffer: []u8,
    ) (@TypeOf(reader).Error || ScanError)!?[]const u8 {
        return nextMarkerOrSrcImpl(
            stream.asScanner(),
            tokenizer,
            @TypeOf(reader),
            .{ .reader = reader, .read_buffer = read_buffer },
            .src,
        );
    }
};

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
    mixed_content_spec_end: MixedContentSpecEnd,
    children_tok: ?ChildrenToken,

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
        /// Afterwards, `nextMarker` will return `.mixed_content_spec_end`;
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

    pe_reference: enum { name, end },

    entity: Entity,
    element: Element,
    attlist: Attlist,
    notation: Notation,

    const Entity = struct {
        data: Data,
        state: Entity.State,

        const Data = union {
            none: void,

            maybe_need_ent_kind: ScanMarker.EntityKind,

            value_quoted_cached_tt: struct { Tokenizer.QuoteType, ?Tokenizer.TokenType },
            value_quoted: Tokenizer.QuoteType,

            extid_lit: struct { Tokenizer.QuoteType, ScanMarker.EntityKind },
        };
        const State = enum {
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
        };
    };

    const Element = struct {
        data: Data,
        state: enum {
            name_start,
            name_end,

            mixed_start,
            mixed_name_start,
            mixed_name_end,
            mixed_end,

            children_lparen_lparen,
            children_lparen_name,
            children_lparen,

            children,
            children_name,
        },

        const Data = union {
            none: void,
            depth_and_prev: DepthAndPrev,

            const DepthAndPrev = struct {
                depth: u32,
                prev: Prev,
            };

            const Prev = enum { name, lparen, rparen, quant, sep };
        };
    };

    const Attlist = struct {
        state: enum {
            name_start,
            name_end,
        },
    };

    const Notation = struct {
        state: enum {
            name_start,
            name_end,
        },
    };
};

const ReturnType = enum {
    marker,
    src,

    fn Type(ret_type: ReturnType, reader_not_null: bool) type {
        return switch (ret_type) {
            .marker => ScanMarker,
            .src => ?if (reader_not_null) []const u8 else Tokenizer.Range,
        };
    }
};
fn nextMarkerOrSrcImpl(
    scanner: *Scanner,
    tokenizer: *Tokenizer,
    comptime MaybeReader: ?type,
    mbr: parse_helper.MaybeBufferedReader(MaybeReader),
    comptime ret_type: ReturnType,
) !ret_type.Type(MaybeReader != null) {
    switch (scanner.state) {
        .end => return switch (ret_type) {
            .marker => .end,
            .src => unreachable,
        },
        .blank => return while (true) break switch (ret_type) {
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
                    scanner.state = .{ .pe_reference = .name };
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
                            scanner.state = .{ .element = .{
                                .data = .{ .none = {} },
                                .state = .name_start,
                            } };
                            break .element_decl;
                        },
                        .@"<!ATTLIST" => {
                            if (percent) return ScanError.UnexpectedToken;
                            scanner.state = .{ .attlist = .{
                                .state = .name_start,
                            } };
                            break .attlist_decl;
                        },
                        .@"<!NOTATION" => {
                            if (percent) return ScanError.UnexpectedToken;
                            scanner.state = .{ .notation = .{
                                .state = .name_start,
                            } };
                            break .notation_decl;
                        },
                    }
                },
            },
            .src => unreachable,
        },

        .pi => |*state| return while (true) switch (state.*) {
            .in_progress => {
                if (ret_type != .src) unreachable;
                if (MaybeReader != null) {
                    if (try parse_helper.nextTokenSegment(tokenizer, .pi, mbr.reader, mbr.read_buffer)) |segment| return segment;
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

        .pe_reference => |*state| return while (true) switch (state.*) {
            .name => {
                if (ret_type != .src) unreachable;
                if (MaybeReader != null) {
                    if (try parse_helper.nextTokenSegment(tokenizer, .reference, mbr.reader, mbr.read_buffer)) |segment| return segment;
                    state.* = .end;
                    continue;
                } else {
                    state.* = .end;
                    break tokenizer.nextSrcNoUnderrun(.reference);
                }
            },
            .end => {
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
        },

        .entity => |*entity| return handleEntity(entity, tokenizer, MaybeReader, mbr, ret_type),
        .element => |*element| return handleElement(element, tokenizer, MaybeReader, mbr, ret_type),
        .attlist => |*attlist| return handleAttlist(attlist, tokenizer, MaybeReader, mbr, ret_type),
        .notation => |*notation| return handleNotation(notation, tokenizer, MaybeReader, mbr, ret_type),
    }
}

fn handleEntity(
    entity: *State.Entity,
    tokenizer: *Tokenizer,
    comptime MaybeReader: ?type,
    mbr: parse_helper.MaybeBufferedReader(MaybeReader),
    comptime ret_type: ReturnType,
) !ret_type.Type(MaybeReader != null) {
    const scanner = @fieldParentPtr(Scanner, "state", @fieldParentPtr(Scanner.State, "entity", entity));
    return while (true) switch (entity.state) {
        .name_start => {
            if (ret_type != .src) unreachable;
            _ = entity.data.maybe_need_ent_kind;
            if (MaybeReader != null) {
                if (try parse_helper.nextTokenSegment(tokenizer, .dtd, mbr.reader, mbr.read_buffer)) |segment| break segment;
                entity.state = .name_end;
                continue;
            } else {
                entity.state = .name_end;
                break tokenizer.nextSrcNoUnderrun(.dtd);
            }
        },
        .name_end => {
            if (ret_type != .src) unreachable;
            _ = entity.data.maybe_need_ent_kind;
            entity.state = .def_detect_type;
            break null;
        },

        .def_detect_type => {
            if (ret_type != .marker) unreachable;
            const ent_kind = entity.data.maybe_need_ent_kind;

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
                    entity.state, //
                    entity.data //
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

                    entity.state, entity.data = switch (try parse_helper.nextTokenType(tokenizer, quote_type.systemLiteralCtx(), MaybeReader, mbr)) {
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
                entity.state = .def_empty_end;
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
            const quote_type, var maybe_cached_tt = entity.data.value_quoted_cached_tt;
            entity.data = .{ .value_quoted = quote_type };

            switch (blk: {
                defer maybe_cached_tt = null;
                break :blk maybe_cached_tt orelse try parse_helper.nextTokenType(tokenizer, quote_type.entityValueCtx(), MaybeReader, mbr);
            }) {
                else => unreachable,
                .eof => return ScanError.UnexpectedEof,
                .text_data => {
                    entity.state = .def_value_text_start;
                    break .text;
                },
                .ampersand => {
                    entity.state = .def_value_ref_start;
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
            const quote_type = entity.data.value_quoted;
            const str_ctx = quote_type.entityValueCtx();

            if (MaybeReader != null) {
                if (try parse_helper.nextTokenSegment(tokenizer, str_ctx, mbr.reader, mbr.read_buffer)) |segment| break segment;
                entity.state = .def_value_text_end;
                continue;
            } else {
                entity.state = .def_value_text_end;
                break tokenizer.nextSrcNoUnderrun(str_ctx);
            }
        },
        .def_value_text_end => {
            if (ret_type != .src) unreachable;
            const quote_type = entity.data.value_quoted;
            entity.data = .{ .value_quoted_cached_tt = .{ quote_type, null } };
            entity.state = .def_value_detect_type;
            break null;
        },

        .def_value_ref_start => {
            if (ret_type != .src) unreachable;
            _ = entity.data.value_quoted;

            if (MaybeReader != null) {
                if (try parse_helper.nextTokenSegment(tokenizer, .reference, mbr.reader, mbr.read_buffer)) |segment| break segment;
                entity.state = .def_value_ref_end;
                continue;
            } else {
                entity.state = .def_value_ref_end;
                break tokenizer.nextSrcNoUnderrun(.reference);
            }
        },
        .def_value_ref_end => {
            if (ret_type != .src) unreachable;
            const quote_type = entity.data.value_quoted;
            switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
                else => unreachable,
                .tag_token => unreachable,
                .eof => return ScanError.UnexpectedEof,
                .hashtag => return ScanError.UnexpectedToken,
                .invalid_reference_end => return ScanError.UnexpectedToken,
                .semicolon => {},
            }
            entity.data = .{ .value_quoted_cached_tt = .{ quote_type, null } };
            entity.state = .def_value_detect_type;
            break null;
        },

        .def_extid_pub_literal_empty_start => {
            if (ret_type != .src) unreachable;
            _ = entity.data.maybe_need_ent_kind;
            entity.state = .def_extid_pub_literal_empty_end;
            break if (MaybeReader != null) "" else .{ .start = tokenizer.index - 1, .end = tokenizer.index - 1 };
        },
        .def_extid_pub_literal_empty_end => {
            if (ret_type != .src) unreachable;
            const ent_kind = entity.data.maybe_need_ent_kind;

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

            entity.state, entity.data = switch (try parse_helper.nextTokenType(tokenizer, quote_type.systemLiteralCtx(), MaybeReader, mbr)) {
                else => unreachable,
                .eof => return ScanError.UnexpectedEof,
                .quote_single, .quote_double => .{ .def_extid_sys_literal_empty_start, .{ .maybe_need_ent_kind = ent_kind } },
                .text_data => .{ .def_extid_sys_literal_start, .{ .extid_lit = .{ quote_type, ent_kind } } },
            };

            break null;
        },

        .def_extid_pub_literal_start => {
            if (ret_type != .src) unreachable;
            const quote_type, _ = entity.data.extid_lit;
            if (MaybeReader != null) {
                if (try parse_helper.nextTokenSegment(tokenizer, quote_type.systemLiteralCtx(), mbr.reader, mbr.read_buffer)) |segment| {
                    if (!validPubidLiteralSegment(segment, quote_type)) return ScanError.InvalidPubidLiteral;
                    break segment;
                }
                entity.state = .def_extid_pub_literal_end;
                continue;
            } else {
                const range = tokenizer.nextSrcNoUnderrun(quote_type.systemLiteralCtx());
                if (!validPubidLiteralSegment(range.toStr(tokenizer.src), quote_type)) return ScanError.InvalidPubidLiteral;
                entity.state = .def_extid_pub_literal_end;
                break range;
            }
        },
        .def_extid_pub_literal_end => {
            if (ret_type != .src) unreachable;
            const quote_type, const ent_kind = entity.data.extid_lit;

            switch (try parse_helper.nextTokenType(tokenizer, quote_type.systemLiteralCtx(), MaybeReader, mbr)) {
                else => unreachable,
                .text_data => unreachable,
                .eof => return ScanError.UnexpectedEof,
                .quote_single, .quote_double => |close_quote_tt| assert(quote_type.toTokenType() == close_quote_tt),
            }

            // we re-use the `.def_extid_pub_literal_empty_end` prong for the same purpose, as the code is identical
            // to what would otherwise be here.
            entity.data = .{ .maybe_need_ent_kind = ent_kind };
            entity.state = .def_extid_pub_literal_empty_end;
            continue;
        },

        .def_extid_sys_literal_empty_start => {
            if (ret_type != .src) unreachable;
            _ = entity.data.maybe_need_ent_kind;
            entity.state = .def_extid_sys_literal_empty_end;
            break if (MaybeReader != null) "" else .{ .start = tokenizer.index - 1, .end = tokenizer.index - 1 };
        },
        .def_extid_sys_literal_empty_end => {
            if (ret_type != .src) unreachable;
            // we skip the `.src` branch of the `.def_extid_sys_literal_end` prong, going
            // right to the logic for detecting the end, with the same quoteless data.
            _ = entity.data.maybe_need_ent_kind;
            entity.state = .def_extid_sys_literal_end;
            break null;
        },

        .def_extid_sys_literal_start => {
            if (ret_type != .src) unreachable;
            const quote_type, _ = entity.data.extid_lit;
            if (MaybeReader != null) {
                if (try parse_helper.nextTokenSegment(tokenizer, quote_type.systemLiteralCtx(), mbr.reader, mbr.read_buffer)) |segment| break segment;
                entity.state = .def_extid_sys_literal_end;
                continue;
            } else {
                entity.state = .def_extid_sys_literal_end;
                break tokenizer.nextSrcNoUnderrun(quote_type.systemLiteralCtx());
            }
        },
        .def_extid_sys_literal_end => switch (ret_type) {
            .src => {
                const quote_type, const ent_kind = entity.data.extid_lit;
                switch (try parse_helper.nextTokenType(tokenizer, quote_type.systemLiteralCtx(), MaybeReader, mbr)) {
                    else => unreachable,
                    .text_data => unreachable,
                    .eof => return ScanError.UnexpectedEof,
                    .quote_single, .quote_double => |close_quote_tt| assert(quote_type.toTokenType() == close_quote_tt),
                }
                entity.data = .{ .maybe_need_ent_kind = ent_kind };
                break null;
            },

            .marker => {
                const ent_kind = entity.data.maybe_need_ent_kind;
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

                        entity.data = .{ .none = {} };
                        entity.state = .def_ndata_decl_start;
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
            _ = entity.data.none;

            if (MaybeReader != null) {
                if (try parse_helper.nextTokenSegment(tokenizer, .dtd, mbr.reader, mbr.read_buffer)) |segment| break segment;
                entity.state = .def_ndata_decl_end;
                continue;
            } else {
                entity.state = .def_ndata_decl_end;
                break tokenizer.nextSrcNoUnderrun(.dtd);
            }
        },
        .def_ndata_decl_end => {
            _ = entity.data.none;
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
    };
}

fn handleElement(
    element: *State.Element,
    tokenizer: *Tokenizer,
    comptime MaybeReader: ?type,
    mbr: parse_helper.MaybeBufferedReader(MaybeReader),
    comptime ret_type: ReturnType,
) !ret_type.Type(MaybeReader != null) {
    const scanner = @fieldParentPtr(Scanner, "state", @fieldParentPtr(Scanner.State, "element", element));
    return while (true) break switch (element.state) {
        .name_start => {
            if (ret_type != .src) unreachable;
            _ = element.data.none;
            if (MaybeReader != null) {
                if (try parse_helper.nextTokenSegment(tokenizer, .dtd, mbr.reader, mbr.read_buffer)) |segment| break segment;
                element.state = .name_end;
                continue;
            } else {
                element.state = .name_end;
                break tokenizer.nextSrcNoUnderrun(.dtd);
            }
        },
        .name_end => {
            _ = element.data.none;
            switch (ret_type) {
                .src => break null,
                .marker => {
                    if (try parse_helper.skipIfTagWhitespaceOrGetNextTokType(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
                        .tag_whitespace => unreachable,
                        .tag_token => unreachable,
                        .eof => return ScanError.UnexpectedEof,
                        else => return ScanError.UnexpectedToken,
                    };

                    const first_meaningful_tt = try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr);
                    switch (first_meaningful_tt) {
                        else => return ScanError.UnexpectedToken,
                        .eof => return ScanError.UnexpectedEof,
                        .tag_whitespace => unreachable,

                        // 'EMPTY' | 'ANY'
                        .tag_token => {
                            const empty_or_any = try parse_helper.nextTokenSrcAsEnum(tokenizer, .dtd, MaybeReader, mbr, enum { EMPTY, ANY }) orelse
                                return ScanError.UnexpectedToken;
                            switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                                else => return ScanError.UnexpectedToken,
                                .tag_whitespace => unreachable,
                                .angle_bracket_right => {},
                            }
                            scanner.state = .blank;
                            break .{ .content_spec = switch (empty_or_any) {
                                .EMPTY => .empty,
                                .ANY => .any,
                            } };
                        },

                        .lparen => {},
                    }
                    assert(first_meaningful_tt == .lparen);

                    const after_lparen: enum {
                        @"#PCDATA",
                        second_lparen,
                        tag_token,
                    } = switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                        else => return ScanError.UnexpectedToken,
                        .eof => return ScanError.UnexpectedEof,
                        .tag_whitespace => unreachable,

                        .hashtag => pcdata: {
                            switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                else => return ScanError.UnexpectedToken,
                                .eof => return ScanError.UnexpectedEof,
                                .tag_token => {},
                            }
                            if (try parse_helper.nextTokenSrcAsEnum(tokenizer, .dtd, MaybeReader, mbr, enum { PCDATA }) == null)
                                return ScanError.UnexpectedToken;
                            break :pcdata .@"#PCDATA";
                        },
                        .lparen => .second_lparen,
                        .tag_token => .tag_token,
                    };

                    const content_spec: ScanMarker.ContentSpec, //
                    element.state //
                    = switch (after_lparen) {
                        .@"#PCDATA" => .{ .mixed, .mixed_start },
                        .second_lparen => .{ .children, .children_lparen_lparen },
                        .tag_token => .{ .children, .children_lparen_name },
                    };
                    break .{ .content_spec = content_spec };
                },
            }
        },

        .mixed_start => {
            _ = element.data.none;
            switch (ret_type) {
                .src => switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                    else => return ScanError.UnexpectedToken,
                    .eof => return ScanError.UnexpectedEof,
                    .tag_whitespace => unreachable,

                    .rparen => break null,

                    .pipe => {
                        switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                            else => return ScanError.UnexpectedToken,
                            .eof => return ScanError.UnexpectedEof,
                            .tag_whitespace => unreachable,
                            .tag_token => {},
                        }
                        element.state = .mixed_name_start;
                        continue;
                    },
                },
                .marker => {
                    const spec_end: ScanMarker.MixedContentSpecEnd = switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                        else => return ScanError.UnexpectedToken,
                        .eof => return ScanError.UnexpectedEof,

                        .asterisk => zero_or_many: {
                            switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                                else => return ScanError.UnexpectedToken,
                                .eof => return ScanError.UnexpectedEof,
                                .tag_whitespace => unreachable,

                                .angle_bracket_right => {},
                            }
                            break :zero_or_many .zero_or_many;
                        },

                        .tag_whitespace => one: {
                            try parse_helper.skipWhitespaceSrcUnchecked(tokenizer, .dtd, MaybeReader, mbr);
                            switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                else => return ScanError.UnexpectedToken,
                                .eof => return ScanError.UnexpectedEof,
                                .tag_whitespace => unreachable,

                                .angle_bracket_right => {},
                            }
                            break :one .one;
                        },
                        .angle_bracket_right => .one,
                    };
                    scanner.state = .blank;
                    break .{ .mixed_content_spec_end = spec_end };
                },
            }
        },
        .mixed_name_start => {
            if (ret_type != .src) unreachable;
            _ = element.data.none;
            if (MaybeReader != null) {
                if (try parse_helper.nextTokenSegment(tokenizer, .dtd, mbr.reader, mbr.read_buffer)) |segment| break segment;
                element.state = .mixed_name_end;
                continue;
            } else {
                element.state = .mixed_name_end;
                break tokenizer.nextSrcNoUnderrun(.dtd);
            }
        },
        .mixed_name_end => {
            if (ret_type != .src) unreachable;
            _ = element.data.none;
            element.state = switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                else => return ScanError.UnexpectedToken,
                .eof => return ScanError.UnexpectedEof,
                .tag_whitespace => unreachable,

                .rparen => .mixed_end,
                .pipe => switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                    else => return ScanError.UnexpectedToken,
                    .eof => return ScanError.UnexpectedEof,
                    .tag_whitespace => unreachable,

                    .tag_token => .mixed_name_start,
                },
            };
            break null;
        },
        .mixed_end => {
            _ = element.data.none;
            switch (ret_type) {
                .src => break null,
                .marker => {
                    switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                        else => return ScanError.UnexpectedToken,
                        .eof => return ScanError.UnexpectedEof,
                        .asterisk => {},
                    }
                    switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                        else => return ScanError.UnexpectedToken,
                        .eof => return ScanError.UnexpectedEof,
                        .tag_whitespace => unreachable,
                        .angle_bracket_right => {},
                    }
                    scanner.state = .blank;
                    break .{ .mixed_content_spec_end = .zero_or_many };
                },
            }
        },

        .children_lparen_lparen => {
            if (ret_type != .marker) unreachable;
            _ = element.data.none;
            element.state = .children_lparen;
            break .{ .children_tok = .lparen };
        },
        .children_lparen_name => {
            if (ret_type != .marker) unreachable;
            element.data = .{ .depth_and_prev = .{ .depth = 1, .prev = .lparen } };
            element.state = .children_name;
            break .{ .children_tok = .lparen };
        },
        .children_lparen => {
            if (ret_type != .marker) unreachable;
            element.data = .{ .depth_and_prev = .{ .depth = 2, .prev = .lparen } };
            element.state = .children;
            break .{ .children_tok = .lparen };
        },

        .children => {
            const depth_and_prev = &element.data.depth_and_prev;
            switch (ret_type) {
                .marker => {
                    const children_tok: ScanMarker.ChildrenToken = switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                        else => return ScanError.UnexpectedToken,
                        .eof => return ScanError.UnexpectedEof,
                        .tag_whitespace => unreachable,

                        .angle_bracket_right => {
                            if (depth_and_prev.depth != 0) return ScanError.UnexpectedToken;
                            scanner.state = .blank;
                            break .{ .children_tok = null };
                        },

                        .tag_token,
                        => .name,

                        inline //
                        .lparen,
                        .rparen,

                        .pipe,
                        .comma,

                        .qmark,
                        .asterisk,
                        .plus,
                        => |tt| @field(ScanMarker.ChildrenToken, @tagName(tt)),
                    };

                    const transition = struct {
                        const Kind = State.Element.Data.Prev;
                        const Bits = packed struct { prev: Kind, curr: Kind };
                        const BitsInt = std.meta.Int(.unsigned, @bitSizeOf(Bits));
                        inline fn transition(prev: Kind, curr: Kind) BitsInt {
                            return @bitCast(Bits{ .prev = prev, .curr = curr });
                        }
                    }.transition;

                    const prev = depth_and_prev.prev;
                    const curr: State.Element.Data.Prev = switch (children_tok) {
                        .name => .name,
                        .lparen => .lparen,
                        .rparen => .rparen,
                        .qmark, .asterisk, .plus => .quant,
                        .comma, .pipe => .sep,
                    };
                    depth_and_prev.prev = curr;

                    if (depth_and_prev.depth == 0 and
                        prev == .quant //
                    ) return ScanError.UnexpectedToken;
                    if (depth_and_prev.depth == 0) assert(prev == .rparen);

                    switch (transition(prev, curr)) {
                        transition(.name, .name),
                        transition(.rparen, .name),
                        transition(.quant, .name),
                        => return ScanError.UnexpectedToken,
                        transition(.lparen, .name),
                        transition(.sep, .name),
                        => {
                            assert(depth_and_prev.depth != 0);
                            element.state = .children_name;
                        },

                        transition(.name, .lparen),
                        transition(.rparen, .lparen),
                        transition(.quant, .lparen),
                        => return ScanError.UnexpectedToken,
                        transition(.lparen, .lparen),
                        transition(.sep, .lparen),
                        => {
                            assert(depth_and_prev.depth != 0);
                            depth_and_prev.depth += 1;
                        },

                        transition(.lparen, .rparen),
                        transition(.sep, .rparen),
                        => return ScanError.UnexpectedToken,
                        transition(.rparen, .rparen),
                        => if (depth_and_prev.depth == 0) return ScanError.UnexpectedToken,
                        transition(.name, .rparen),
                        transition(.quant, .rparen),
                        => {
                            assert(depth_and_prev.depth != 0);
                            depth_and_prev.depth -= 1;
                        },

                        transition(.lparen, .quant),
                        transition(.quant, .quant),
                        transition(.sep, .quant),
                        => return ScanError.UnexpectedToken,
                        transition(.name, .quant),
                        => assert(depth_and_prev.depth != 0),
                        transition(.rparen, .quant),
                        => {},

                        transition(.lparen, .sep),
                        transition(.sep, .sep),
                        => return ScanError.UnexpectedToken,
                        transition(.rparen, .sep),
                        => if (depth_and_prev.depth == 0) return ScanError.UnexpectedToken,
                        transition(.name, .sep),
                        transition(.quant, .sep),
                        => assert(depth_and_prev.depth != 0),

                        else => unreachable,
                    }

                    break .{ .children_tok = children_tok };
                },
                .src => break null,
            }
        },
        .children_name => {
            const depth_and_prev = &element.data.depth_and_prev;
            switch (ret_type) {
                .marker => break .{ .children_tok = .name },
                .src => {
                    if (MaybeReader != null) {
                        if (try parse_helper.nextTokenSegment(tokenizer, .dtd, mbr.reader, mbr.read_buffer)) |segment| break segment;
                        depth_and_prev.prev = .name;
                        element.state = .children;
                        continue;
                    } else {
                        depth_and_prev.prev = .name;
                        element.state = .children;
                        break tokenizer.nextSrcNoUnderrun(.dtd);
                    }
                },
            }
        },
    };
}

fn handleAttlist(
    attlist: *State.Attlist,
    tokenizer: *Tokenizer,
    comptime MaybeReader: ?type,
    mbr: parse_helper.MaybeBufferedReader(MaybeReader),
    comptime ret_type: ReturnType,
) !ret_type.Type(MaybeReader != null) {
    _ = tokenizer;
    _ = mbr;
    const scanner = @fieldParentPtr(Scanner, "state", @fieldParentPtr(Scanner.State, "attlist", attlist));
    _ = scanner;
    @panic("TODO");
}

fn handleNotation(
    notation: *State.Notation,
    tokenizer: *Tokenizer,
    comptime MaybeReader: ?type,
    mbr: parse_helper.MaybeBufferedReader(MaybeReader),
    comptime ret_type: ReturnType,
) !ret_type.Type(MaybeReader != null) {
    _ = tokenizer;
    _ = mbr;
    const scanner = @fieldParentPtr(Scanner, "state", @fieldParentPtr(Scanner.State, "notation", notation));
    _ = scanner;
    @panic("TODO");
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
    marker: ScanError!ScanMarker,
    str: ScanError!?[]const u8,
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

            var scanner: Scanner = .{};
            for (expected_items, 0..) |expected_item, i| {
                errdefer std.log.err("Error occurred on item {d}", .{i});
                switch (expected_item) {
                    .marker => |marker| try std.testing.expectEqual(marker, scanner.stream.nextMarker(&tokenizer, fbs.reader(), read_buffer)),
                    .str => |expected_maybe_str_or_err| {
                        const actual_maybe_str_or_err: ScanError!?[]const u8 = blk: {
                            var first = true;
                            while (true) : (first = false) {
                                const maybe_segment = scanner.stream.nextSrc(&tokenizer, fbs.reader(), read_buffer) catch |actual_err| {
                                    comptime assert(@TypeOf(actual_err) == ScanError);
                                    break :blk actual_err;
                                };
                                const first_segment = maybe_segment orelse {
                                    if (first) break :blk null;
                                    break;
                                };
                                try str_buffer.appendSlice(first_segment);
                            }

                            break :blk str_buffer.items;
                        };
                        try expectEqualStringOrErrOrNull(expected_maybe_str_or_err, actual_maybe_str_or_err);
                        str_buffer.clearRetainingCapacity();
                    },
                }
            }
            try std.testing.expectEqual(.end, scanner.stream.nextMarker(&tokenizer, fbs.reader(), read_buffer));
        }
    }

    var tokenizer = Tokenizer.initComplete(src);
    assert(tokenizer.nextTypeNoUnderrun(.dtd) == .square_bracket_left);

    var scanner: Scanner = .{};
    for (expected_items, 0..) |expected_item, i| {
        errdefer std.log.err("Error occurred on item {d}", .{i});
        switch (expected_item) {
            .marker => |marker| try std.testing.expectEqual(marker, scanner.full.nextMarker(&tokenizer)),
            .str => |expected_maybe_str_or_err| {
                const actual_maybe_str_or_err: ScanError!?[]const u8 = blk: {
                    var first = true;
                    while (true) : (first = false) {
                        const maybe_segment = scanner.full.nextSrc(&tokenizer) catch |actual_err| {
                            comptime assert(@TypeOf(actual_err) == ScanError);
                            break :blk actual_err;
                        };
                        const first_segment = maybe_segment orelse {
                            if (first) break :blk null;
                            break;
                        };
                        try str_buffer.appendSlice(first_segment.toStr(tokenizer.src));
                    }
                    break :blk str_buffer.items;
                };
                try expectEqualStringOrErrOrNull(expected_maybe_str_or_err, actual_maybe_str_or_err);
                str_buffer.clearRetainingCapacity();
            },
        }
    }
    try std.testing.expectEqual(.end, scanner.full.nextMarker(&tokenizer));
}

fn expectEqualStringOrErrOrNull(
    expected: anyerror!?[]const u8,
    actual: anyerror!?[]const u8,
) !void {
    const ValKind = enum { err, null, str };
    const expected_kind: ValKind = if (expected) |maybe_str| if (maybe_str != null) .str else .null else |_| .err;
    const actual_kind: ValKind = if (actual) |maybe_str| if (maybe_str != null) .str else .null else |_| .err;

    const combo = packed struct {
        expected: ValKind,
        actual: ValKind,
        inline fn combo(exp: ValKind, act: ValKind) u4 {
            const bits: @This() = .{ .expected = exp, .actual = act };
            return @bitCast(bits);
        }
    }.combo;

    switch (combo(expected_kind, actual_kind)) {
        combo(.str, .str) => try std.testing.expectEqualStrings((expected catch unreachable).?, (actual catch unreachable).?),
        combo(.null, .null) => {},
        combo(.err, .err) => try std.testing.expectEqual(expected, actual),

        combo(.str, .null),
        combo(.str, .err),
        => {
            const expected_str = (expected catch unreachable).?;
            const actual_non_str: ?anyerror = if (actual) |must_be_null| if (must_be_null) |_| unreachable else null else |err| err;
            std.log.err("Expected '{[expected]}', got {[actual]?}", .{
                .expected = std.zig.fmtEscapes(expected_str),
                .actual = actual_non_str,
            });
            return error.TestExpectedEqual;
        },

        combo(.null, .str),
        combo(.err, .str),
        => {
            const expected_non_str: ?anyerror = if (expected) |must_be_null| if (must_be_null) |_| unreachable else null else |err| err;
            const actual_str = (actual catch unreachable).?;
            std.log.err("Expected {[expected]?}, got '{[actual]}'", .{
                .expected = expected_non_str,
                .actual = std.zig.fmtEscapes(actual_str),
            });
            return error.TestExpectedEqual;
        },

        combo(.null, .err),
        combo(.err, .null),
        => {
            const expected_non_str: ?anyerror = if (expected) |must_be_null| if (must_be_null) |_| unreachable else null else |err| err;
            const actual_non_str: ?anyerror = if (actual) |must_be_null| if (must_be_null) |_| unreachable else null else |err| err;
            std.log.err("Expected {[expected]?}, got {[actual]?}", .{
                .expected = expected_non_str,
                .actual = actual_non_str,
            });
            return error.TestExpectedEqual;
        },

        else => unreachable,
    }
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
