//! Provides the means to scan through the Internal Subset of a DTD Declaration,
//! starting after the '[' token.

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
        segment_iter: parse_helper.TokenSrcIter(MaybeReader) = undefined,
        const Self = @This();

        pub const Segment = if (MaybeReader != null)
            []const u8
        else
            Tokenizer.Range;

        pub inline fn nextMarker(scanner: *Self) !ScanMarker {
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

pub const ScanError = error{
    UnexpectedToken,
    UnexpectedEof,
    MissingAsterisk,
    EmptyPI,
    EmptyReference,
};

pub const ScanMarker = union(enum) {
    /// Encountered ']', terminating the internal subset.
    end,

    /// `nextSegment` will return the PI data.
    pi,

    /// `nextSegment` will return the text data.
    text,

    /// `nextSegment` will return the entity name.
    ge_reference,
    /// `nextSegment` will return the entity name.
    pe_reference,
    /// `nextSegment` will return the character number string.
    char_reference,

    /// First `nextSegment` will return the declared element's name.
    /// Then `nextMarker` will return `.content_spec`, with a payload
    /// describing the procedure to obtain the content specification.
    element_decl,
    content_spec: ContentSpec,
    children_tok: ?ChildrenToken,
    mixed_content_spec_end: MixedContentSpecEnd,

    /// First `nextSegment` will return the target element's name.
    /// Then `nextMarker` will either return `.attlist_end`, closing
    /// the attribute list declaration, or `.attribute_name`, beginning
    /// an attribute definition. In the latter case, the second step
    /// described here will repeat after acquiring the attribute definition.
    attlist_decl,
    /// First `nextSegment` will return the declared attribute's name.
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

        /// `nextSegment` will return a list of `Name`s.
        notation,
        /// `nextSegment` will return a list of `Nmtoken`s.
        enumeration,
    };

    pub const DefaultDecl = enum {
        /// `'#REQUIRED'`
        required,
        /// `'#IMPLIED'`
        implied,
        /// `'#FIXED' S AttValue`
        /// Followed by a series of `.text` and `.reference`, terminated by `.attribute_name` or `.attlist_end`.
        fixed,
        /// `AttValue`
        /// Followed by a series of `.text` and `.reference`, terminated by `.attribute_name` or `.attlist_end`.
        value,
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

    attlist_start,
    attlist_attr_name,
    attlist_attr_type_enumerated_list,
    attlist_attr_type_end,
    attlist_attr_default_decl_empty,
    attlist_attr_default_decl_text_sq,
    attlist_attr_default_decl_text_dq,
    attlist_attr_default_decl_ampersand_sq,
    attlist_attr_default_decl_ampersand_dq,
    attlist_attr_default_decl_empty_end,
};

fn nextMarkerOrSegmentImpl(
    comptime MaybeReader: ?type,
    scanner: *Scanner(MaybeReader),
    comptime ret_type: enum { marker, segment },
) !switch (ret_type) {
    .marker => ScanMarker,
    .segment => ?Scanner(MaybeReader).Segment,
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
                    .eof => return ScanError.UnexpectedEof,
                    .pi_end => return ScanError.EmptyPI,
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
                    })) orelse return ScanError.UnexpectedToken;
                    switch (decl_str) {
                        .@"<!ENTITY" => @panic("TODO"),
                        .@"<!ELEMENT" => {
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
                            scanner.segment_iter = .{};
                            scanner.state = .element_start;
                            break .element_decl;
                        },
                        .@"<!ATTLIST" => {
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
                            scanner.segment_iter = .{};
                            scanner.state = .attlist_start;
                            break .attlist_decl;
                        },
                        .@"<!NOTATION" => @panic("TODO"),
                    }
                },

                .comment_start => switch (try parse_helper.handleCommentSkip(tokenizer, MaybeReader, mbr)) {
                    .normal_end => continue,
                    .invalid_end_triple_dash => return ScanError.UnexpectedToken,
                    .invalid_dash_dash => return ScanError.UnexpectedToken,
                    .eof => return ScanError.UnexpectedEof,
                },
                .eof => return ScanError.UnexpectedEof,
                else => return ScanError.UnexpectedToken,
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
                    .eof => return ScanError.UnexpectedEof,
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
                    .eof => return ScanError.UnexpectedEof,
                    else => return ScanError.UnexpectedToken,
                };
                switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                    else => return ScanError.UnexpectedToken,
                    .eof => return ScanError.UnexpectedEof,
                    .tag_whitespace => unreachable,
                    .tag_token => {
                        const empty_or_any = (try parse_helper.nextTokenSrcAsEnum(tokenizer, .dtd, MaybeReader, mbr, enum { EMPTY, ANY })) orelse
                            return ScanError.UnexpectedToken;
                        switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                            else => return ScanError.UnexpectedToken,
                            .eof => return ScanError.UnexpectedEof,
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
                        else => return ScanError.UnexpectedToken,
                        .eof => return ScanError.UnexpectedEof,

                        .hashtag => {
                            switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                                else => return ScanError.UnexpectedToken,
                                .eof => return ScanError.UnexpectedEof,
                                .tag_token => if (try parse_helper.nextTokenSrcAsEnum(tokenizer, .dtd, MaybeReader, mbr, enum { PCDATA }) == null) {
                                    return ScanError.UnexpectedToken;
                                },
                            }
                            scanner.state = switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                                else => return ScanError.UnexpectedToken,
                                .tag_whitespace => unreachable,
                                .tag_token => unreachable,

                                .pipe => switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                                    else => return ScanError.UnexpectedToken,
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
                    else => return ScanError.UnexpectedToken,
                    .eof => return ScanError.UnexpectedEof,
                    .tag_whitespace => unreachable,

                    .rparen => scanner.state = .element_mixed_list_rparen,

                    .pipe => switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                        else => return ScanError.UnexpectedToken,
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
                const mixed_content_spec_end: ScanMarker.MixedContentSpecEnd = switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                    .asterisk => blk: {
                        switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                            else => return ScanError.UnexpectedToken,
                            .eof => return ScanError.UnexpectedEof,
                            .tag_whitespace => unreachable,
                            .angle_bracket_right => {},
                        }
                        break :blk .zero_or_many;
                    },
                    .tag_whitespace => blk: {
                        try parse_helper.skipWhitespaceSrcUnchecked(tokenizer, .dtd, MaybeReader, mbr);
                        switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                            else => return ScanError.UnexpectedToken,
                            .eof => return ScanError.UnexpectedEof,
                            .tag_whitespace => unreachable,
                            .angle_bracket_right => {},
                        }
                        break :blk .one;
                    },
                    .angle_bracket_right => .one,
                    .eof => return ScanError.UnexpectedEof,
                    else => return ScanError.UnexpectedToken,
                };

                switch (mixed_content_spec_end) {
                    .one => switch (tag) {
                        .element_mixed_list_rparen => return ScanError.MissingAsterisk,
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
                    break .{ .children_tok = @field(ScanMarker.ChildrenToken, @tagName(tt)) };
                },
                inline .qmark, .asterisk, .plus => |tt| {
                    scanner.state = .{ .element_children_quantifier = depth };
                    break .{ .children_tok = @field(ScanMarker.ChildrenToken, @tagName(tt)) };
                },
                else => return ScanError.UnexpectedToken,
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
                    if (depth == 0) return ScanError.UnexpectedToken;
                    scanner.state = .{ .element_children_rparen = depth - 1 };
                    break .{ .children_tok = .rparen };
                },
                inline .comma, .pipe => |tt| {
                    if (depth == 0) return ScanError.UnexpectedToken;
                    scanner.state = .{ .element_children_sep = depth };
                    break .{ .children_tok = @field(ScanMarker.ChildrenToken, @tagName(tt)) };
                },
                .angle_bracket_right => {
                    if (depth != 0) return ScanError.UnexpectedToken;
                    scanner.state = .blank;
                    break .{ .children_tok = null };
                },
                else => return ScanError.UnexpectedToken,
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
                else => return ScanError.UnexpectedToken,
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
                else => return ScanError.UnexpectedToken,
            },
            .segment => unreachable,
        },
        .element_children_rparen => |*depth| switch (ret_type) {
            .marker => switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                .rparen => {
                    if (depth.* == 0) return ScanError.UnexpectedToken;
                    depth.* -= 1;
                    break .{ .children_tok = .rparen };
                },
                inline .comma, .pipe => |tt| {
                    if (depth.* == 0) return ScanError.UnexpectedToken;
                    scanner.state = .{ .element_children_sep = depth.* };
                    break .{ .children_tok = @field(ScanMarker.ChildrenToken, @tagName(tt)) };
                },
                inline .qmark, .asterisk, .plus => |tt| {
                    scanner.state = .{ .element_children_quantifier = depth.* };
                    break .{ .children_tok = @field(ScanMarker.ChildrenToken, @tagName(tt)) };
                },
                .angle_bracket_right => {
                    scanner.state = .blank;
                    break .{ .children_tok = null };
                },
                else => return ScanError.UnexpectedToken,
            },
            .segment => unreachable,
        },

        .attlist_start => switch (ret_type) {
            // this goes second, and then may be reached by a number of other `.attlist_*` substates.
            .marker => {
                switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                    else => return ScanError.UnexpectedToken,
                    .eof => return ScanError.UnexpectedEof,

                    .angle_bracket_right => {
                        scanner.state = .blank;
                        break .attlist_end;
                    },
                    .tag_whitespace => {},
                }
                try parse_helper.skipWhitespaceSrcUnchecked(tokenizer, .dtd, MaybeReader, mbr);

                switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                    else => return ScanError.UnexpectedToken,
                    .eof => return ScanError.UnexpectedEof,
                    .tag_whitespace => unreachable,

                    .angle_bracket_right => {
                        scanner.state = .blank;
                        break .attlist_end;
                    },
                    .tag_token => {
                        scanner.state = .attlist_attr_name;
                        scanner.segment_iter = .{};
                        break .attribute_name;
                    },
                }
            },

            // this goes first, only reached from `.blank`
            .segment => {
                if (try scanner.segment_iter.next(tokenizer, .dtd, mbr)) |segment| break segment;
                scanner.segment_iter = undefined;
                break null;
            },
        },
        .attlist_attr_name => switch (ret_type) {
            // this goes second
            .marker => {
                if (try parse_helper.skipIfTagWhitespaceOrGetNextTokType(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
                    .tag_whitespace => unreachable,
                    .tag_token => unreachable,
                    .eof => return ScanError.UnexpectedEof,
                    else => return ScanError.UnexpectedToken,
                };

                // true for 'NOTATION (...)' and '(...)'
                const att_type: ScanMarker.AttType = switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                    .tag_token => blk: {
                        const att_type_str = (try parse_helper.nextTokenSrcAsEnum(tokenizer, .dtd, MaybeReader, mbr, enum {
                            CDATA,
                            ID,
                            IDREF,
                            IDREFS,
                            ENTITY,
                            ENTITIES,
                            NMTOKEN,
                            NMTOKENS,

                            NOTATION,
                        })) orelse return ScanError.UnexpectedToken;
                        break :blk switch (att_type_str) {
                            .CDATA => .cdata,
                            .ID => .id,
                            .IDREF => .idref,
                            .IDREFS => .idrefs,
                            .ENTITY => .entity,
                            .ENTITIES => .entities,
                            .NMTOKEN => .nmtoken,
                            .NMTOKENS => .nmtokens,

                            .NOTATION => .notation,
                        };
                    },
                    .lparen => .enumeration,

                    .eof => return ScanError.UnexpectedEof,
                    else => return ScanError.UnexpectedToken,
                };

                switch (att_type) {
                    // if it's `.notation`, we're expecting a parentheses, after the 'NOTATION'.
                    // if it's `.enumeration`, we already saw a parentheses.
                    // after the parentheses, the procedure for `.notation` and `.enumeration` is
                    // effectively the same, as far as we're concerned (we're not validating the Name vs Nmtoken distinction),
                    // so we first verify the parentheses is there, so afterwards we can proceed under the assumption that
                    // we've seen the parentheses whether it's `.notation` or `.enumeration`.
                    .notation => {
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
                            .lparen => {},
                        }
                    },
                    else => {},
                }

                scanner.state = switch (att_type) {
                    // we expect at least one name after the lparen.
                    .notation, .enumeration => switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                        else => return ScanError.UnexpectedToken,
                        .eof => return ScanError.UnexpectedEof,
                        .tag_whitespace => unreachable,
                        .tag_token => blk: {
                            scanner.segment_iter = .{};
                            break :blk .attlist_attr_type_enumerated_list;
                        },
                    },
                    else => .attlist_attr_type_end,
                };

                break .{ .attribute_type = att_type };
            },

            // this goes first
            .segment => {
                if (try scanner.segment_iter.next(tokenizer, .dtd, mbr)) |segment| break segment;
                scanner.segment_iter = undefined;
                break null;
            },
        },
        .attlist_attr_type_enumerated_list => switch (ret_type) {
            .marker => unreachable,
            .segment => {
                if (try scanner.segment_iter.next(tokenizer, .dtd, mbr)) |segment| break segment;
                scanner.segment_iter = undefined;

                switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                    else => return ScanError.UnexpectedToken,
                    .eof => return ScanError.UnexpectedEof,
                    .tag_whitespace => unreachable,
                    .pipe => switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                        else => return ScanError.UnexpectedToken,
                        .eof => return ScanError.UnexpectedEof,
                        .tag_whitespace => unreachable,
                        .tag_token => scanner.segment_iter = .{},
                    },
                    // hits the branch that returns null
                    .rparen => scanner.state = .attlist_attr_type_end,
                }

                break null;
            },
        },
        .attlist_attr_type_end => switch (ret_type) {
            .marker => {
                if (try parse_helper.skipIfTagWhitespaceOrGetNextTokType(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
                    .tag_whitespace => unreachable,
                    .tag_token => unreachable,
                    .eof => return ScanError.UnexpectedEof,
                    else => return ScanError.UnexpectedToken,
                };
                const QuoteType = enum { quote_single, quote_double };
                const dd_type: union(ScanMarker.DefaultDecl) {
                    required,
                    implied,
                    fixed: QuoteType,
                    value: QuoteType,
                } = switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                    else => return ScanError.UnexpectedToken,
                    .eof => return ScanError.UnexpectedEof,
                    .tag_whitespace => unreachable,

                    .hashtag => blk: {
                        switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                            else => return ScanError.UnexpectedToken,
                            .eof => return ScanError.UnexpectedEof,
                            .tag_token => {},
                        }
                        const dd_type_str = (try parse_helper.nextTokenSrcAsEnum(tokenizer, .dtd, MaybeReader, mbr, enum {
                            REQUIRED,
                            IMPLIED,
                            FIXED,
                        })) orelse return ScanError.UnexpectedToken;

                        switch (dd_type_str) {
                            .REQUIRED => break :blk .required,
                            .IMPLIED => break :blk .implied,
                            .FIXED => {},
                        }
                        assert(dd_type_str == .FIXED);
                        if (try parse_helper.skipIfTagWhitespaceOrGetNextTokType(tokenizer, .dtd, MaybeReader, mbr)) |non_ws| switch (non_ws) {
                            .tag_whitespace => unreachable,
                            .tag_token => unreachable,
                            .eof => return ScanError.UnexpectedEof,
                            else => return ScanError.UnexpectedToken,
                        };
                        const quote_type: QuoteType = switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
                            else => return ScanError.UnexpectedToken,
                            .eof => return ScanError.UnexpectedEof,
                            .tag_whitespace => unreachable,
                            inline //
                            .quote_single,
                            .quote_double,
                            => |tt| @field(QuoteType, @tagName(tt)),
                        };
                        break :blk .{ .fixed = quote_type };
                    },
                    inline //
                    .quote_single,
                    .quote_double,
                    => |tt| .{ .value = @field(QuoteType, @tagName(tt)) },
                };

                scanner.state = switch (dd_type) {
                    .required, .implied => .attlist_start,
                    .fixed, .value => |quote_type| blk: {
                        const str_ctx: Tokenizer.Context = switch (quote_type) {
                            .quote_single => .attribute_value_quote_single,
                            .quote_double => .attribute_value_quote_double,
                        };
                        break :blk switch (try parse_helper.nextTokenType(tokenizer, str_ctx, MaybeReader, mbr)) {
                            .quote_single, .quote_double => .attlist_attr_default_decl_empty,
                            .text_data => switch (quote_type) {
                                .quote_single => .attlist_attr_default_decl_text_sq,
                                .quote_double => .attlist_attr_default_decl_text_dq,
                            },
                            .ampersand => switch (quote_type) {
                                .quote_single => .attlist_attr_default_decl_ampersand_sq,
                                .quote_double => .attlist_attr_default_decl_ampersand_dq,
                            },
                            .angle_bracket_left => return ScanError.UnexpectedToken,
                            .eof => return ScanError.UnexpectedEof,
                            else => return ScanError.UnexpectedToken,
                        };
                    },
                };

                break .{ .attribute_default_decl = dd_type };
            },
            // this will only be hit after `.attlist_attr_type_enumerated_list`
            .segment => break null,
        },
        .attlist_attr_default_decl_empty => switch (ret_type) {
            .marker => break .text,
            .segment => {
                scanner.state = .attlist_attr_default_decl_empty_end;
                break if (MaybeReader != null) "" else .{ .start = tokenizer.index - 1, .end = tokenizer.index };
            },
        },
        .attlist_attr_default_decl_empty_end => switch (ret_type) {
            .marker => unreachable,
            .segment => {
                scanner.state = .attlist_start;
                break null;
            },
        },

        inline //
        .attlist_attr_default_decl_text_sq,
        .attlist_attr_default_decl_text_dq,
        => |_, quote_state| {
            const str_ctx: Tokenizer.Context = switch (quote_state) {
                .attlist_attr_default_decl_text_sq => .attribute_value_quote_single,
                .attlist_attr_default_decl_text_dq => .attribute_value_quote_double,
                else => unreachable,
            };

            switch (ret_type) {
                .marker => {
                    scanner.segment_iter = .{};
                    break .text;
                },
                .segment => {
                    if (try scanner.segment_iter.next(tokenizer, str_ctx, mbr)) |segment| break segment;
                    scanner.state = switch (try parse_helper.nextTokenType(tokenizer, str_ctx, MaybeReader, mbr)) {
                        else => return ScanError.UnexpectedToken,
                        .eof => return ScanError.UnexpectedEof,

                        .quote_single, .quote_double => .attlist_start,
                        .text_data => scanner.state,
                        .ampersand => switch (quote_state) {
                            .attlist_attr_default_decl_text_sq => .attlist_attr_default_decl_ampersand_sq,
                            .attlist_attr_default_decl_text_dq => .attlist_attr_default_decl_ampersand_dq,
                            else => comptime unreachable,
                        },
                    };
                    break null;
                },
            }
        },

        inline //
        .attlist_attr_default_decl_ampersand_sq,
        .attlist_attr_default_decl_ampersand_dq,
        => |_, quote_state| {
            const str_ctx: Tokenizer.Context = switch (quote_state) {
                .attlist_attr_default_decl_ampersand_sq => .attribute_value_quote_single,
                .attlist_attr_default_decl_ampersand_dq => .attribute_value_quote_double,
                else => comptime unreachable,
            };
            switch (ret_type) {
                .marker => {
                    const ref_kind: ScanMarker = switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
                        .eof => return ScanError.UnexpectedEof,
                        .semicolon => return ScanError.EmptyReference,
                        .invalid_reference_end => return ScanError.UnexpectedToken,

                        .hashtag => switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
                            .eof => return ScanError.UnexpectedEof,
                            .semicolon => return ScanError.EmptyReference,
                            .invalid_reference_end => return ScanError.UnexpectedToken,
                            .hashtag => return ScanError.UnexpectedToken,

                            .tag_token => .char_reference,
                            else => unreachable,
                        },
                        .tag_token => .ge_reference,
                        else => unreachable,
                    };

                    scanner.segment_iter = .{};
                    break ref_kind;
                },
                .segment => {
                    if (try scanner.segment_iter.next(tokenizer, .reference, mbr)) |segment| break segment;
                    switch (try parse_helper.nextTokenType(tokenizer, .reference, MaybeReader, mbr)) {
                        else => unreachable,
                        .tag_token => unreachable,
                        .eof => return ScanError.UnexpectedEof,
                        .hashtag => return ScanError.UnexpectedToken,
                        .invalid_reference_end => return ScanError.UnexpectedToken,
                        .semicolon => {},
                    }
                    scanner.state = switch (try parse_helper.nextTokenType(tokenizer, str_ctx, MaybeReader, mbr)) {
                        else => return ScanError.UnexpectedToken,
                        .eof => return ScanError.UnexpectedEof,

                        .quote_single, .quote_double => .attlist_start,
                        .ampersand => scanner.state,
                        .text_data => switch (quote_state) {
                            .attlist_attr_default_decl_ampersand_sq => .attlist_attr_default_decl_text_sq,
                            .attlist_attr_default_decl_ampersand_dq => .attlist_attr_default_decl_text_dq,
                            else => comptime unreachable,
                        },
                    };
                    break null;
                },
            }
        },
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

    var scanner = sliceScanner(&tokenizer);
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

test "ELEMENT" {
    try testScanner(
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

test "ATTLIST" {
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
        .{ .marker = .ge_reference },
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
            .{ .marker = .ge_reference },
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
            .{ .marker = .ge_reference },
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
            .{ .marker = .ge_reference },
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
            .{ .marker = .ge_reference },
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
