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
        state: State = .start,
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
    EmptyPI,
    MissingAsterisk,
};

pub const ScanMarker = union(enum) {
    /// Encountered ']', terminating the internal subset.
    end,

    /// `nextSegment` will return the PI data.
    pi,

    /// First `nextSegment` will return the declared element's name.
    /// Then `nextMarker` will return `.content_spec_start`, with a payload
    /// describing the procedure to obtain the content specification.
    element_decl,
    content_spec_start: ContentSpecStart,
    children_tok: ?ChildrenToken,
    mixed_content_spec_end: MixedContentSpecEnd,

    pub const ContentSpecStart = enum {
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
        /// if the list contained at least one name, `.content_spec = .zero_or_many`,
        /// otherwise, if the list was empty, it may be either `.one` or `.zero_or_many`.
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

const State = enum {
    end,
    blank,
    pi,

    element_start,
    element_mixed_list_name,
    element_mixed_list_rparen,
    element_mixed_empty_rparen,

    element_children_lparen_tag_token,
    element_children_lparen_lparen,
    element_children_lparen,
    element_children,
};

fn nextMarkerOrSegmentImpl(
    comptime MaybeReader: ?type,
    scanner: *InternalSubsetScanner(MaybeReader),
    comptime ret_type: enum { marker, segment },
) !switch (ret_type) {
    .marker => ScanMarker,
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
                                .tag_token => unreachable,
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

                .comment => switch (try parse_helper.handleCommentSkip(tokenizer, MaybeReader, mbr)) {
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
                        break .{ .content_spec_start = switch (empty_or_any) {
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
                            break .{ .content_spec_start = .mixed };
                        },

                        .lparen,
                        .tag_token,
                        => @panic("TODO"),
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

                switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                    else => return ScanError.UnexpectedToken,
                    .eof => return ScanError.UnexpectedEof,
                    .tag_whitespace => unreachable,

                    .rparen => scanner.state = .element_mixed_list_rparen,

                    .pipe => switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                        else => return ScanError.UnexpectedToken,
                        .tag_whitespace => unreachable,
                        .tag_token => {},
                    },
                }

                break null;
            },
        },
        .element_mixed_list_rparen,
        .element_mixed_empty_rparen,
        => |tag| switch (ret_type) {
            // this goes second
            .marker => {
                const mixed_content_spec_end: ScanMarker.MixedContentSpecEnd = switch (try parse_helper.nextTokenType(tokenizer, .dtd, MaybeReader, mbr)) {
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
                    .asterisk => blk: {
                        switch (try parse_helper.nextTokenTypeIgnoreTagWhitespace(tokenizer, .dtd, MaybeReader, mbr)) {
                            else => return ScanError.UnexpectedToken,
                            .eof => return ScanError.UnexpectedEof,
                            .tag_whitespace => unreachable,
                            .angle_bracket_right => {},
                        }
                        break :blk .zero_or_many;
                    },
                    .eof => return ScanError.UnexpectedEof,
                    else => return ScanError.UnexpectedToken,
                };

                switch (tag) {
                    .element_mixed_list_rparen => switch (mixed_content_spec_end) {
                        .one => return ScanError.MissingAsterisk,
                        .zero_or_many => {},
                    },
                    .element_mixed_empty_rparen => {},
                    else => unreachable,
                }

                scanner.state = .blank;
                break .{ .mixed_content_spec_end = mixed_content_spec_end };
            },

            // this goes first
            .segment => break null,
        },

        .element_children_lparen_tag_token,
        .element_children_lparen_lparen,
        .element_children_lparen,
        .element_children,
        => |tag| {
            _ = tag;
            @panic("TODO");
        },
    };
}

const std = @import("std");
const assert = std.debug.assert;

const builtin = @import("builtin");

const iksemel = @import("iksemel.zig");
const Tokenizer = iksemel.Tokenizer;

const parse_helper = @import("parse_helper.zig");
