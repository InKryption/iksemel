//! The lowest level API for interpreting an XML document.
//! While simple, it provides little to no safety in the event of API misuse.
//! It has two possible modes of operation: streaming and non-streaming.
//!
//! * Streaming: tokenizer is initialized with `initStreaming`, requires
//! the programmer to make use of the `feedInput` method after encountering
//! `error.BufferUnderrun` while using the `nextTypeStream` and `nextSrcStream`
//! methods. `feedEof` must be used to signal the end of the XML source.
//!
//! * Non-streaming: tokenizer is initialized with `initComplete`, or with
//! `initStreaming` before eventually calling `feedEof`. This mode allows
//! use of the standard API (`nextTypeStream`, `nextSrcStream`), in addition
//! to the mirror methods `nextTypeNoUnderrun` & `nextSrcNoUnderrun`, which
//! take advantage of the assumption that the entirety of the of the XML source
//! has been given. The `feedInput` and `feedEof` methods are illegal in this mode.
//!
//! Important to note: if the tokenizer was first initialized with `initStreaming`,
//! and then becomes non-streaming via a call to `feedEof`, immediately following
//! a call to `feedInput` as a response to `error.BufferUnderrun`, the currently
//! in-bound token may still be returned partially, as part of it may have existed
//! in the previous buffer.
//!
//! Any references in documentation to `nextTypeStream` apply equally to `nextTypeNoUnderrun`.
//! Any references in documentation to `nextSrcStream` apply equally to `nextSrcNoUnderrun`.
//!
//! The tokenization of a valid document requires no special considerations,
//! however the tokenizer is error-tolerant, and defines specially recognized
//! invalid cases which should be treated as failures at some point, if not
//! at the point of tokenization.

const std = @import("std");
const assert = std.debug.assert;
const builtin = @import("builtin");

const Tokenizer = @This();
src: []const u8,
index: usize,
state: State,
eof_specified: bool,

/// Initializes the `Tokenizer` with the full input.
/// Calling `feedInput` or `feedEof` is illegal.
/// Treating `error.BufferUnderrun` as `unreachable` is safe.
/// Equivalent initialising a streaming tokenizer, feeding it
/// `src`, and then feeding it eof.
pub inline fn initComplete(src: []const u8) Tokenizer {
    var tokenizer = Tokenizer.initStreaming();
    tokenizer.feedInput(src);
    tokenizer.feedEof();
    return tokenizer;
}

pub inline fn initStreaming() Tokenizer {
    return .{
        .src = "",
        .index = 0,
        .state = .general,
        .eof_specified = false,
    };
}

/// Should only be called to feed more input after
/// encountering `error.BufferUnderrun`, or directly after initialization.
///
/// If there is no more input to feed, call `feedEof`.
/// Calls to this are illegal after `feedEof` has been called.
///
/// The memory pointed to by `src` must remain valid until
/// the next call to `feedInput`, or until the last call to
/// `nextSrcStream` after a call to `feedEof`.
pub inline fn feedInput(tokenizer: *Tokenizer, src: []const u8) void {
    assert(!tokenizer.eof_specified);
    assert(tokenizer.index == tokenizer.src.len);
    tokenizer.src = src;
    tokenizer.index = 0;
}

/// Inform the tokenizer that the entirety of the XML source has been
/// supplied directly after a call to `feedInput`, or directly after
/// encountering `error.BufferUnderrun`.
/// Subsequent calls to this are illegal after the first call.
pub fn feedEof(tokenizer: *Tokenizer) void {
    assert(!tokenizer.eof_specified);
    assert(tokenizer.index == 0 or tokenizer.index == tokenizer.src.len);
    tokenizer.eof_specified = true;
}

pub const BufferError = error{BufferUnderrun};

pub const NextTypeError = BufferError;
/// Returns type of the next token. There are two possibilities:
/// * The token type represents a builtin syntactic construct for which
///   there is no further textual information to retrieve.
///   In this case, the caller should simply call `nextTypeStream` again to
///   obtain the following token type.
///
/// * The token indicates some textual information which isn't a builtin
///   syntactic construct, like identifiers in markup and textual data.
///   In this case, the caller should call `nextSrcStream` until it returns null.
pub inline fn nextTypeStream(tokenizer: *Tokenizer) NextTypeError!TokenType {
    return tokenizer.nextTypeOrSrcImpl(.type);
}

/// Same as `nextTypeStream`, assuming that `error.BufferUnderrun` is impossible (eof has already been fed).
pub inline fn nextTypeNoUnderrun(tokenizer: *Tokenizer) TokenType {
    return tokenizer.nextTypeOrSrcImpl(.type_no_underrun);
}

pub const NextSrcError = BufferError;
/// It is guaranteed to return a non-null value once. After the initial non-null value is
/// acquired, this must be called again until it returns null, concatenating the returned
/// token sources in order. During this chain of calls it may return `error.BufferUnderrun`,
/// in which case the caller should invoke `feedInput` or `feedEof`, and then try again.
///
/// For a non-streaming tokenizer, see `nextSrcAssumeUnderrun`.
pub inline fn nextSrcStream(tokenizer: *Tokenizer) NextSrcError!?TokenSrc {
    return tokenizer.nextTypeOrSrcImpl(.src);
}

/// Equivalent to `nextSrcStream`, but returns the full token source in one call.
/// If the returned token is a literal, and this is following a call to `feedInput`,
/// it may not be converted into a range.
pub inline fn nextSrcNoUnderrun(tokenizer: *Tokenizer) TokenSrc {
    defer assert(tokenizer.nextTypeOrSrcImpl(.src_no_underrun) == null);
    return tokenizer.nextTypeOrSrcImpl(.src_no_underrun).?;
}

pub const TokenType = enum(u8) {
    /// A run of any non-markup characters. Its meaning is dependent on
    /// the context in which it's being scanned.
    ///
    /// Whatever context it is in, it does not contain any data which
    /// would result in a tokenization error or ambiguity.
    ///
    /// Outside of any markup, this is simply part of the data of the
    /// parent element, or whitespace/unexpected character data before/after
    /// the root element.
    text_data,
    /// A run of one or more whitespace characters inside a markup tag.
    tag_whitespace,
    /// A run of one or more non-whitespace characters inside a markup tag.
    /// The way in which it is delimited from other data and its meaning are
    /// dependent on the context in which it's being scanned.
    tag_token,
    /// Some form of contextually invalid token, not featuring whitespace.
    invalid_token,

    /// The end of the XML source.
    /// This is the last token that will appear.
    /// Terminates the token sequence.
    /// The tokenizer will continue to return this after returning it once.
    eof,

    /// The '=' token.
    equals,

    /// The '(' token.
    lparen,
    /// The ')' token.
    rparen,

    /// The '|' token.
    pipe,
    /// The ',' token.
    comma,
    /// The '?' token.
    qmark,
    /// The '*' token.
    asterisk,
    /// The '+' token.
    plus,

    /// The '/' token.
    slash,

    /// The '%' token.
    ///
    /// Often followed by `.tag_token` and then `.semicolon`, however it can equally be followed
    /// by other DTD tokens. Whether or not it represents the start of a Parsed Entity Reference
    /// depends on these contextual details, and is left to the programmer.
    percent,

    /// The "'" token.
    quote_single,
    /// The '"' token.
    quote_double,

    /// The '<' token.
    ///
    /// If outside of any other markup, it starts an element open tag, and
    /// the subsequent token sequence will be one of:
    /// * `.tag_token`.
    /// * `.tag_whitespace`.
    /// * `.equals`.
    /// * `.quote_single` where the subsequent token sequence will be one of the following:
    ///    + `.text_data`.
    ///    + `.ampersand` followed by a token sequence as described in its own documentation.
    ///    + `.angle_bracket_left` as an invalid token ('<' in the attribute value).
    ///    + `.quote_single` ending the token sequence.
    ///    + `.eof`.
    /// * `.quote_double` following sequence is equivalent as for `.quote_single`, replacing it with `.quote_double`.
    /// * `.slash`.
    /// * `.angle_bracket_right`.
    /// * `.eof`.
    angle_bracket_left,
    /// The '>' token.
    angle_bracket_right,

    /// The '[' token.
    square_bracket_left,
    /// The ']' token.
    square_bracket_right,

    /// The '&' token.
    ///
    /// Represents the start of a Character or Entity reference.
    /// The subsequent token sequence will be one of:
    /// * `.tag_token`.
    /// * `.semicolon`.
    /// * `.invalid_reference_end`.
    ampersand,
    /// The ';' token.
    ///
    /// Ends the token sequence.
    semicolon,
    /// Encountered a terminating character (or EOF) other than the ';' token after the ampersand.
    /// This token simply represents the absence of the ';' token.
    /// Ends the token sequence.
    invalid_reference_end,

    /// The '<?' token.
    ////
    /// The subsequent token sequence will be:
    /// * `.text_data`.
    /// * `.pi_end` ending the token sequence.
    /// * `.eof`.
    pi_start,
    /// The '?>' token, terminating the PI tag.
    ///
    /// Ends the token sequence.
    pi_end,

    /// The '<![CDATA[' token.
    ///
    /// Starts a CDATA Section.
    /// The subsequent token sequence will be one of:
    /// * `.text_data`.
    /// * `.cdata_end` ending the token sequence.
    /// * `.eof`.
    cdata_start,
    /// A token which partially matches the '<![CDATA[' token.
    ///
    /// The source for the partial match is returned, and then afterwards
    /// the subsequent token sequence will be the same as for `.cdata_start`.
    invalid_cdata_start,
    /// The ']]>' token.
    ///
    /// Ends the CDATA Section, or is an invalid if not preceeded by a matching `.data` token.
    cdata_end,

    /// The '<!--' token.
    ///
    /// The subsequent token sequence will be one of:
    /// * `.text_data`.
    /// * `.invalid_comment_dash_dash`.
    /// * `.invalid_comment_end_triple_dash`.
    /// * `.comment_end`.
    /// * `.eof`.
    comment_start,
    /// The '<!-' token.
    ///
    /// The subsequent token sequence will be the same as for `.comment_start`.
    invalid_comment_start_single_dash,
    /// The invalid token '--'.
    invalid_comment_dash_dash,
    /// The invalid token '--->'.
    invalid_comment_end_triple_dash,
    /// Indicates '-->' after a comment.
    ///
    /// Ends the token sequence.
    comment_end,

    /// The '<!DOCTYPE' token.
    ///
    /// Starts the DTD declaration.
    /// The subsequent token sequence will be one of:
    /// * `.tag_token`.
    /// * `.tag_whitespace`.
    /// * `.invalid_token`.
    /// * `.lparen`.
    /// * `.rparen`.
    /// * `.qmark`.
    /// * `.asterisk`.
    /// * `.pipe`.
    /// * `.comma`.
    /// * `.percent`.
    /// * `.semicolon`.
    /// * `.quote_single`, followed optionally by `.text_data`, and then always by either `.quote_single` or `.eof`.
    /// * `.quote_double`, where the subsequent token sequence will be the same as for `.quote_single`.
    /// * `.square_bracket_left`.
    /// * `.square_bracket_right`.
    /// * `.element_decl`.
    /// * `.entity_decl`.
    /// * `.attlist_decl`.
    /// * `.notation_decl`.
    /// * `.pi_start`.
    /// * `.comment_start`.
    /// * `.angle_bracket_right`, terminating the token sequence.
    dtd_start,
    /// A token which partially matches the '<!DOCTYPE' token.
    ///
    /// The source for the partial match is returned, and then afterwards
    /// the subsequent token sequence will be the same as for `.dtd_start`.
    invalid_dtd_start,

    /// The '<!ELEMENT' token.
    ///
    /// The subsequent token sequence will be equivalent as for `.dtd`, and terminated by any token other than:
    /// * `.lparen`
    /// * `.rparen`
    /// * `.pipe`
    /// * `.comma`
    /// * `.qmark`
    /// * `.asterisk`
    /// * `.plus`
    /// * `.percent`
    /// * `.semicolon`
    /// * `.quote_single`
    /// * `.quote_double`
    /// In a valid DTD, the terminating token would be `.angle_bracket_right` ('>').
    element_decl,
    /// The '<!ENTITY' token.
    ///
    /// The subsequent token sequence will be equivalent to the one for `.element_decl`.
    entity_decl,
    /// The '<!ATTLIST' token.
    ///
    /// The subsequent token sequence will be equivalent to the one for `.element_decl`.
    attlist_decl,
    /// The '<!NOTATION' token.
    ///
    /// The subsequent token sequence will be equivalent to the one for `.element_decl`.
    notation_decl,
    /// A segment of one of '<!ELEMENT', '<!ENTITY', '<!ATTLIST', or '<!NOTATION'.
    ///
    /// First the source for the segment will be returned, and then
    /// the subsequent token sequence will be equivalent to the one for `.element_decl`.
    invalid_decl,

    /// The '<!' token.
    ///
    /// This is returned when '<!' is followed by a sequence
    /// which does not ultimately form a recognized markup tag.
    ///
    /// Inside standard element character data, this is returned
    /// as a standalone token and doesn't affect the nesting/context.
    ///
    /// Inside a DTD, the subsequent token sequence will be equivalent
    /// to the one for `.element_decl`.
    invalid_angle_bracket_left_bang,

    /// Whether or not the token represents any text to be returned by `nextSrcStream`.
    pub inline fn hasString(token_type: TokenType) bool {
        return switch (token_type) {
            .text_data,
            .tag_whitespace,
            .tag_token,
            .invalid_token,

            .invalid_cdata_start,
            .invalid_dtd_start,
            .invalid_decl,
            => true,

            else => false,
        };
    }

    pub inline fn stringLiteral(token_type: TokenType) ?[]const u8 {
        return switch (token_type) {
            .text_data => null,
            .tag_whitespace => null,
            .tag_token => null,
            .invalid_token => null,

            .eof => null,

            .equals => "=",

            .lparen => "(",
            .rparen => ")",

            .pipe => "|",
            .comma => ",",
            .qmark => "?",
            .asterisk => "*",
            .plus => "+",

            .slash => "/",

            .percent => "%",

            .quote_single => "\'",
            .quote_double => "\"",

            .angle_bracket_left => "<",
            .angle_bracket_right => ">",

            .square_bracket_left => "[",
            .square_bracket_right => "]",

            .ampersand => "&",
            .semicolon => ";",
            .invalid_reference_end => null,

            .pi_start => "<?",
            .pi_end => "?>",

            .invalid_angle_bracket_left_bang => "<!",

            .cdata_start => "<![CDATA[",
            .invalid_cdata_start => null,
            .cdata_end => "]]>",

            .comment_start => "<!--",
            .invalid_comment_start_single_dash => "<!-",
            .invalid_comment_dash_dash => "--",
            .invalid_comment_end_triple_dash => "--->",
            .comment_end => "-->",

            .dtd_start => "<!DOCTYPE",
            .invalid_dtd_start => null,

            .element_decl => "<!ELEMENT",
            .entity_decl => "<!ENTITY",
            .attlist_decl => "<!ATTLIST",
            .notation_decl => "<!NOTATION",
        };
    }
};

/// The set of codepoints defined as whitespace. They are all
/// exactly one byte in size.
pub const whitespace_set: []const u8 = &[_]u8{
    '\u{20}',
    '\u{09}',
    '\u{0D}',
    '\u{0A}',
};

fn nextTypeOrSrcImpl(
    tokenizer: *Tokenizer,
    comptime ret_type: enum {
        type,
        type_no_underrun,
        src,
        src_no_underrun,
    },
) switch (ret_type) {
    .type => NextTypeError!TokenType,
    .type_no_underrun => TokenType,
    .src => NextSrcError!?TokenSrc,
    .src_no_underrun => ?TokenSrc,
} {
    const helper = struct {
        const dtd_terminal_characters = &[_]u8{
            '[',  ']', '(', ')',
            '?',  '*', '+', '|',
            ',',  '%', ';', '\'',
            '\"', '<', '>',
        };

        inline fn rangeInit(range_start: usize, range_end: usize) TokenSrc {
            return .{ .range = .{ .start = range_start, .end = range_end } };
        }
        inline fn literalInit(literal: TokenSrc.Literal) TokenSrc {
            return .{ .literal = literal };
        }
    };

    const ret_kind: enum { type, src }, //
    const assert_eof_specified: bool //
    = comptime switch (ret_type) {
        .type => .{ .type, false },
        .src => .{ .src, false },
        .type_no_underrun => .{ .type, true },
        .src_no_underrun => .{ .src, true },
    };

    const src = tokenizer.src;
    assert(!assert_eof_specified or tokenizer.eof_specified);
    const eof_specified = assert_eof_specified or tokenizer.eof_specified;

    return while (tokenizer.index != src.len or eof_specified) switch (tokenizer.state) {
        .eof => switch (ret_kind) {
            .type => break .eof,
            .src => break null,
        },

        .general => switch (ret_kind) {
            .type => if (tokenizer.index == src.len)
                break .eof
            else switch (src[tokenizer.index]) {
                ']' => {
                    tokenizer.state = .@"general,]";
                    tokenizer.index += 1;
                    continue;
                },
                '<' => {
                    tokenizer.state = .@"general,<";
                    tokenizer.index += 1;
                    continue;
                },
                '&' => {
                    tokenizer.state = .@"general,&";
                    tokenizer.index += 1;
                    break .ampersand;
                },
                else => break .text_data,
            },
            .src => {
                if (tokenizer.index == src.len) break null;
                const str_start = tokenizer.index;
                const str_end = std.mem.indexOfAnyPos(u8, src, tokenizer.index, &[_]u8{ ']', '&', '<' }) orelse src.len;
                tokenizer.index = str_end;
                if (str_start != str_end) break helper.rangeInit(str_start, str_end);
                if (src[tokenizer.index] != ']') break null;
                tokenizer.state = .@"general,]";
                tokenizer.index += 1;
                continue;
            },
        },
        .@"general,]" => switch (ret_kind) {
            .type => {
                if (tokenizer.index == src.len) break .text_data;
                if (src[tokenizer.index] != ']') break .text_data;
                tokenizer.state = .@"general,]]";
                tokenizer.index += 1;
                continue;
            },
            .src => {
                if (tokenizer.index == src.len) {
                    tokenizer.state = .eof;
                    break helper.literalInit(.@"]");
                }
                if (src[tokenizer.index] != ']') {
                    tokenizer.state = .general;
                    break helper.literalInit(.@"]");
                }
                tokenizer.state = .@"general,]]";
                tokenizer.index += 1;
                continue;
            },
        },
        .@"general,]]" => switch (ret_kind) {
            .type => {
                if (tokenizer.index == src.len) break .text_data;
                if (src[tokenizer.index] != '>') break .text_data;
                tokenizer.state = .general;
                tokenizer.index += 1;
                break .cdata_end;
            },
            .src => {
                if (tokenizer.index == src.len) {
                    tokenizer.state = .eof;
                    break helper.literalInit(.@"]]");
                }
                if (src[tokenizer.index] != '>') {
                    tokenizer.state = .general;
                    break helper.literalInit(.@"]]");
                }
                break null;
            },
        },
        .@"general,&" => switch (ret_kind) {
            .type => {
                if (tokenizer.index == src.len) {
                    tokenizer.state = .eof;
                    break .invalid_reference_end;
                }
                if (src[tokenizer.index] == ';') {
                    tokenizer.state = .general;
                    tokenizer.index += 1;
                    break .semicolon;
                }
                if (std.mem.indexOfScalar(u8, whitespace_set ++ &[_]u8{ ']', '&', '<' }, src[tokenizer.index]) != null) {
                    tokenizer.state = .general;
                    break .invalid_reference_end;
                }
                break .tag_token;
            },
            .src => {
                if (tokenizer.index == src.len) break null;
                const str_start = tokenizer.index;
                const str_end = std.mem.indexOfAnyPos(u8, src, tokenizer.index, whitespace_set ++ &[_]u8{ ';', ']', '&', '<' }) orelse src.len;
                tokenizer.index = str_end;
                if (str_start != tokenizer.index) {
                    break helper.rangeInit(str_start, tokenizer.index);
                }
                break null;
            },
        },
        .@"general,<" => switch (ret_kind) {
            .type => {
                if (tokenizer.index == src.len) {
                    tokenizer.state = .eof;
                    break .angle_bracket_left;
                }
                switch (src[tokenizer.index]) {
                    '?' => {
                        tokenizer.state = .pi;
                        tokenizer.index += 1;
                        break .pi_start;
                    },
                    '!' => {
                        tokenizer.state = .@"general,<!";
                        tokenizer.index += 1;
                        continue;
                    },
                    else => {
                        tokenizer.state = .element_tag;
                        break .angle_bracket_left;
                    },
                }
            },
            .src => unreachable,
        },

        .@"general,<!" => switch (ret_kind) {
            .type => {
                if (tokenizer.index == src.len) {
                    tokenizer.state = .eof;
                    break .invalid_angle_bracket_left_bang;
                }
                tokenizer.state = switch (src[tokenizer.index]) {
                    '-' => .@"general,<!-",
                    'D' => .@"<!D",
                    '[' => .@"<![",
                    else => {
                        tokenizer.state = .general;
                        break .invalid_angle_bracket_left_bang;
                    },
                };
                tokenizer.index += 1;
                continue;
            },
            .src => unreachable,
        },
        .@"general,<!-",
        .@"dtd,<!-",
        => |tag| switch (ret_kind) {
            .type => {
                if (tokenizer.index == src.len) {
                    tokenizer.state = .eof;
                    break .invalid_comment_start_single_dash;
                }
                tokenizer.state = switch (tag) {
                    .@"general,<!-" => .@"general,<!--",
                    .@"dtd,<!-" => .@"dtd,<!--",
                    else => unreachable,
                };
                if (src[tokenizer.index] != '-') {
                    break .invalid_comment_start_single_dash;
                }
                tokenizer.index += 1;
                break .comment_start;
            },
            .src => unreachable,
        },

        .pi, .dtd_pi => |tag| switch (ret_kind) {
            .type => {
                if (tokenizer.index == src.len) {
                    tokenizer.state = .eof;
                    break .eof;
                }
                if (src[tokenizer.index] != '?') {
                    break .text_data;
                }
                tokenizer.state = switch (tag) {
                    .pi => .@"pi,?",
                    .dtd_pi => .@"dtd_pi,?",
                    else => unreachable,
                };
                tokenizer.index += 1;
                continue;
            },
            .src => {
                if (tokenizer.index == src.len) {
                    tokenizer.state = .eof;
                    break null;
                }

                const str_start = tokenizer.index;
                const str_end = std.mem.indexOfScalarPos(u8, src, tokenizer.index, '?') orelse src.len;
                tokenizer.index = str_end;
                if (str_start != str_end) {
                    break helper.rangeInit(str_start, str_end);
                }
                tokenizer.state = switch (tag) {
                    .pi => .@"pi,?",
                    .dtd_pi => .@"dtd_pi,?",
                    else => unreachable,
                };
                tokenizer.index += 1;
                continue;
            },
        },
        .@"pi,?",
        .@"dtd_pi,?",
        => |tag| switch (ret_kind) {
            .type => {
                if (tokenizer.index == src.len) break .text_data;
                if (src[tokenizer.index] != '>') break .text_data;
                tokenizer.state = switch (tag) {
                    .@"pi,?" => .general,
                    .@"dtd_pi,?" => .dtd,
                    else => unreachable,
                };
                tokenizer.index += 1;
                break .pi_end;
            },
            .src => {
                const state_on_unmatched: State = switch (tag) {
                    .@"pi,?" => .pi,
                    .@"dtd_pi,?" => .dtd_pi,
                    else => unreachable,
                };
                if (tokenizer.index == src.len) {
                    tokenizer.state = state_on_unmatched;
                    break helper.literalInit(.@"?");
                }
                if (src[tokenizer.index] != '>') {
                    tokenizer.state = state_on_unmatched;
                    break helper.literalInit(.@"?");
                }
                break null;
            },
        },

        .element_tag => switch (ret_kind) {
            .type => {
                if (tokenizer.index == src.len) {
                    tokenizer.state = .eof;
                    break .eof;
                }
                switch (src[tokenizer.index]) {
                    inline '\'', '\"', '/', '=', '>' => |char| {
                        const result: TokenType, //
                        const maybe_state: ?State //
                        = comptime switch (char) {
                            '\'' => .{ .quote_single, .element_tag_sq },
                            '\"' => .{ .quote_double, .element_tag_dq },
                            '/' => .{ .slash, null },
                            '=' => .{ .equals, null },
                            '>' => .{ .angle_bracket_right, .general },
                            else => unreachable,
                        };
                        if (maybe_state) |new_state| tokenizer.state = new_state;
                        tokenizer.index += 1;
                        break result;
                    },
                    else => |char| {
                        const not_whitespace = std.mem.indexOfScalar(u8, whitespace_set, char) == null;
                        if (not_whitespace) break .tag_token;
                        tokenizer.state = .element_tag_whitespace;
                        break .tag_whitespace;
                    },
                }
            },
            .src => {
                if (tokenizer.index == src.len) break null;
                const str_start = tokenizer.index;
                const str_end = std.mem.indexOfAnyPos(u8, src, tokenizer.index, whitespace_set ++ &[_]u8{ '\'', '\"', '/', '=', '>' }) orelse src.len;
                tokenizer.index = str_end;
                if (str_start != str_end) {
                    break helper.rangeInit(str_start, str_end);
                }
                break null;
            },
        },

        .element_tag_whitespace => switch (ret_kind) {
            .type => unreachable,
            .src => {
                if (tokenizer.index == src.len) break null;
                const str_start = tokenizer.index;
                const str_end = std.mem.indexOfNonePos(u8, src, tokenizer.index, whitespace_set) orelse src.len;
                tokenizer.index = str_end;
                if (str_start != str_end) {
                    break helper.rangeInit(str_start, str_end);
                }
                tokenizer.state = .element_tag;
                break null;
            },
        },

        .element_tag_sq,
        .element_tag_dq,
        => |tag| switch (ret_kind) {
            .type => {
                if (tokenizer.index == src.len) break .eof;
                const matching_quote_char: u8, //
                const matching_quote_type: TokenType, //
                const on_ref_state: State //
                = switch (tag) {
                    .element_tag_sq => .{ '\'', .quote_single, .@"element_tag_sq,&" },
                    .element_tag_dq => .{ '\"', .quote_double, .@"element_tag_dq,&" },
                    else => unreachable,
                };
                if (src[tokenizer.index] == matching_quote_char) {
                    tokenizer.state = .element_tag;
                    tokenizer.index += 1;
                    break matching_quote_type;
                }
                switch (src[tokenizer.index]) {
                    '&' => {
                        tokenizer.state = on_ref_state;
                        tokenizer.index += 1;
                        break .ampersand;
                    },
                    '<' => {
                        tokenizer.index += 1;
                        break .angle_bracket_left;
                    },
                    else => break .text_data,
                }
            },
            .src => {
                if (tokenizer.index == src.len) break null;
                const matching_quote_char: u8 = switch (tag) {
                    .element_tag_sq => '\'',
                    .element_tag_dq => '\"',
                    else => unreachable,
                };
                const str_start = tokenizer.index;
                const str_end = std.mem.indexOfAnyPos(u8, src, tokenizer.index, &[_]u8{ matching_quote_char, '&', '<' }) orelse src.len;
                tokenizer.index = str_end;
                if (str_start != str_end) {
                    break helper.rangeInit(str_start, str_end);
                }
                break null;
            },
        },
        .@"element_tag_sq,&",
        .@"element_tag_dq,&",
        => |tag| switch (ret_kind) {
            .type => {
                if (tokenizer.index == src.len) break .eof;
                const matching_quote_char: u8, //
                const matching_state: State //
                = switch (tag) {
                    .@"element_tag_sq,&" => .{ '\'', .element_tag_sq },
                    .@"element_tag_dq,&" => .{ '\"', .element_tag_dq },
                    else => unreachable,
                };
                if (src[tokenizer.index] == matching_quote_char) {
                    tokenizer.state = matching_state;
                    break .invalid_reference_end;
                }
                if (src[tokenizer.index] == ';') {
                    tokenizer.state = matching_state;
                    tokenizer.index += 1;
                    break .semicolon;
                }
                break .tag_token;
            },
            .src => {
                if (tokenizer.index == src.len) break null;
                const matching_quote_char: u8 = switch (tag) {
                    .@"element_tag_sq,&" => '\'',
                    .@"element_tag_dq,&" => '\"',
                    else => unreachable,
                };
                const str_start = tokenizer.index;
                const str_end = std.mem.indexOfAnyPos(u8, src, tokenizer.index, whitespace_set ++ &[_]u8{ ';', matching_quote_char, '&', '<' }) orelse src.len;
                tokenizer.index = str_end;
                if (str_start != str_end) {
                    break helper.rangeInit(str_start, str_end);
                }
                break null;
            },
        },

        .@"general,<!--",
        .@"dtd,<!--",
        => |tag| switch (ret_kind) {
            .type => {
                if (tokenizer.index == src.len) {
                    tokenizer.state = .eof;
                    break .eof;
                }
                if (src[tokenizer.index] != '-') {
                    break .text_data;
                }
                tokenizer.state = switch (tag) {
                    .@"general,<!--" => .@"general,<!--,-",
                    .@"dtd,<!--" => .@"dtd_comment,-",
                    else => unreachable,
                };
                tokenizer.index += 1;
                continue;
            },
            .src => {
                if (tokenizer.index == src.len) {
                    tokenizer.state = .eof;
                    break null;
                }
                const str_start = tokenizer.index;
                const str_end = std.mem.indexOfScalarPos(u8, src, tokenizer.index, '-') orelse src.len;
                tokenizer.index = str_end;
                if (str_start != str_end) {
                    break helper.rangeInit(str_start, str_end);
                }
                if (src[tokenizer.index] == '-') {
                    tokenizer.state = switch (tag) {
                        .@"general,<!--" => .@"general,<!--,-",
                        .@"dtd,<!--" => .@"dtd_comment,-",
                        else => unreachable,
                    };
                    tokenizer.index += 1;
                    continue;
                }
                break null;
            },
        },
        .@"general,<!--,-",
        .@"dtd_comment,-",
        => |tag| switch (ret_kind) {
            .type => {
                if (tokenizer.index == src.len) break .text_data;
                if (src[tokenizer.index] != '-') break .text_data;
                tokenizer.state = switch (tag) {
                    .@"general,<!--,-" => .@"general,<!--,--",
                    .@"dtd_comment,-" => .@"dtd_comment,--",
                    else => unreachable,
                };
                tokenizer.index += 1;
                continue;
            },
            .src => {
                const state_on_unmatched: State, //
                const state_on_match: State //
                = switch (tag) {
                    .@"general,<!--,-" => .{ .@"general,<!--", .@"general,<!--,--" },
                    .@"dtd_comment,-" => .{ .@"dtd,<!--", .@"dtd_comment,--" },
                    else => unreachable,
                };
                if (tokenizer.index == src.len) {
                    tokenizer.state = state_on_unmatched;
                    break helper.literalInit(.@"-");
                }
                if (src[tokenizer.index] != '-') {
                    tokenizer.state = state_on_unmatched;
                    break helper.literalInit(.@"-");
                }
                tokenizer.state = state_on_match;
                tokenizer.index += 1;
                continue;
            },
        },
        .@"general,<!--,--",
        .@"dtd_comment,--",
        => |tag| switch (ret_kind) {
            .type => {
                const state_on_unmatched: State, //
                const state_on_dash: State, //
                const state_on_rab: State //
                = switch (tag) {
                    .@"general,<!--,--" => .{ .@"general,<!--", .@"general,<!--,---", .general },
                    .@"dtd_comment,--" => .{ .@"dtd,<!--", .@"dtd_comment,---", .dtd },
                    else => unreachable,
                };
                if (tokenizer.index == src.len) {
                    tokenizer.state = state_on_unmatched;
                    break .invalid_comment_dash_dash;
                }
                switch (src[tokenizer.index]) {
                    '-' => {
                        tokenizer.state = state_on_dash;
                        tokenizer.index += 1;
                        continue;
                    },
                    '>' => {
                        tokenizer.state = state_on_rab;
                        tokenizer.index += 1;
                        break .comment_end;
                    },
                    else => {
                        tokenizer.state = state_on_unmatched;
                        break .invalid_comment_dash_dash;
                    },
                }
            },
            .src => {
                if (tokenizer.index == src.len) break null;
                if (src[tokenizer.index] == '-') tokenizer.state = switch (tag) {
                    .@"general,<!--,--" => .@"general,<!--,---",
                    .@"dtd_comment,--" => .@"dtd_comment,---",
                    else => unreachable,
                };
                break null;
            },
        },
        .@"general,<!--,---",
        .@"dtd_comment,---",
        => |tag| switch (ret_kind) {
            .type => {
                const state_on_unmatched: State, //
                const state_on_dash: State, //
                const state_on_rab: State //
                = switch (tag) {
                    .@"general,<!--,---" => .{ .@"general,<!--,-", .@"general,<!--,--", .general },
                    .@"dtd_comment,---" => .{ .@"dtd_comment,-", .@"dtd_comment,--", .dtd },
                    else => unreachable,
                };
                if (tokenizer.index == src.len) {
                    tokenizer.state = state_on_unmatched;
                    break .invalid_comment_dash_dash;
                }
                switch (src[tokenizer.index]) {
                    '-' => {
                        tokenizer.state = state_on_dash;
                        tokenizer.index += 1;
                        break .invalid_comment_dash_dash;
                    },
                    '>' => {
                        tokenizer.state = state_on_rab;
                        tokenizer.index += 1;
                        break .invalid_comment_end_triple_dash;
                    },
                    else => {
                        tokenizer.state = state_on_unmatched;
                        break .invalid_comment_dash_dash;
                    },
                }
            },
            .src => unreachable,
        },

        .@"<![",
        .@"<![C",
        .@"<![CD",
        .@"<![CDA",
        .@"<![CDAT",
        .@"<![CDATA",
        => |tag| switch (ret_kind) {
            .type => {
                const expected_char: u8, //
                const state_on_match: State //
                = switch (tag) {
                    .@"<![" => .{ 'C', .@"<![C" },
                    .@"<![C" => .{ 'D', .@"<![CD" },
                    .@"<![CD" => .{ 'A', .@"<![CDA" },
                    .@"<![CDA" => .{ 'T', .@"<![CDAT" },
                    .@"<![CDAT" => .{ 'A', .@"<![CDATA" },
                    .@"<![CDATA" => .{ '[', .cdata },
                    else => unreachable,
                };
                if (tokenizer.index == src.len) break .invalid_cdata_start;
                if (src[tokenizer.index] != expected_char) break .invalid_cdata_start;
                tokenizer.state = state_on_match;
                tokenizer.index += 1;
                if (state_on_match != .cdata) continue;
                break .cdata_start;
            },
            .src => {
                tokenizer.state = .cdata_start_invalid;
                return helper.literalInit(switch (tag) {
                    inline //
                    .@"<![",
                    .@"<![C",
                    .@"<![CD",
                    .@"<![CDA",
                    .@"<![CDAT",
                    .@"<![CDATA",
                    => |itag| @field(TokenSrc.Literal, @tagName(itag)),
                    else => unreachable,
                });
            },
        },
        .cdata_start_invalid => switch (ret_kind) {
            .type => unreachable,
            .src => {
                if (tokenizer.index == src.len) {
                    tokenizer.state = .eof;
                    return null;
                }
                const str_start = tokenizer.index;
                const maybe_str_end = std.mem.indexOfAnyPos(u8, src, tokenizer.index, whitespace_set ++ &[_]u8{ '[', ']' }) orelse src.len;
                const ends_with_lbracket = maybe_str_end != src.len and src[maybe_str_end] == '[';
                const str_end = maybe_str_end + @intFromBool(ends_with_lbracket);
                tokenizer.index = str_end;
                if (str_start != str_end) {
                    return helper.rangeInit(str_start, str_end);
                }
                tokenizer.state = .general;
                return null;
            },
        },
        .cdata => switch (ret_kind) {
            .type => {
                if (tokenizer.index == src.len) {
                    tokenizer.state = .eof;
                    break .eof;
                }
                if (src[tokenizer.index] != ']') {
                    break .text_data;
                }
                tokenizer.state = .@"cdata,]";
                tokenizer.index += 1;
                continue;
            },
            .src => {
                if (tokenizer.index == src.len) {
                    tokenizer.state = .eof;
                    return null;
                }
                const str_start = tokenizer.index;
                const str_end = std.mem.indexOfScalarPos(u8, src, tokenizer.index, ']') orelse src.len;
                tokenizer.index = str_end;
                if (str_start != str_end) {
                    return helper.rangeInit(str_start, str_end);
                }
                tokenizer.state = .@"cdata,]";
                tokenizer.index += 1;
                continue;
            },
        },
        .@"cdata,]" => switch (ret_kind) {
            .type => {
                if (tokenizer.index == src.len) break .text_data;
                if (src[tokenizer.index] != ']') break .text_data;
                tokenizer.state = .@"cdata,]]";
                tokenizer.index += 1;
                continue;
            },
            .src => {
                if (tokenizer.index == src.len) {
                    tokenizer.state = .eof;
                    return helper.literalInit(.@"]");
                }
                if (src[tokenizer.index] != ']') {
                    tokenizer.state = .cdata;
                    return helper.literalInit(.@"]");
                }
                tokenizer.state = .@"cdata,]]";
                tokenizer.index += 1;
                continue;
            },
        },
        .@"cdata,]]" => switch (ret_kind) {
            .type => {
                if (tokenizer.index == src.len) break .text_data;
                if (src[tokenizer.index] != '>') break .text_data;
                tokenizer.state = .general;
                tokenizer.index += 1;
                break .cdata_end;
            },
            .src => {
                if (tokenizer.index == src.len) {
                    tokenizer.state = .eof;
                    return helper.literalInit(.@"]]");
                }
                switch (src[tokenizer.index]) {
                    '>' => return null,
                    ']' => {
                        tokenizer.index += 1;
                        return helper.literalInit(.@"]");
                    },
                    else => {
                        tokenizer.state = .cdata;
                        return helper.literalInit(.@"]]");
                    },
                }
            },
        },

        .@"<!D",
        .@"<!DO",
        .@"<!DOC",
        .@"<!DOCT",
        .@"<!DOCTY",
        .@"<!DOCTYP",
        => |tag| switch (ret_kind) {
            .type => {
                if (tokenizer.index == src.len) break .invalid_dtd_start;
                const expected_char: u8, //
                const state_on_match: State //
                = switch (tag) {
                    .@"<!D" => .{ 'O', .@"<!DO" },
                    .@"<!DO" => .{ 'C', .@"<!DOC" },
                    .@"<!DOC" => .{ 'T', .@"<!DOCT" },
                    .@"<!DOCT" => .{ 'Y', .@"<!DOCTY" },
                    .@"<!DOCTY" => .{ 'P', .@"<!DOCTYP" },
                    .@"<!DOCTYP" => .{ 'E', .dtd },
                    else => unreachable,
                };
                if (src[tokenizer.index] != expected_char) {
                    break .invalid_dtd_start;
                }
                tokenizer.state = state_on_match;
                tokenizer.index += 1;
                if (state_on_match != .dtd) continue;
                break .dtd_start;
            },
            .src => {
                tokenizer.state = .dtd_invalid_start;
                return helper.literalInit(switch (tag) {
                    inline //
                    .@"<!D",
                    .@"<!DO",
                    .@"<!DOC",
                    .@"<!DOCT",
                    .@"<!DOCTY",
                    .@"<!DOCTYP",
                    => |itag| @field(TokenSrc.Literal, @tagName(itag)),
                    else => unreachable,
                });
            },
        },
        .dtd,
        .dtd_subtag,
        => |tag| switch (ret_kind) {
            .type => {
                if (tokenizer.index == src.len) {
                    tokenizer.state = .eof;
                    break .eof;
                }
                switch (src[tokenizer.index]) {
                    // these cases do not affect the nesting context in any way
                    inline '(', ')', '?', '*', '+', '|', ',', '%', ';' => |char| {
                        tokenizer.index += 1;
                        break comptime switch (char) {
                            '(' => .lparen,
                            ')' => .rparen,
                            '?' => .qmark,
                            '*' => .asterisk,
                            '+' => .plus,
                            '|' => .pipe,
                            ',' => .comma,
                            '%' => .percent,
                            ';' => .semicolon,
                            else => unreachable,
                        };
                    },
                    // these quote cases retain the nesting context
                    '\'' => {
                        tokenizer.state = switch (tag) {
                            .dtd => .dtd_sq,
                            .dtd_subtag => .dtd_subtag_sq,
                            else => unreachable,
                        };
                        tokenizer.index += 1;
                        break .quote_single;
                    },
                    '\"' => {
                        tokenizer.state = switch (tag) {
                            .dtd => .dtd_dq,
                            .dtd_subtag => .dtd_subtag_dq,
                            else => unreachable,
                        };
                        tokenizer.index += 1;
                        break .quote_double;
                    },
                    // the rest of these specific cases modify the nesting context
                    '[' => {
                        switch (tag) {
                            .dtd => {},
                            .dtd_subtag => tokenizer.state = .dtd,
                            else => unreachable,
                        }
                        tokenizer.index += 1;
                        break .square_bracket_left;
                    },
                    ']' => {
                        switch (tag) {
                            .dtd => {},
                            .dtd_subtag => tokenizer.state = .dtd,
                            else => unreachable,
                        }
                        tokenizer.index += 1;
                        break .square_bracket_right;
                    },
                    '<' => {
                        tokenizer.state = .@"dtd,<";
                        tokenizer.index += 1;
                        continue;
                    },
                    '>' => {
                        tokenizer.state = switch (tag) {
                            .dtd => .general,
                            .dtd_subtag => .dtd,
                            else => unreachable,
                        };
                        tokenizer.index += 1;
                        break .angle_bracket_right;
                    },
                    else => {
                        const non_whitespace = std.mem.indexOfScalar(u8, whitespace_set, src[tokenizer.index]) == null;
                        const token_state: State, //
                        const whitespace_state: State //
                        = switch (tag) {
                            .dtd => .{ .dtd_token, .dtd_whitespace },
                            .dtd_subtag => .{ .dtd_subtag_token, .dtd_subtag_whitespace },
                            else => unreachable,
                        };
                        tokenizer.state = if (non_whitespace) token_state else whitespace_state;
                        break if (non_whitespace) .tag_token else .tag_whitespace;
                    },
                }
            },
            .src => unreachable,
        },
        .dtd_sq,
        .dtd_dq,
        .dtd_subtag_sq,
        .dtd_subtag_dq,
        => |tag| switch (ret_kind) {
            .type => {
                if (tokenizer.index == src.len) {
                    tokenizer.state = .eof;
                    break .eof;
                }
                const matching_quote_char: u8, //
                const matching_quote_type: TokenType, //
                const on_match_state: State //
                = switch (tag) {
                    .dtd_sq => .{ '\'', .quote_single, .dtd },
                    .dtd_dq => .{ '\"', .quote_double, .dtd },
                    .dtd_subtag_sq => .{ '\'', .quote_single, .dtd_subtag },
                    .dtd_subtag_dq => .{ '\"', .quote_double, .dtd_subtag },
                    else => unreachable,
                };
                if (src[tokenizer.index] != matching_quote_char) {
                    break .text_data;
                }
                tokenizer.state = on_match_state;
                tokenizer.index += 1;
                break matching_quote_type;
            },
            .src => {
                if (tokenizer.index == src.len) {
                    tokenizer.state = .eof;
                    break null;
                }
                const matching_quote_char: u8 = switch (tag) {
                    .dtd_sq, .dtd_subtag_sq => '\'',
                    .dtd_dq, .dtd_subtag_dq => '\"',
                    else => unreachable,
                };
                const str_start = tokenizer.index;
                const str_end = std.mem.indexOfScalarPos(u8, src, tokenizer.index, matching_quote_char) orelse src.len;
                tokenizer.index = str_end;
                if (str_start != str_end) {
                    break helper.rangeInit(str_start, str_end);
                }
                break null;
            },
        },

        .dtd_token,
        .dtd_subtag_token,
        => |tag| switch (ret_kind) {
            .type => unreachable,
            .src => {
                if (tokenizer.index == src.len) {
                    tokenizer.state = .eof;
                    break null;
                }
                const str_start = tokenizer.index;
                const str_end = std.mem.indexOfAnyPos(u8, src, tokenizer.index, whitespace_set ++ helper.dtd_terminal_characters) orelse src.len;
                tokenizer.index = str_end;
                if (str_start != str_end) {
                    break helper.rangeInit(str_start, str_end);
                }
                tokenizer.state = switch (tag) {
                    .dtd_token => .dtd,
                    .dtd_subtag_token => .dtd_subtag,
                    else => unreachable,
                };
                break null;
            },
        },

        .dtd_whitespace,
        .dtd_subtag_whitespace,
        => |tag| switch (ret_kind) {
            .type => unreachable,
            .src => {
                if (tokenizer.index == src.len) {
                    tokenizer.state = .eof;
                    break null;
                }
                const str_start = tokenizer.index;
                const str_end = std.mem.indexOfNonePos(u8, src, tokenizer.index, whitespace_set) orelse src.len;
                tokenizer.index = str_end;
                if (str_start != str_end) {
                    break helper.rangeInit(str_start, str_end);
                }
                tokenizer.state = switch (tag) {
                    .dtd_whitespace => .dtd,
                    .dtd_subtag_whitespace => .dtd_subtag,
                    else => unreachable,
                };
                break null;
            },
        },

        .@"dtd,<" => switch (ret_kind) {
            .type => {
                if (tokenizer.index == src.len) {
                    tokenizer.state = .eof;
                    break .angle_bracket_left;
                }
                switch (src[tokenizer.index]) {
                    '?' => {
                        tokenizer.state = .dtd_pi;
                        tokenizer.index += 1;
                        break .pi_start;
                    },
                    '!' => {
                        tokenizer.state = .@"dtd,<!";
                        tokenizer.index += 1;
                        continue;
                    },
                    else => {
                        tokenizer.state = .dtd;
                        break .angle_bracket_left;
                    },
                }
            },
            .src => unreachable,
        },

        .@"dtd,<!" => switch (ret_kind) {
            .type => {
                if (tokenizer.index == src.len) {
                    tokenizer.state = .eof;
                    break .invalid_angle_bracket_left_bang;
                }
                switch (src[tokenizer.index]) {
                    inline '-', 'A', 'E', 'N' => |char| {
                        tokenizer.state = comptime switch (char) {
                            '-' => .@"dtd,<!-",
                            'E' => .@"dtd,<!E",
                            'A' => .@"dtd,<!A",
                            'N' => .@"dtd,<!N",
                            else => unreachable,
                        };
                        tokenizer.index += 1;
                        continue;
                    },
                    else => {
                        tokenizer.state = .dtd_subtag;
                        break .invalid_angle_bracket_left_bang;
                    },
                }
            },
            .src => unreachable,
        },

        .dtd_invalid_start,
        .dtd_subtag_invalid_start,
        => |tag| switch (ret_kind) {
            .type => unreachable,
            .src => {
                if (tokenizer.index == src.len) {
                    tokenizer.state = .eof;
                    return null;
                }
                const str_start = tokenizer.index;
                const str_end = std.mem.indexOfAnyPos(u8, src, tokenizer.index, whitespace_set ++ helper.dtd_terminal_characters) orelse src.len;
                tokenizer.index = str_end;
                if (str_start != str_end) {
                    return helper.rangeInit(str_start, str_end);
                }
                tokenizer.state = switch (tag) {
                    .dtd_invalid_start => .dtd,
                    .dtd_subtag_invalid_start => .dtd_subtag,
                    else => unreachable,
                };
                return null;
            },
        },

        .@"dtd,<!E",

        .@"dtd,<!EL",
        .@"dtd,<!ELE",
        .@"dtd,<!ELEM",
        .@"dtd,<!ELEME",
        .@"dtd,<!ELEMEN",

        .@"dtd,<!EN",
        .@"dtd,<!ENT",
        .@"dtd,<!ENTI",
        .@"dtd,<!ENTIT",

        .@"dtd,<!A",
        .@"dtd,<!AT",
        .@"dtd,<!ATT",
        .@"dtd,<!ATTL",
        .@"dtd,<!ATTLI",
        .@"dtd,<!ATTLIS",

        .@"dtd,<!N",
        .@"dtd,<!NO",
        .@"dtd,<!NOT",
        .@"dtd,<!NOTA",
        .@"dtd,<!NOTAT",
        .@"dtd,<!NOTATI",
        .@"dtd,<!NOTATIO",
        => |tag| switch (ret_kind) {
            .type => {
                if (tokenizer.index == src.len) break .invalid_token;
                const expected_char: u8, //
                const on_match: union(enum) { state: State, type: TokenType } //
                = switch (tag) {
                    .@"dtd,<!E" => {
                        if (tokenizer.index == src.len) break .invalid_decl;
                        switch (src[tokenizer.index]) {
                            'L' => {
                                tokenizer.state = .@"dtd,<!EL";
                                tokenizer.index += 1;
                                continue;
                            },
                            'N' => {
                                tokenizer.state = .@"dtd,<!EN";
                                tokenizer.index += 1;
                                continue;
                            },
                            else => break .invalid_decl,
                        }
                    },

                    .@"dtd,<!EL" => .{ 'E', .{ .state = .@"dtd,<!ELE" } },
                    .@"dtd,<!ELE" => .{ 'M', .{ .state = .@"dtd,<!ELEM" } },
                    .@"dtd,<!ELEM" => .{ 'E', .{ .state = .@"dtd,<!ELEME" } },
                    .@"dtd,<!ELEME" => .{ 'N', .{ .state = .@"dtd,<!ELEMEN" } },
                    .@"dtd,<!ELEMEN" => .{ 'T', .{ .type = .element_decl } },

                    .@"dtd,<!EN" => .{ 'T', .{ .state = .@"dtd,<!ENT" } },
                    .@"dtd,<!ENT" => .{ 'I', .{ .state = .@"dtd,<!ENTI" } },
                    .@"dtd,<!ENTI" => .{ 'T', .{ .state = .@"dtd,<!ENTIT" } },
                    .@"dtd,<!ENTIT" => .{ 'Y', .{ .type = .entity_decl } },

                    .@"dtd,<!A" => .{ 'T', .{ .state = .@"dtd,<!AT" } },
                    .@"dtd,<!AT" => .{ 'T', .{ .state = .@"dtd,<!ATT" } },
                    .@"dtd,<!ATT" => .{ 'L', .{ .state = .@"dtd,<!ATTL" } },
                    .@"dtd,<!ATTL" => .{ 'I', .{ .state = .@"dtd,<!ATTLI" } },
                    .@"dtd,<!ATTLI" => .{ 'S', .{ .state = .@"dtd,<!ATTLIS" } },
                    .@"dtd,<!ATTLIS" => .{ 'T', .{ .type = .attlist_decl } },

                    .@"dtd,<!N" => .{ 'O', .{ .state = .@"dtd,<!NO" } },
                    .@"dtd,<!NO" => .{ 'T', .{ .state = .@"dtd,<!NOT" } },
                    .@"dtd,<!NOT" => .{ 'A', .{ .state = .@"dtd,<!NOTA" } },
                    .@"dtd,<!NOTA" => .{ 'T', .{ .state = .@"dtd,<!NOTAT" } },
                    .@"dtd,<!NOTAT" => .{ 'I', .{ .state = .@"dtd,<!NOTATI" } },
                    .@"dtd,<!NOTATI" => .{ 'O', .{ .state = .@"dtd,<!NOTATIO" } },
                    .@"dtd,<!NOTATIO" => .{ 'N', .{ .type = .notation_decl } },

                    else => unreachable,
                };
                if (src[tokenizer.index] != expected_char) break .invalid_decl;
                switch (on_match) {
                    .state => |state_on_match| {
                        tokenizer.state = state_on_match;
                        tokenizer.index += 1;
                        continue;
                    },
                    .type => |type_on_match| {
                        tokenizer.state = .dtd_subtag;
                        tokenizer.index += 1;
                        break type_on_match;
                    },
                }
            },

            .src => {
                tokenizer.state = .dtd_subtag_invalid_start;
                return helper.literalInit(switch (tag) {
                    inline //
                    .@"dtd,<!E",

                    .@"dtd,<!EL",
                    .@"dtd,<!ELE",
                    .@"dtd,<!ELEM",
                    .@"dtd,<!ELEME",
                    .@"dtd,<!ELEMEN",

                    .@"dtd,<!EN",
                    .@"dtd,<!ENT",
                    .@"dtd,<!ENTI",
                    .@"dtd,<!ENTIT",

                    .@"dtd,<!A",
                    .@"dtd,<!AT",
                    .@"dtd,<!ATT",
                    .@"dtd,<!ATTL",
                    .@"dtd,<!ATTLI",
                    .@"dtd,<!ATTLIS",

                    .@"dtd,<!N",
                    .@"dtd,<!NO",
                    .@"dtd,<!NOT",
                    .@"dtd,<!NOTA",
                    .@"dtd,<!NOTAT",
                    .@"dtd,<!NOTATI",
                    .@"dtd,<!NOTATIO",
                    => |itag| @field(TokenSrc.Literal, @tagName(itag)["dtd,".len..]),
                    else => unreachable,
                });
            },
        },
    } else error.BufferUnderrun;
}

pub const TokenSrc = union(enum) {
    range: Range,
    literal: Literal,

    pub const Range = struct {
        start: usize,
        end: usize,

        /// For a streaming `Tokenizer`, this must be called with the `src` field
        /// immediately after it is returned from `nextSrcStream` to get the source string.
        /// For a non-streaming `Tokenizer`, this can be called at any time with
        /// the `src` field.
        /// The returned string aliases the `src` parameter.
        pub inline fn toStr(range: Range, src: []const u8) []const u8 {
            return src[range.start..range.end];
        }
    };

    pub const Literal = enum {
        @"]",
        @"]]",
        @"/",
        @"?",

        @"<!",
        @"<!-",

        @"<![",
        @"<![C",
        @"<![CD",
        @"<![CDA",
        @"<![CDAT",
        @"<![CDATA",

        @"<!D",
        @"<!DO",
        @"<!DOC",
        @"<!DOCT",
        @"<!DOCTY",
        @"<!DOCTYP",

        @"<!E",

        @"<!EN",
        @"<!ENT",
        @"<!ENTI",
        @"<!ENTIT",

        @"<!EL",
        @"<!ELE",
        @"<!ELEM",
        @"<!ELEME",
        @"<!ELEMEN",

        @"<!A",
        @"<!AT",
        @"<!ATT",
        @"<!ATTL",
        @"<!ATTLI",
        @"<!ATTLIS",

        @"<!N",
        @"<!NO",
        @"<!NOT",
        @"<!NOTA",
        @"<!NOTAT",
        @"<!NOTATI",
        @"<!NOTATIO",

        @"-",

        /// For a streaming `Tokenizer`, this is illegal.
        /// For a non-streaming `Tokenizer`, this must be called immediately after it is returned
        /// from `nextSrcStream` to get the source range, which may then be turned into a string which
        /// aliases the `src` field.
        pub inline fn toRange(literal_tok: Literal, tokenizer: *const Tokenizer) Range {
            const len = literal_tok.toStr().len;
            const result: Range = .{ .start = tokenizer.index - len, .end = tokenizer.index };
            assert(result.end - result.start == len);
            return result;
        }

        /// This is valid to call at any time, regardless of whether the source `Tokenizer` is
        /// streaming, as it will simply return the represented string literal.
        pub inline fn toStr(literal_tok: Literal) []const u8 {
            return @tagName(literal_tok);
        }
    };
};

const State = enum {
    eof,

    general,
    @"general,]",
    @"general,]]",
    @"general,&",

    @"general,<",

    @"general,<!",
    @"general,<!-",

    pi,
    @"pi,?",

    element_tag,
    element_tag_whitespace,
    element_tag_sq,
    element_tag_dq,
    @"element_tag_sq,&",
    @"element_tag_dq,&",

    @"general,<!--",
    @"general,<!--,-",
    @"general,<!--,--",
    @"general,<!--,---",

    @"<![",
    @"<![C",
    @"<![CD",
    @"<![CDA",
    @"<![CDAT",
    @"<![CDATA",
    cdata_start_invalid,
    cdata,
    @"cdata,]",
    @"cdata,]]",

    @"<!D",
    @"<!DO",
    @"<!DOC",
    @"<!DOCT",
    @"<!DOCTY",
    @"<!DOCTYP",

    dtd_invalid_start,
    dtd,
    dtd_sq,
    dtd_dq,
    dtd_token,
    dtd_whitespace,

    dtd_subtag_invalid_start,
    dtd_subtag,
    dtd_subtag_sq,
    dtd_subtag_dq,
    dtd_subtag_token,
    dtd_subtag_whitespace,

    @"dtd,<",

    dtd_pi,
    @"dtd_pi,?",

    @"dtd,<!",
    @"dtd,<!-",
    @"dtd,<!--",
    @"dtd_comment,-",
    @"dtd_comment,--",
    @"dtd_comment,---",

    @"dtd,<!E",

    @"dtd,<!EL",
    @"dtd,<!ELE",
    @"dtd,<!ELEM",
    @"dtd,<!ELEME",
    @"dtd,<!ELEMEN",

    @"dtd,<!EN",
    @"dtd,<!ENT",
    @"dtd,<!ENTI",
    @"dtd,<!ENTIT",

    @"dtd,<!A",
    @"dtd,<!AT",
    @"dtd,<!ATT",
    @"dtd,<!ATTL",
    @"dtd,<!ATTLI",
    @"dtd,<!ATTLIS",

    @"dtd,<!N",
    @"dtd,<!NO",
    @"dtd,<!NOT",
    @"dtd,<!NOTA",
    @"dtd,<!NOTAT",
    @"dtd,<!NOTATI",
    @"dtd,<!NOTATIO",
};

/// A checked version of the tokenizer, mainly used for testing and debugging.
/// Panics on API misuse instead of reacting with undefined behaviour.
/// Acts as a drop-in replacement to facilitate this use case.
pub const CheckedTokenizer = struct {
    raw: Tokenizer,
    is_streaming: bool,
    prev_output: PrevOutput,

    const PrevOutput = union(enum) {
        init,
        tok: TokenType,
        str: enum { non_null, null },
    };

    pub inline fn initComplete(src: []const u8) CheckedTokenizer {
        return .{
            .raw = Tokenizer.initComplete(src),
            .is_streaming = false,
            .prev_output = .init,
        };
    }

    pub inline fn initStreaming() CheckedTokenizer {
        return .{
            .raw = Tokenizer.initStreaming(),
            .is_streaming = true,
            .prev_output = .init,
        };
    }

    pub fn feedInput(checked: *CheckedTokenizer, src: []const u8) void {
        if (!checked.is_streaming) @panic("Can't feed input to a non-streaming tokenizer");
        checked.raw.feedInput(src);
    }

    pub fn feedEof(checked: *CheckedTokenizer) void {
        if (!checked.is_streaming) @panic("Can't feed input to a non-streaming tokenizer");
        checked.raw.feedEof();
    }

    pub fn nextType(checked: *CheckedTokenizer) NextTypeError!TokenType {
        switch (checked.prev_output) {
            .init => {},
            .tok => |prev| if (prev.hasString()) @panic(switch (prev) {
                inline else => |tag| "Can't call `nextType` when the previous token '." ++ @tagName(tag) ++
                    "' has a string pending - call `nextSrc` first.",
            }),
            .str => |str| switch (str) {
                .non_null => @panic("Can't call `nextType` when there is still a string pending - call `nextSrc` until it returns null."),
                .null => {},
            },
        }
        const tok_type = try checked.raw.nextTypeStream();
        checked.prev_output = .{ .tok = tok_type };
        return tok_type;
    }

    pub fn nextSrc(checked: *CheckedTokenizer) NextSrcError!?TokenSrc {
        switch (checked.prev_output) {
            .init => {},
            .tok => |prev| if (!prev.hasString()) @panic(switch (prev) {
                inline else => |tag| "Can't call `nextSrc` for token type '." ++ @tagName(tag) ++ "'. Call `nextType` instead.",
            }),
            .str => |str| switch (str) {
                .non_null => {},
                .null => @panic("Can't call `nextSrc` after one of them have already returned null. Call `nextType` instead."),
            },
        }
        const tok_src = try checked.raw.nextSrcStream();
        checked.prev_output = .{ .str = if (tok_src != null) .non_null else .null };
        return tok_src;
    }

    pub fn expectNextType(tokenizer: *CheckedTokenizer, expected: NextTypeError!TokenType) !void {
        comptime assert(builtin.is_test);
        const actual = tokenizer.nextType();
        try std.testing.expectEqual(expected, actual);
    }
    pub fn expectNextString(tokenizer: *CheckedTokenizer, expected: NextSrcError!?[]const u8) !void {
        comptime assert(builtin.is_test);
        const actual: NextSrcError!?[]const u8 = blk: {
            const maybe_tok_src = tokenizer.nextSrc() catch |err| break :blk err;
            const tok_src = maybe_tok_src orelse break :blk null;
            break :blk switch (tok_src) {
                .range => |range| range.toStr(tokenizer.raw.src),
                .literal => |literal| if (tokenizer.is_streaming)
                    literal.toStr()
                else
                    literal.toRange(&tokenizer.raw).toStr(tokenizer.raw.src),
            };
        };

        const maybe_expected = expected catch |expected_err| {
            const maybe_actual = actual catch |actual_err| {
                return try std.testing.expectEqual(expected_err, actual_err);
            };
            testingPrint("expected {[expected]}, found {[actual_quote]s}{[actual]?}{[actual_quote]s}\n", .{
                .expected = expected_err,
                .actual_quote = if (maybe_actual == null) "" else "'",
                .actual = if (maybe_actual) |actual_unwrapped| std.zig.fmtEscapes(actual_unwrapped) else null,
            });
            return error.TestExpectedEqual;
        };
        const maybe_actual = actual catch |actual_err| {
            testingPrint("expected {[expected_quote]s}{[expected]?}{[expected_quote]s}, found {[actual]}\n", .{
                .expected_quote = if (maybe_expected == null) "" else "'",
                .expected = if (maybe_expected) |expected_unwrapped| std.zig.fmtEscapes(expected_unwrapped) else null,
                .actual = actual_err,
            });
            return error.TestExpectedEqual;
        };

        const expected_unwrapped = maybe_expected orelse {
            if (maybe_actual) |actual_unwrapped| {
                testingPrint("expected null, found '{}'\n", .{std.zig.fmtEscapes(actual_unwrapped)});
                return error.TestExpectedEqual;
            }
            return;
        };
        const actual_unwrapped = maybe_actual orelse {
            testingPrint("expected '{}', found null\n", .{std.zig.fmtEscapes(expected_unwrapped)});
            return error.TestExpectedEqual;
        };

        try std.testing.expectEqualStrings(expected_unwrapped, actual_unwrapped);
    }
    pub fn expectNextStringSeq(tokenizer: *CheckedTokenizer, expected_list: []const NextSrcError!?[]const u8) !void {
        for (expected_list) |expected| try tokenizer.expectNextString(expected);
    }
    pub fn expectNextTypeStringSeq(tokenizer: *CheckedTokenizer, expected_tt: TokenType, expected_list: []const NextSrcError!?[]const u8) !void {
        try tokenizer.expectNextType(expected_tt);
        try std.testing.expect(expected_tt.hasString());
        try tokenizer.expectNextStringSeq(expected_list);
    }
};

fn testingPrint(comptime fmt_str: []const u8, args: anytype) void {
    if (@inComptime()) {
        @compileError(std.fmt.comptimePrint(fmt_str, args));
    } else if (std.testing.backend_can_print) {
        std.debug.print(fmt_str, args);
    }
}

test "Tokenizer Invalid Markup" {
    var tokenizer: CheckedTokenizer = undefined;

    tokenizer = CheckedTokenizer.initComplete("<!");
    try tokenizer.expectNextType(.invalid_angle_bracket_left_bang);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<!-");
    try tokenizer.expectNextType(.invalid_comment_start_single_dash);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<!DOCTYPE<!-");
    try tokenizer.expectNextType(.dtd_start);
    try tokenizer.expectNextType(.invalid_comment_start_single_dash);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<!-<");
    try tokenizer.expectNextType(.invalid_comment_start_single_dash);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "<", null });
    try tokenizer.expectNextType(.eof);

    for ([_]struct { TokenType, []const u8 }{
        .{ .invalid_cdata_start, "<![" },
        .{ .invalid_cdata_start, "<![C" },
        .{ .invalid_cdata_start, "<![CD" },
        .{ .invalid_cdata_start, "<![CDA" },
        .{ .invalid_cdata_start, "<![CDAT" },
        .{ .invalid_cdata_start, "<![CDATA" },

        .{ .invalid_dtd_start, "<!D" },
        .{ .invalid_dtd_start, "<!DO" },
        .{ .invalid_dtd_start, "<!DOC" },
        .{ .invalid_dtd_start, "<!DOCT" },
        .{ .invalid_dtd_start, "<!DOCTY" },
        .{ .invalid_dtd_start, "<!DOCTYP" },
    }) |values| {
        const expected_tt, const incomplete_mk_str = values;

        tokenizer = CheckedTokenizer.initComplete(incomplete_mk_str);
        try tokenizer.expectNextTypeStringSeq(expected_tt, &.{ incomplete_mk_str, null });
        try tokenizer.expectNextType(.eof);

        tokenizer = CheckedTokenizer.initStreaming();
        for (0..incomplete_mk_str.len) |i| {
            tokenizer.feedInput(incomplete_mk_str[i..][0..1]);
            try tokenizer.expectNextType(error.BufferUnderrun);
        }

        var eof_copy = tokenizer;
        eof_copy.feedEof();
        try eof_copy.expectNextTypeStringSeq(expected_tt, &.{ incomplete_mk_str, null });
        try eof_copy.expectNextType(.eof);

        tokenizer.feedInput("a");
        tokenizer.feedEof();
        try tokenizer.expectNextType(expected_tt);
        try tokenizer.expectNextString(incomplete_mk_str);
        try tokenizer.expectNextString("a");
        try tokenizer.expectNextString(null);
        try tokenizer.expectNextType(.eof);
    }

    tokenizer = CheckedTokenizer.initComplete("<! a <");
    try tokenizer.expectNextType(.invalid_angle_bracket_left_bang);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ " a ", null });
    try tokenizer.expectNextType(.angle_bracket_left);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<! a");
    try tokenizer.expectNextType(.invalid_angle_bracket_left_bang);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ " a", null });
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<![CDATAR a");
    try tokenizer.expectNextTypeStringSeq(.invalid_cdata_start, &.{ "<![CDATA", "R", null });
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ " a", null });
    try tokenizer.expectNextType(.eof);
}

test "Tokenizer Processing Instructions" {
    var tokenizer: CheckedTokenizer = undefined;

    tokenizer = CheckedTokenizer.initComplete("<??>");
    try tokenizer.expectNextType(.pi_start);
    try tokenizer.expectNextType(.pi_end);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<?");
    try tokenizer.expectNextType(.pi_start);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<? ?>");
    try tokenizer.expectNextType(.pi_start);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ " ", null });
    try tokenizer.expectNextType(.pi_end);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<?foo?>");
    try tokenizer.expectNextType(.pi_start);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "foo", null });
    try tokenizer.expectNextType(.pi_end);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<?foo ?>");
    try tokenizer.expectNextType(.pi_start);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "foo ", null });
    try tokenizer.expectNextType(.pi_end);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<?foo bar?>");
    try tokenizer.expectNextType(.pi_start);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "foo bar", null });
    try tokenizer.expectNextType(.pi_end);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<?" ++ "???" ++ "?>");
    try tokenizer.expectNextType(.pi_start);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "?", "?", "?", null });
    try tokenizer.expectNextType(.pi_end);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initStreaming();
    tokenizer.feedInput("<?foo");
    try tokenizer.expectNextType(.pi_start);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "foo", error.BufferUnderrun });
    tokenizer.feedInput("?>");
    try tokenizer.expectNextString(null);
    try tokenizer.expectNextType(.pi_end);
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedEof();
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initStreaming();
    tokenizer.feedInput("<?foo");
    try tokenizer.expectNextType(.pi_start);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "foo", error.BufferUnderrun });
    tokenizer.feedInput("?");
    try tokenizer.expectNextString(error.BufferUnderrun);
    tokenizer.feedInput(">");
    try tokenizer.expectNextString(null);
    try tokenizer.expectNextType(.pi_end);
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedEof();
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initStreaming();
    tokenizer.feedInput("<?");
    try tokenizer.expectNextType(.pi_start);
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedInput("fizz?>");
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "fizz", null });
    try tokenizer.expectNextType(.pi_end);
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedEof();
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initStreaming();
    tokenizer.feedInput("<?bar");
    try tokenizer.expectNextType(.pi_start);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "bar", error.BufferUnderrun });
    tokenizer.feedInput("?");
    try tokenizer.expectNextString(error.BufferUnderrun);
    tokenizer.feedInput("?");
    try tokenizer.expectNextStringSeq(&.{ "?", error.BufferUnderrun });
    tokenizer.feedInput("baz");
    try tokenizer.expectNextStringSeq(&.{ "?", "baz", error.BufferUnderrun });
    tokenizer.feedInput("?>");
    try tokenizer.expectNextString(null);
    try tokenizer.expectNextType(.pi_end);
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedEof();
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initStreaming();
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedInput("<");
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedInput("?");
    try tokenizer.expectNextType(.pi_start);
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedInput("fo");
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "fo", error.BufferUnderrun });
    tokenizer.feedInput("o?");
    try tokenizer.expectNextStringSeq(&.{ "o", error.BufferUnderrun });
    tokenizer.feedInput("bar");
    try tokenizer.expectNextStringSeq(&.{ "?", "bar", error.BufferUnderrun });
    tokenizer.feedInput(" ");
    try tokenizer.expectNextStringSeq(&.{ " ", error.BufferUnderrun });
    tokenizer.feedInput("?");
    try tokenizer.expectNextString(error.BufferUnderrun);
    tokenizer.feedInput(" ");
    try tokenizer.expectNextStringSeq(&.{ "?", " ", error.BufferUnderrun });
    tokenizer.feedInput("?");
    try tokenizer.expectNextString(error.BufferUnderrun);
    tokenizer.feedInput(">");
    try tokenizer.expectNextString(null);
    try tokenizer.expectNextType(.pi_end);
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedEof();
    try tokenizer.expectNextType(.eof);
}

test "Tokenizer Comments" {
    var tokenizer: CheckedTokenizer = undefined;

    tokenizer = CheckedTokenizer.initComplete("<!--" ++ "-->");
    try tokenizer.expectNextType(.comment_start);
    try tokenizer.expectNextType(.comment_end);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<!--" ++ "-" ++ "-->");
    try tokenizer.expectNextType(.comment_start);
    try tokenizer.expectNextType(.invalid_comment_end_triple_dash);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<!--" ++ "--" ++ "-->");
    try tokenizer.expectNextType(.comment_start);
    try tokenizer.expectNextType(.invalid_comment_dash_dash);
    try tokenizer.expectNextType(.comment_end);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<!--" ++ "--" ++ "-" ++ "-->");
    try tokenizer.expectNextType(.comment_start);
    try tokenizer.expectNextType(.invalid_comment_dash_dash);
    try tokenizer.expectNextType(.invalid_comment_end_triple_dash);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<!-- <foo bar> -->");
    try tokenizer.expectNextType(.comment_start);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ " <foo bar> ", null });
    try tokenizer.expectNextType(.comment_end);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initStreaming();
    tokenizer.feedInput("<!--");
    try tokenizer.expectNextType(.comment_start);
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedInput("--");
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedInput(">");
    try tokenizer.expectNextType(.comment_end);
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedEof();
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initStreaming();
    tokenizer.feedInput("<");
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedInput("!");
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedInput("-");
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedInput("-");
    try tokenizer.expectNextType(.comment_start);
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedInput("-");
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedInput("-");
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedInput(">");
    try tokenizer.expectNextType(.comment_end);
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedEof();
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initStreaming();
    tokenizer.feedInput("<!--");
    try tokenizer.expectNextType(.comment_start);
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedInput("--a");
    try tokenizer.expectNextType(.invalid_comment_dash_dash);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "a", error.BufferUnderrun });
    tokenizer.feedInput("-->");
    try tokenizer.expectNextString(null);
    try tokenizer.expectNextType(.comment_end);
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedEof();
    try tokenizer.expectNextType(.eof);
}

test "Tokenizer CDATA Sections" {
    var tokenizer: CheckedTokenizer = undefined;

    tokenizer = CheckedTokenizer.initComplete("<![CDATA[" ++ "]]>");
    try tokenizer.expectNextType(.cdata_start);
    try tokenizer.expectNextType(.cdata_end);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<![CDATA[" ++ " foo " ++ "]]>");
    try tokenizer.expectNextType(.cdata_start);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ " foo ", null });
    try tokenizer.expectNextType(.cdata_end);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<![CDATA[" ++ "]]" ++ "]]>");
    try tokenizer.expectNextType(.cdata_start);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "]", "]", null });
    try tokenizer.expectNextType(.cdata_end);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<![CDATA[" ++ "]" ++ "]]" ++ "]]>");
    try tokenizer.expectNextType(.cdata_start);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "]", "]", "]", null });
    try tokenizer.expectNextType(.cdata_end);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<![CDATA[" ++ "]>" ++ "]]>");
    try tokenizer.expectNextType(.cdata_start);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "]", ">", null });
    try tokenizer.expectNextType(.cdata_end);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initStreaming();
    for ("<![CDATA[") |c| {
        try tokenizer.expectNextType(error.BufferUnderrun);
        tokenizer.feedInput(&.{c});
    }
    try tokenizer.expectNextType(.cdata_start);
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedInput("foo");
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "foo", error.BufferUnderrun });
    try tokenizer.expectNextString(error.BufferUnderrun);
    tokenizer.feedInput("]]");
    try tokenizer.expectNextString(error.BufferUnderrun);
    tokenizer.feedInput("]");
    try tokenizer.expectNextString("]");
    try tokenizer.expectNextString(error.BufferUnderrun);
    tokenizer.feedInput("]]]");
    try tokenizer.expectNextStringSeq(&.{ "]", "]", "]", error.BufferUnderrun }); // from the first to `feedInput`s, not the latest one
    tokenizer.feedInput(">");
    try tokenizer.expectNextString(null);
    try tokenizer.expectNextType(.cdata_end);
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedEof();
    try tokenizer.expectNextType(.eof);
}

test "Tokenizer Character/Entity References" {
    var tokenizer: CheckedTokenizer = undefined;

    tokenizer = CheckedTokenizer.initComplete("&abc;");
    try tokenizer.expectNextType(.ampersand);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "abc", null });
    try tokenizer.expectNextType(.semicolon);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("&;");
    try tokenizer.expectNextType(.ampersand);
    try tokenizer.expectNextType(.semicolon);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("&");
    try tokenizer.expectNextType(.ampersand);
    try tokenizer.expectNextType(.invalid_reference_end);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("&foo");
    try tokenizer.expectNextType(.ampersand);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try tokenizer.expectNextType(.invalid_reference_end);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("&foo ");
    try tokenizer.expectNextType(.ampersand);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try tokenizer.expectNextType(.invalid_reference_end);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ " ", null });
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initStreaming();
    tokenizer.feedInput("&");
    try tokenizer.expectNextType(.ampersand);
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedInput(";");
    try tokenizer.expectNextType(.semicolon);
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedEof();
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initStreaming();
    tokenizer.feedInput("&");
    try tokenizer.expectNextType(.ampersand);
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedInput("foo");
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "foo", error.BufferUnderrun });
    tokenizer.feedInput("bar");
    try tokenizer.expectNextStringSeq(&.{ "bar", error.BufferUnderrun });
    tokenizer.feedEof();
    try tokenizer.expectNextString(null);
    try tokenizer.expectNextType(.invalid_reference_end);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initStreaming();
    tokenizer.feedInput("&");
    try tokenizer.expectNextType(.ampersand);
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedInput("foo");
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "foo", error.BufferUnderrun });
    tokenizer.feedInput("bar");
    try tokenizer.expectNextStringSeq(&.{ "bar", error.BufferUnderrun });
    tokenizer.feedInput(";");
    try tokenizer.expectNextString(null);
    try tokenizer.expectNextType(.semicolon);
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedEof();
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initStreaming();
    tokenizer.feedInput("&");
    try tokenizer.expectNextType(.ampersand);
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedInput("foo");
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "foo", error.BufferUnderrun });
    tokenizer.feedInput("bar");
    try tokenizer.expectNextStringSeq(&.{ "bar", error.BufferUnderrun });
    tokenizer.feedInput(" ");
    try tokenizer.expectNextString(null);
    try tokenizer.expectNextType(.invalid_reference_end);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ " ", error.BufferUnderrun });
    tokenizer.feedEof();
    try tokenizer.expectNextString(null);
    try tokenizer.expectNextType(.eof);
}

test "Tokenizer Text Data" {
    var tokenizer: CheckedTokenizer = undefined;

    tokenizer = CheckedTokenizer.initComplete("");
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initStreaming();
    tokenizer.feedEof();
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("foo bar");
    try tokenizer.expectNextType(.text_data);
    try tokenizer.expectNextString("foo bar");
    try tokenizer.expectNextString(null);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("foo bar");
    try tokenizer.expectNextType(.text_data);
    try tokenizer.expectNextString("foo bar");
    try tokenizer.expectNextString(null);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initStreaming();
    tokenizer.feedInput("foo");
    try tokenizer.expectNextType(.text_data);
    try tokenizer.expectNextString("foo");
    try tokenizer.expectNextString(error.BufferUnderrun);
    tokenizer.feedInput(" bar");
    try tokenizer.expectNextString(" bar");
    try tokenizer.expectNextString(error.BufferUnderrun);
    tokenizer.feedEof();
    try tokenizer.expectNextString(null);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("]]>");
    try tokenizer.expectNextType(.cdata_end);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initStreaming();
    tokenizer.feedInput("]]>");
    try tokenizer.expectNextType(.cdata_end);
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedInput("foo");
    try tokenizer.expectNextType(.text_data);
    try tokenizer.expectNextString("foo");
    try tokenizer.expectNextString(error.BufferUnderrun);
    tokenizer.feedEof();
    try tokenizer.expectNextString(null);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("foo]]> bar");
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "foo", null });
    try tokenizer.expectNextType(.cdata_end);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ " bar", null });
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initStreaming();
    tokenizer.feedInput("]");
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedEof();
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "]", null });
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initStreaming();
    tokenizer.feedInput("]]");
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedEof();
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "]]", null });
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initStreaming();
    tokenizer.feedInput("]]>");
    try tokenizer.expectNextType(.cdata_end);
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedEof();
    try tokenizer.expectNextType(.eof);
}

test "Tokenizer Element Closing Tags" {
    var tokenizer: CheckedTokenizer = undefined;

    tokenizer = CheckedTokenizer.initComplete("</foo>");
    try tokenizer.expectNextType(.angle_bracket_left);
    try tokenizer.expectNextType(.slash);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("</foo >");
    try tokenizer.expectNextType(.angle_bracket_left);
    try tokenizer.expectNextType(.slash);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("</ >");
    try tokenizer.expectNextType(.angle_bracket_left);
    try tokenizer.expectNextType(.slash);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("</>");
    try tokenizer.expectNextType(.angle_bracket_left);
    try tokenizer.expectNextType(.slash);
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.eof);
}

test "Tokenizer Element Opening Tags" {
    var tokenizer: CheckedTokenizer = undefined;

    tokenizer = CheckedTokenizer.initComplete("<");
    try tokenizer.expectNextType(.angle_bracket_left);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<foo  />");
    try tokenizer.expectNextType(.angle_bracket_left);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ "  ", null });
    try tokenizer.expectNextType(.slash);
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<foo  >");
    try tokenizer.expectNextType(.angle_bracket_left);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ "  ", null });
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<foo  / >");
    try tokenizer.expectNextType(.angle_bracket_left);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ "  ", null });
    try tokenizer.expectNextType(.slash);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<foo bar='fizz'>");
    try tokenizer.expectNextType(.angle_bracket_left);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "bar", null });
    try tokenizer.expectNextType(.equals);
    try tokenizer.expectNextType(.quote_single);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "fizz", null });
    try tokenizer.expectNextType(.quote_single);
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<foo bar = 'fizz' >");
    try tokenizer.expectNextType(.angle_bracket_left);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "bar", null });
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextType(.equals);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextType(.quote_single);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "fizz", null });
    try tokenizer.expectNextType(.quote_single);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<foo bar=\"fizz\">");
    try tokenizer.expectNextType(.angle_bracket_left);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "bar", null });
    try tokenizer.expectNextType(.equals);
    try tokenizer.expectNextType(.quote_double);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "fizz", null });
    try tokenizer.expectNextType(.quote_double);
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<foo bar='&baz;'>");
    try tokenizer.expectNextType(.angle_bracket_left);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "bar", null });
    try tokenizer.expectNextType(.equals);
    try tokenizer.expectNextType(.quote_single);
    try tokenizer.expectNextType(.ampersand);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "baz", null });
    try tokenizer.expectNextType(.semicolon);
    try tokenizer.expectNextType(.quote_single);
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<foo  bar='fizz&baz;buzz'>");
    try tokenizer.expectNextType(.angle_bracket_left);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ "  ", null });
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "bar", null });
    try tokenizer.expectNextType(.equals);
    try tokenizer.expectNextType(.quote_single);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "fizz", null });
    try tokenizer.expectNextType(.ampersand);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "baz", null });
    try tokenizer.expectNextType(.semicolon);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "buzz", null });
    try tokenizer.expectNextType(.quote_single);
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initStreaming();
    tokenizer.feedInput("<fo");
    try tokenizer.expectNextType(.angle_bracket_left);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "fo", error.BufferUnderrun });
    tokenizer.feedInput("o  ba");
    try tokenizer.expectNextStringSeq(&.{ "o", null });

    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ "  ", null });

    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "ba", error.BufferUnderrun });
    tokenizer.feedInput("r='fi");
    try tokenizer.expectNextStringSeq(&.{ "r", null });

    try tokenizer.expectNextType(.equals);

    try tokenizer.expectNextType(.quote_single);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "fi", error.BufferUnderrun });
    tokenizer.feedInput("zz&ba");
    try tokenizer.expectNextStringSeq(&.{ "zz", null });

    try tokenizer.expectNextType(.ampersand);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "ba", error.BufferUnderrun });
    try tokenizer.expectNextString(error.BufferUnderrun);
    tokenizer.feedInput("z;bu");
    try tokenizer.expectNextStringSeq(&.{ "z", null });
    try tokenizer.expectNextType(.semicolon);

    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "bu", error.BufferUnderrun });
    tokenizer.feedInput("zz'");
    try tokenizer.expectNextStringSeq(&.{ "zz", null });
    try tokenizer.expectNextType(.quote_single);

    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedInput(">");
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedEof();
    try tokenizer.expectNextType(.eof);
}

test "Tokenizer Document Type Definition" {
    var tokenizer: CheckedTokenizer = undefined;

    tokenizer = CheckedTokenizer.initComplete("<!DOCTYPE");
    try tokenizer.expectNextType(.dtd_start);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<!DOCTYPE>");
    try tokenizer.expectNextType(.dtd_start);
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<!DOCTYPEfoo");
    try tokenizer.expectNextType(.dtd_start);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initStreaming();
    tokenizer.feedInput("<!DOCTYPEfoo");
    try tokenizer.expectNextType(.dtd_start);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "foo", error.BufferUnderrun });
    tokenizer.feedEof();
    try tokenizer.expectNextString(null);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<!DOCTYPE foo SYSTEM 'bar");
    try tokenizer.expectNextType(.dtd_start);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "SYSTEM", null });
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextType(.quote_single);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "bar", null });
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<!DOCTYPE foo SYSTEM 'bar' >");
    try tokenizer.expectNextType(.dtd_start);

    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "SYSTEM", null });
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });

    try tokenizer.expectNextType(.quote_single);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "bar", null });
    try tokenizer.expectNextType(.quote_single);

    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<!DOCTYPE []>");
    try tokenizer.expectNextType(.dtd_start);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextType(.square_bracket_left);
    try tokenizer.expectNextType(.square_bracket_right);
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<!DOCTYPE [ asdf ]>");
    try tokenizer.expectNextType(.dtd_start);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextType(.square_bracket_left);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "asdf", null });
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextType(.square_bracket_right);
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<!DOCTYPE [ asdf%foo; ]>");
    try tokenizer.expectNextType(.dtd_start);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextType(.square_bracket_left);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "asdf", null });
    try tokenizer.expectNextType(.percent);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try tokenizer.expectNextType(.semicolon);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextType(.square_bracket_right);
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<!DOCTYPE foo PUBLIC 'bar' [\n\n] >");
    try tokenizer.expectNextType(.dtd_start);

    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "PUBLIC", null });
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });

    try tokenizer.expectNextType(.quote_single);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "bar", null });
    try tokenizer.expectNextType(.quote_single);

    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });

    try tokenizer.expectNextType(.square_bracket_left);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ "\n\n", null });
    try tokenizer.expectNextType(.square_bracket_right);

    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });

    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete(
        \\<!DOCTYPE [%foo;]>
    );
    try tokenizer.expectNextType(.dtd_start);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextType(.square_bracket_left);
    try tokenizer.expectNextType(.percent);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try tokenizer.expectNextType(.semicolon);
    try tokenizer.expectNextType(.square_bracket_right);
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete(
        \\<!DOCTYPE 'fizz buzz'>
    );
    try tokenizer.expectNextType(.dtd_start);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextType(.quote_single);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "fizz buzz", null });
    try tokenizer.expectNextType(.quote_single);
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<!DOCTYPE[<??>]>");
    try tokenizer.expectNextType(.dtd_start);
    try tokenizer.expectNextType(.square_bracket_left);
    try tokenizer.expectNextType(.pi_start);
    try tokenizer.expectNextType(.pi_end);
    try tokenizer.expectNextType(.square_bracket_right);
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<!DOCTYPE[<?fizz buzz?>]>");
    try tokenizer.expectNextType(.dtd_start);
    try tokenizer.expectNextType(.square_bracket_left);
    try tokenizer.expectNextType(.pi_start);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "fizz buzz", null });
    try tokenizer.expectNextType(.pi_end);
    try tokenizer.expectNextType(.square_bracket_right);
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<!DOCTYPE[<!--fizz buzz-->]>");
    try tokenizer.expectNextType(.dtd_start);
    try tokenizer.expectNextType(.square_bracket_left);
    try tokenizer.expectNextType(.comment_start);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "fizz buzz", null });
    try tokenizer.expectNextType(.comment_end);
    try tokenizer.expectNextType(.square_bracket_right);
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<!DOCTYPE[<!---->]>");
    try tokenizer.expectNextType(.dtd_start);
    try tokenizer.expectNextType(.square_bracket_left);
    try tokenizer.expectNextType(.comment_start);
    try tokenizer.expectNextType(.comment_end);
    try tokenizer.expectNextType(.square_bracket_right);
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete(
        \\<!DOCTYPE [
        \\  <!ELEMENT foo EMPTY>
        \\]>
    );
    try tokenizer.expectNextType(.dtd_start);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextType(.square_bracket_left);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ "\n  ", null });
    try tokenizer.expectNextType(.element_decl);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "EMPTY", null });
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ "\n", null });
    try tokenizer.expectNextType(.square_bracket_right);
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete(
        \\<!DOCTYPE[
        \\  <!ELEMENT br EMPTY>
        \\  <!ELEMENT p (#PCDATA|emph)* >
        \\  <!ELEMENT %name.para; %content.para; >
        \\]>
        \\
    );
    try tokenizer.expectNextType(.dtd_start);
    try tokenizer.expectNextType(.square_bracket_left);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ "\n  ", null });

    try tokenizer.expectNextType(.element_decl);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "br", null });
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "EMPTY", null });
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ "\n  ", null });

    try tokenizer.expectNextType(.element_decl);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "p", null });
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextType(.lparen);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "#PCDATA", null });
    try tokenizer.expectNextType(.pipe);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "emph", null });
    try tokenizer.expectNextType(.rparen);
    try tokenizer.expectNextType(.asterisk);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ "\n  ", null });

    try tokenizer.expectNextType(.element_decl);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextType(.percent);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "name.para", null });
    try tokenizer.expectNextType(.semicolon);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextType(.percent);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "content.para", null });
    try tokenizer.expectNextType(.semicolon);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ "\n", null });

    try tokenizer.expectNextType(.square_bracket_right);
    try tokenizer.expectNextType(.angle_bracket_right);

    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "\n", null });
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<!DOCTYPE[<!ENTITY% >]>");
    try tokenizer.expectNextType(.dtd_start);
    try tokenizer.expectNextType(.square_bracket_left);
    try tokenizer.expectNextType(.entity_decl);
    try tokenizer.expectNextType(.percent);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.square_bracket_right);
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<!DOCTYPE[<!ENTITY%>]>");
    try tokenizer.expectNextType(.dtd_start);
    try tokenizer.expectNextType(.square_bracket_left);
    try tokenizer.expectNextType(.entity_decl);
    try tokenizer.expectNextType(.percent);
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.square_bracket_right);
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<!DOCTYPE[<!ENTITY %>]>");
    try tokenizer.expectNextType(.dtd_start);
    try tokenizer.expectNextType(.square_bracket_left);
    try tokenizer.expectNextType(.entity_decl);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextType(.percent);
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.square_bracket_right);
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete(
        \\<!DOCTYPE[
        \\  <!ENTITY copyright "Copyright &#169;2005 Test Name.">
        \\  <!ENTITY % autoelems "year, make, model">
        \\]>
    );

    try tokenizer.expectNextType(.dtd_start);
    try tokenizer.expectNextType(.square_bracket_left);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ "\n  ", null });

    try tokenizer.expectNextType(.entity_decl);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "copyright", null });
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextType(.quote_double);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "Copyright &#169;2005 Test Name.", null });
    try tokenizer.expectNextType(.quote_double);
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ "\n  ", null });

    try tokenizer.expectNextType(.entity_decl);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextType(.percent);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "autoelems", null });
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextType(.quote_double);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "year, make, model", null });
    try tokenizer.expectNextType(.quote_double);
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ "\n", null });

    try tokenizer.expectNextType(.square_bracket_right);
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<!DOCTYPR a");
    try tokenizer.expectNextTypeStringSeq(.invalid_dtd_start, &.{ "<!DOCTYP", "R", null });
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "a", null });
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<!DOCTYPE <!> > ");
    try tokenizer.expectNextType(.dtd_start);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextType(.invalid_angle_bracket_left_bang);
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ " ", null });
    try tokenizer.expectNextType(.eof);

    tokenizer = CheckedTokenizer.initComplete("<!DOCTYPE<!EL>foo");
    try tokenizer.expectNextType(.dtd_start);
    try tokenizer.expectNextTypeStringSeq(.invalid_decl, &.{ "<!EL", null });
    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try tokenizer.expectNextType(.eof);

    inline for (.{
        .{ "<!", .invalid_angle_bracket_left_bang },
        .{ "<!ELEMENT", .element_decl },
        .{ "<!ENTITY", .entity_decl },
        .{ "<!ATTLIST", .attlist_decl },
        .{ "<!NOTATION", .notation_decl },
    }) |values| {
        const decl_src: []const u8, const expected_decl_tt: TokenType = values;

        tokenizer = CheckedTokenizer.initComplete("<!DOCTYPE" ++ decl_src ++ "]>foo");
        try tokenizer.expectNextType(.dtd_start);
        try tokenizer.expectNextType(expected_decl_tt);
        try tokenizer.expectNextType(.square_bracket_right);
        try tokenizer.expectNextType(.angle_bracket_right);
        try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "foo", null });
        try tokenizer.expectNextType(.eof);

        tokenizer = CheckedTokenizer.initComplete("<!DOCTYPE" ++ decl_src ++ ">]foo");
        try tokenizer.expectNextType(.dtd_start);
        try tokenizer.expectNextType(expected_decl_tt);
        try tokenizer.expectNextType(.angle_bracket_right);
        try tokenizer.expectNextType(.square_bracket_right);
        try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
        try tokenizer.expectNextType(.eof);

        tokenizer = CheckedTokenizer.initComplete("<!DOCTYPE" ++ decl_src ++ "<!>]foo");
        try tokenizer.expectNextType(.dtd_start);
        try tokenizer.expectNextType(expected_decl_tt);
        try tokenizer.expectNextType(.invalid_angle_bracket_left_bang);
        try tokenizer.expectNextType(.angle_bracket_right);
        try tokenizer.expectNextType(.square_bracket_right);
        try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
        try tokenizer.expectNextType(.eof);

        tokenizer = CheckedTokenizer.initComplete("<!DOCTYPE" ++ decl_src);
        try tokenizer.expectNextType(.dtd_start);
        try tokenizer.expectNextType(expected_decl_tt);
        try tokenizer.expectNextType(.eof);
    }
}

test Tokenizer {
    var feeder = std.mem.window(u8,
        \\<?xml version="1.0" encoding="UTF-8"?>
        \\
        \\<foo>
        \\  Lorem ipsum
        \\  <bar fizz='buzz'><baz/></bar>
        \\</foo>
        \\
    , 2, 2);

    var tokenizer = CheckedTokenizer.initStreaming();
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedInput(feeder.next().?);
    try tokenizer.expectNextType(.pi_start);
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedInput(feeder.next().?);
    try tokenizer.expectNextType(.text_data);
    for ([_][]const u8{
        "xm", "l ", "ve", "rs", "io", "n=", "\"1", ".0", "\" ", "en", "co", "di", "ng", "=\"", "UT", "F-", "8\"",
    }) |expected| {
        try tokenizer.expectNextStringSeq(&.{ expected, error.BufferUnderrun });
        tokenizer.feedInput(feeder.next().?);
    }
    try tokenizer.expectNextString(null);
    try tokenizer.expectNextType(.pi_end);

    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedInput(feeder.next().?);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "\n\n", error.BufferUnderrun });
    tokenizer.feedInput(feeder.next().?);
    try tokenizer.expectNextString(null);

    try tokenizer.expectNextType(.angle_bracket_left);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "f", error.BufferUnderrun });
    tokenizer.feedInput(feeder.next().?);
    try tokenizer.expectNextStringSeq(&.{ "oo", error.BufferUnderrun });
    tokenizer.feedInput(feeder.next().?);
    try tokenizer.expectNextString(null);
    try tokenizer.expectNextType(.angle_bracket_right);

    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "\n", error.BufferUnderrun });

    for (comptime &[_]*const [2:0]u8{
        "  ", "Lo", "re", "m ", "ip", "su", "m\n", "  ",
    }) |expected| {
        tokenizer.feedInput(feeder.next().?);
        try tokenizer.expectNextString(expected);
    }
    try tokenizer.expectNextString(error.BufferUnderrun);
    tokenizer.feedInput(feeder.next().?);
    try tokenizer.expectNextString(null);

    try tokenizer.expectNextType(.angle_bracket_left);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "b", error.BufferUnderrun });
    tokenizer.feedInput(feeder.next().?);
    try tokenizer.expectNextStringSeq(&.{ "ar", error.BufferUnderrun });
    tokenizer.feedInput(feeder.next().?);
    try tokenizer.expectNextString(null);

    try tokenizer.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "f", error.BufferUnderrun });
    tokenizer.feedInput(feeder.next().?);
    try tokenizer.expectNextStringSeq(&.{ "iz", error.BufferUnderrun });
    tokenizer.feedInput(feeder.next().?);
    try tokenizer.expectNextStringSeq(&.{ "z", null });

    try tokenizer.expectNextType(.equals);

    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedInput(feeder.next().?);
    try tokenizer.expectNextType(.quote_single);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "b", error.BufferUnderrun });
    tokenizer.feedInput(feeder.next().?);
    try tokenizer.expectNextStringSeq(&.{ "uz", error.BufferUnderrun });
    tokenizer.feedInput(feeder.next().?);
    try tokenizer.expectNextStringSeq(&.{ "z", null });
    try tokenizer.expectNextType(.quote_single);
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedInput(feeder.next().?);
    try tokenizer.expectNextType(.angle_bracket_right);

    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedInput(feeder.next().?);
    try tokenizer.expectNextType(.angle_bracket_left);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "ba", error.BufferUnderrun });
    tokenizer.feedInput(feeder.next().?);
    try tokenizer.expectNextStringSeq(&.{ "z", null });
    try tokenizer.expectNextType(.slash);
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedInput(feeder.next().?);
    try tokenizer.expectNextType(.angle_bracket_right);

    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedInput(feeder.next().?);
    try tokenizer.expectNextType(.angle_bracket_left);
    try tokenizer.expectNextType(.slash);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "b", error.BufferUnderrun });
    tokenizer.feedInput(feeder.next().?);
    try tokenizer.expectNextStringSeq(&.{ "ar", error.BufferUnderrun });
    tokenizer.feedInput(feeder.next().?);
    try tokenizer.expectNextString(null);

    try tokenizer.expectNextType(.angle_bracket_right);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "\n", error.BufferUnderrun });
    tokenizer.feedInput(feeder.next().?);
    try tokenizer.expectNextString(null);

    try tokenizer.expectNextType(.angle_bracket_left);
    try tokenizer.expectNextType(.slash);
    try tokenizer.expectNextType(error.BufferUnderrun);
    tokenizer.feedInput(feeder.next().?);
    try tokenizer.expectNextTypeStringSeq(.tag_token, &.{ "fo", error.BufferUnderrun });
    tokenizer.feedInput(feeder.next().?);
    try tokenizer.expectNextStringSeq(&.{ "o", null });
    try tokenizer.expectNextType(.angle_bracket_right);
    tokenizer.feedInput(feeder.next().?);
    try tokenizer.expectNextTypeStringSeq(.text_data, &.{ "\n", error.BufferUnderrun });
    tokenizer.feedEof();
    try tokenizer.expectNextString(null);
    try tokenizer.expectNextType(.eof);
}
