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

debug: Debug,

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
        .state = .blank,
        .eof_specified = false,

        .debug = .{},
    };
}

pub fn feedInput(tokenizer: *Tokenizer, src: []const u8) void {
    assert(!tokenizer.eof_specified);
    assert(tokenizer.index == tokenizer.src.len);
    tokenizer.src = src;
    tokenizer.index = 0;
}

pub fn feedEof(tokenizer: *Tokenizer) void {
    assert(!tokenizer.eof_specified);
    assert(tokenizer.index == 0 or tokenizer.index == tokenizer.src.len);
    tokenizer.eof_specified = true;
}

pub const BufferError = error{BufferUnderrun};

pub const Context = enum {
    /// * `.eof`
    /// * `.angle_bracket_left`
    /// * `.text_data`
    /// * `.ampersand`
    /// * `.pi_start`
    /// * `.comment_start`
    /// * `.invalid_comment_start_single_dash`
    /// * `.cdata_start`
    /// * `.invalid_cdata_start`
    /// * `.cdata_end`
    /// * `.dtd_start`
    /// * `.invalid_dtd_start`
    /// * `.invalid_angle_bracket_left_bang`
    non_markup,

    /// * `.eof`
    /// * `.lparen`
    /// * `.rparen`
    /// * `.qmark`
    /// * `.asterisk`
    /// * `.plus`
    /// * `.pipe`
    /// * `.comma`
    /// * `.hashtag`
    /// * `.percent`
    /// * `.quote_single`
    /// * `.quote_double`
    /// * `.square_bracket_left`
    /// * `.square_bracket_right`
    /// * `.angle_bracket_left`
    /// * `.angle_bracket_right`
    /// * `.pi_start`
    /// * `.invalid_angle_bracket_left_bang`
    /// * `.dtd_decl`
    /// * `.invalid_comment_start_single_dash`
    /// * `.comment_start`
    /// * `.tag_whitespace`
    /// * `.tag_token`
    dtd,

    /// Possible token types are:
    /// * `.eof`
    /// * `.slash`
    /// * `.equals`
    /// * `.quote_single`
    /// * `.quote_double`
    /// * `.angle_bracket_right`
    /// * `.tag_whitespace`
    /// * `.tag_token`
    element_tag,

    /// Possible token types are:
    /// * `.eof`
    /// * `.text_data`
    /// * `.invalid_comment_start_single_dash`
    /// * `.invalid_comment_dash_dash`
    /// * `.invalid_comment_end_triple_dash`
    /// * `.comment_end`
    comment,

    /// Possible token types are:
    /// * `.eof`
    /// * `.text_data`
    /// * `.pi_end`
    pi,

    /// Possible token types are:
    /// * `.eof`
    /// * `.text_data`
    /// * `.cdata_end`
    cdata,

    /// Should be used for both system literals and pubid literals, single quoted.
    /// Possible token types are:
    /// * `.eof`
    /// * `.quote_single`
    /// * `.text_data`
    system_literal_quote_single,
    /// Should be used for both system literals and pubid literals, single quoted.
    /// Possible token types are:
    /// * `.eof`
    /// * `.quote_double`
    /// * `.text_data`
    system_literal_quote_double,

    /// Possible token types are:
    /// * `.eof`
    /// * `.quote_single`
    /// * `.ampersand`
    /// * `.angle_bracket_left`
    /// * `.text_data`
    attribute_value_quote_single,
    /// Possible token types are:
    /// * `.eof`
    /// * `.quote_double`
    /// * `.ampersand`
    /// * `.angle_bracket_left`
    /// * `.text_data`
    attribute_value_quote_double,

    /// Possible token types are:
    /// * `.eof`
    /// * `.quote_single`
    /// * `.ampersand`
    /// * `.percent`
    /// * `.text_data`
    entity_value_quote_single,
    /// Possible token types are:
    /// * `.eof`
    /// * `.quote_double`
    /// * `.ampersand`
    /// * `.percent`
    /// * `.text_data`
    entity_value_quote_double,

    /// Possible token types are:
    /// * `.tag_token`
    /// * `.invalid_reference_end`
    /// * `.semicolon`
    reference,
};

pub fn nextTypeStream(tokenizer: *Tokenizer, context: Context) BufferError!TokenType {
    return switch (context) {
        inline else => |ictx| tokenizer.nextTypeOrSrcImpl(ictx, .type),
    };
}

pub fn nextTypeNoUnderrun(tokenizer: *Tokenizer, context: Context) TokenType {
    return switch (context) {
        inline else => |ictx| tokenizer.nextTypeOrSrcImpl(ictx, .type_no_underrun),
    };
}

pub fn nextSrcStream(tokenizer: *Tokenizer, context: Context) BufferError!?[]const u8 {
    const maybe_tok_src: ?TokenSrc = switch (context) {
        inline else => |ictx| try tokenizer.nextTypeOrSrcImpl(ictx, .src),
    };
    const tok_src = maybe_tok_src orelse return null;
    return switch (tok_src) {
        .range => |range| range.toStr(tokenizer.src),
        .literal => |literal| literal.toStr(),
    };
}

pub fn nextSrcNoUnderrun(tokenizer: *Tokenizer, context: Context) Range {
    switch (context) {
        inline else => |ictx| {
            var full_range: Range = switch (tokenizer.nextTypeOrSrcImpl(ictx, .src_no_underrun).?) {
                .range => |range| range,
                .literal => |literal| literal.toRange(tokenizer),
            };
            while (tokenizer.nextTypeOrSrcImpl(ictx, .src_no_underrun)) |tok_src| {
                const range: Range = switch (tok_src) {
                    .range => |range| range,
                    .literal => |literal| literal.toRange(tokenizer),
                };
                assert(full_range.end == range.start);
                full_range.end = range.end;
            }
            return full_range;
        },
    }
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
    /// The '#' token.
    hashtag,
    /// The '?' token.
    qmark,
    /// The '*' token.
    asterisk,
    /// The '+' token.
    plus,

    /// The '/' token.
    slash,

    /// The '%' token.
    percent,

    /// The "'" token.
    quote_single,
    /// The '"' token.
    quote_double,

    /// The '<' token.
    angle_bracket_left,
    /// The '>' token.
    angle_bracket_right,

    /// The '[' token.
    square_bracket_left,
    /// The ']' token.
    square_bracket_right,

    /// The '&' token.
    ampersand,
    /// The ';' token.
    semicolon,
    /// Encountered a terminating character (or EOF) other than the ';' token after the ampersand.
    /// This token simply represents the absence of the ';' token.
    /// Ends the token sequence.
    invalid_reference_end,

    /// The '<?' token.
    pi_start,
    /// The '?>' token, terminating the PI tag.
    pi_end,

    /// The '<![CDATA[' token.
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
    comment_start,
    /// The '<!-' token.
    invalid_comment_start_single_dash,
    /// The invalid token '--'.
    invalid_comment_dash_dash,
    /// The invalid token '--->'.
    invalid_comment_end_triple_dash,
    /// Indicates '-->' after a comment.
    comment_end,

    /// The '<!DOCTYPE' token.
    dtd_start,
    /// A token which partially matches the '<!DOCTYPE' token.
    ///
    /// The source for the matched token is returned.
    invalid_dtd_start,
    /// The '<!' token followed by some tag token, such as 'ENTITY',
    /// 'ATTLIST', 'ELEMENT', 'NOTATION', or an invalid variant.
    ///
    /// The source for the matched token is returned.
    dtd_decl,

    /// The '<!' token.
    ///
    /// This is returned when '<!' is followed by a sequence
    /// which does not ultimately form a recognized markup tag.
    invalid_angle_bracket_left_bang,

    /// Whether or not the token represents any text to be returned by `nextSrc*`.
    pub inline fn hasSrc(token_type: TokenType) bool {
        return switch (token_type) {
            .text_data,
            .tag_whitespace,
            .tag_token,

            .invalid_cdata_start,
            .invalid_dtd_start,
            .dtd_decl,
            => true,

            else => false,
        };
    }

    pub inline fn stringLiteral(token_type: TokenType) ?[]const u8 {
        return switch (token_type) {
            .text_data => null,
            .tag_whitespace => null,
            .tag_token => null,

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

            .dtd_decl => null,
        };
    }
};

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
    @"<!DOCTYPE",

    @"-",

    /// For a streaming `Tokenizer`, this is illegal.
    /// For a non-streaming `Tokenizer`, this must be called immediately after it is returned
    /// from `nextSrcStream` to get the source range, which may then be turned into a string which
    /// aliases the `src` field.
    inline fn toRange(literal_tok: Literal, tokenizer: *const Tokenizer) Range {
        const len = literal_tok.toStr().len;
        const result: Range = .{ .start = tokenizer.index - len, .end = tokenizer.index };
        assert(result.end - result.start == len);
        return result;
    }

    /// This is valid to call at any time, regardless of whether the source `Tokenizer` is
    /// streaming, as it will simply return the represented string literal.
    inline fn toStr(literal_tok: Literal) []const u8 {
        return @tagName(literal_tok);
    }
};

const TokenSrc = union(enum) {
    range: Range,
    literal: Literal,
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
    comptime context: Context,
    comptime ret_type: next_helper.ReturnType,
) ret_type.Type() {
    const ret_kind = comptime ret_type.kind();
    const assert_eof_specified: bool = comptime switch (ret_type) {
        .type, .src => false,
        .type_no_underrun, .src_no_underrun => true,
    };

    if (std.debug.runtime_safety) switch (ret_kind) {
        .type => {
            tokenizer.debug.expected_context = context;
            assert(!tokenizer.debug.src_queued_up);
        },
        .src => {
            assert(context == tokenizer.debug.expected_context.?);
            assert(tokenizer.debug.src_queued_up);
        },
    };

    const src = tokenizer.src;
    assert(!assert_eof_specified or tokenizer.eof_specified);
    const eof_specified = assert_eof_specified or tokenizer.eof_specified;

    const eof_result = switch (ret_kind) {
        .type => .eof,
        .src => null,
    };

    const result_or_err: ret_type.Type() = while (tokenizer.index != src.len or eof_specified) break switch (context) {
        .non_markup => switch (tokenizer.state) {
            .eof => break eof_result,

            .blank => switch (ret_kind) {
                .type => if (tokenizer.index == src.len)
                    break .eof
                else switch (src[tokenizer.index]) {
                    ']' => {
                        tokenizer.state = .@"]";
                        tokenizer.index += 1;
                        continue;
                    },
                    '&' => {
                        tokenizer.index += 1;
                        break .ampersand;
                    },
                    '<' => {
                        tokenizer.state = .@"<";
                        tokenizer.index += 1;
                        continue;
                    },
                    else => break .text_data,
                },
                .src => {
                    if (tokenizer.index == src.len) break null;
                    const str_start = tokenizer.index;
                    const str_end = std.mem.indexOfAnyPos(u8, src, tokenizer.index, &[_]u8{ ']', '&', '<' }) orelse src.len;
                    tokenizer.index = str_end;
                    if (str_start != str_end) break next_helper.rangeInit(str_start, str_end);
                    if (src[tokenizer.index] != ']') break null;
                    tokenizer.state = .@"]";
                    tokenizer.index += 1;
                    continue;
                },
            },

            inline .@"]", .@"]]", .@"]]]" => |state| switch (next_helper.handlePossibleCdataEnd(tokenizer, state, ret_type)) {
                .@"continue" => continue,
                .@"return" => |result| break result,
            },

            .@"<" => switch (ret_kind) {
                .type => {
                    if (tokenizer.index == src.len) {
                        tokenizer.state = .eof;
                        break .angle_bracket_left;
                    }
                    switch (src[tokenizer.index]) {
                        '!' => {
                            tokenizer.state = .@"<!";
                            tokenizer.index += 1;
                            continue;
                        },
                        '?' => {
                            tokenizer.state = .blank;
                            tokenizer.index += 1;
                            break .pi_start;
                        },
                        else => {
                            tokenizer.state = .blank;
                            break .angle_bracket_left;
                        },
                    }
                },
                .src => unreachable,
            },

            .@"<!" => switch (ret_kind) {
                .type => {
                    if (tokenizer.index == src.len) {
                        tokenizer.state = .eof;
                        break .invalid_angle_bracket_left_bang;
                    }
                    tokenizer.state = switch (src[tokenizer.index]) {
                        '-' => .@"<!-",
                        '[' => .@"<![",
                        'D' => .@"<!D",
                        else => {
                            tokenizer.state = .blank;
                            break .invalid_angle_bracket_left_bang;
                        },
                    };
                    tokenizer.index += 1;
                    continue;
                },
                .src => unreachable,
            },

            .@"<!-" => switch (ret_kind) {
                .type => {
                    if (tokenizer.index == src.len) {
                        tokenizer.state = .eof;
                        break .invalid_comment_start_single_dash;
                    }
                    tokenizer.state = .blank;
                    if (src[tokenizer.index] != '-') {
                        break .invalid_comment_start_single_dash;
                    }
                    tokenizer.index += 1;
                    break .comment_start;
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
                        .@"<![CDATA" => .{ '[', .blank },
                        else => unreachable,
                    };
                    if (tokenizer.index == src.len) break .invalid_cdata_start;
                    if (src[tokenizer.index] != expected_char) break .invalid_cdata_start;
                    tokenizer.state = state_on_match;
                    tokenizer.index += 1;
                    if (state_on_match != .blank) continue;
                    break .cdata_start;
                },
                .src => {
                    tokenizer.state = .angle_bracket_left_bang_invalid_tag_returned;
                    break next_helper.literalInit(switch (tag) {
                        inline //
                        .@"<![",
                        .@"<![C",
                        .@"<![CD",
                        .@"<![CDA",
                        .@"<![CDAT",
                        .@"<![CDATA",
                        => |itag| @field(Literal, @tagName(itag)),
                        else => unreachable,
                    });
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
                        .@"<!DOCTYP" => .{ 'E', .@"<!DOCTYPE" },
                        else => unreachable,
                    };
                    if (src[tokenizer.index] != expected_char) {
                        break .invalid_dtd_start;
                    }
                    tokenizer.state = state_on_match;
                    tokenizer.index += 1;
                    continue;
                },
                .src => {
                    tokenizer.state = .angle_bracket_left_bang_invalid_tag_returned;
                    break next_helper.literalInit(switch (tag) {
                        inline //
                        .@"<!D",
                        .@"<!DO",
                        .@"<!DOC",
                        .@"<!DOCT",
                        .@"<!DOCTY",
                        .@"<!DOCTYP",
                        => |itag| @field(Literal, @tagName(itag)),
                        else => unreachable,
                    });
                },
            },
            .@"<!DOCTYPE" => switch (ret_kind) {
                .type => {
                    if (tokenizer.index == src.len) {
                        tokenizer.state = .eof;
                        break .dtd_start;
                    }
                    if (std.mem.indexOfScalar(u8, whitespace_set ++ next_helper.dtd_terminal_characters, src[tokenizer.index]) != null) {
                        tokenizer.state = .blank;
                        break .dtd_start;
                    }
                    break .invalid_dtd_start;
                },
                .src => {
                    tokenizer.state = .angle_bracket_left_bang_invalid_tag_returned;
                    break next_helper.literalInit(.@"<!DOCTYPE");
                },
            },

            .angle_bracket_left_bang_invalid_tag_returned => switch (ret_kind) {
                .type => unreachable,
                .src => {
                    if (tokenizer.index == src.len) {
                        tokenizer.state = .eof;
                        break null;
                    }
                    const str_start = tokenizer.index;
                    const str_end = std.mem.indexOfAnyPos(u8, src, tokenizer.index, whitespace_set ++ next_helper.dtd_terminal_characters) orelse src.len;
                    tokenizer.index = str_end;
                    if (str_start != str_end) {
                        break next_helper.rangeInit(str_start, str_end);
                    }
                    tokenizer.state = .blank;
                    break null;
                },
            },

            else => unreachable,
        },

        .dtd => switch (tokenizer.state) {
            .eof => break eof_result,

            .blank => switch (ret_kind) {
                .type => {
                    if (tokenizer.index == src.len) {
                        tokenizer.state = .eof;
                        break .eof;
                    }
                    switch (src[tokenizer.index]) {
                        inline '(', ')', '?', '*', '+', '|', ',', '#', '%', '\'', '\"', '[', ']', '>' => |char| {
                            tokenizer.index += 1;
                            break comptime switch (char) {
                                '(' => .lparen,
                                ')' => .rparen,
                                '?' => .qmark,
                                '*' => .asterisk,
                                '+' => .plus,
                                '|' => .pipe,
                                ',' => .comma,
                                '#' => .hashtag,
                                '%' => .percent,
                                '\'' => .quote_single,
                                '\"' => .quote_double,
                                '[' => .square_bracket_left,
                                ']' => .square_bracket_right,
                                '>' => .angle_bracket_right,
                                else => unreachable,
                            };
                        },
                        '<' => {
                            tokenizer.state = .@"<";
                            tokenizer.index += 1;
                            continue;
                        },
                        else => |char| {
                            const not_whitespace = std.mem.indexOfScalar(u8, whitespace_set, char) == null;
                            if (not_whitespace) break .tag_token;
                            tokenizer.state = .whitespace;
                            break .tag_whitespace;
                        },
                    }
                },
                .src => {
                    if (tokenizer.index == src.len) {
                        tokenizer.state = .eof;
                        break null;
                    }
                    const str_start = tokenizer.index;
                    const str_end = std.mem.indexOfAnyPos(u8, src, tokenizer.index, whitespace_set ++ next_helper.dtd_terminal_characters) orelse src.len;
                    tokenizer.index = str_end;
                    if (str_start == str_end) break null;
                    break next_helper.rangeInit(str_start, str_end);
                },
            },

            .whitespace => switch (ret_kind) {
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
                        break next_helper.rangeInit(str_start, str_end);
                    }
                    tokenizer.state = .blank;
                    break null;
                },
            },

            .@"<" => switch (ret_kind) {
                .type => {
                    if (tokenizer.index == src.len) {
                        tokenizer.state = .eof;
                        break .angle_bracket_left;
                    }
                    switch (src[tokenizer.index]) {
                        '?' => {
                            tokenizer.state = .blank;
                            tokenizer.index += 1;
                            break .pi_start;
                        },
                        '!' => {
                            tokenizer.state = .@"<!";
                            tokenizer.index += 1;
                            continue;
                        },
                        else => {
                            tokenizer.state = .blank;
                            break .angle_bracket_left;
                        },
                    }
                },
                .src => unreachable,
            },

            .@"<!" => switch (ret_kind) {
                .type => {
                    if (tokenizer.index == src.len) {
                        tokenizer.state = .eof;
                        break .invalid_angle_bracket_left_bang;
                    }
                    if (src[tokenizer.index] == '-') {
                        tokenizer.state = .@"<!-";
                        tokenizer.index += 1;
                        continue;
                    }
                    if (std.mem.indexOfScalar(u8, whitespace_set ++ next_helper.dtd_terminal_characters, src[tokenizer.index]) != null) {
                        tokenizer.state = .blank;
                        break .invalid_angle_bracket_left_bang;
                    }
                    tokenizer.state = .dtd_subtag_start;
                    break .dtd_decl;
                },
                .src => unreachable,
            },

            .@"<!-" => switch (ret_kind) {
                .type => {
                    if (tokenizer.index == src.len) {
                        tokenizer.state = .eof;
                        break .invalid_comment_start_single_dash;
                    }
                    tokenizer.state = .blank;
                    if (src[tokenizer.index] != '-') {
                        break .invalid_comment_start_single_dash;
                    }
                    tokenizer.index += 1;
                    break .comment_start;
                },
                .src => unreachable,
            },

            .dtd_subtag_start => switch (ret_kind) {
                .type => unreachable,
                .src => {
                    tokenizer.state = .blank; // fall through to the code for tokenizing tag_token
                    break next_helper.literalInit(.@"<!");
                },
            },

            else => unreachable,
        },

        .element_tag => switch (tokenizer.state) {
            .eof => break eof_result,

            .blank => switch (ret_kind) {
                .type => {
                    if (tokenizer.index == src.len) {
                        tokenizer.state = .eof;
                        break .eof;
                    }
                    switch (src[tokenizer.index]) {
                        inline '\'', '\"', '/', '=', '>' => |char| {
                            tokenizer.index += 1;
                            break comptime switch (char) {
                                '\'' => .quote_single,
                                '\"' => .quote_double,
                                '/' => .slash,
                                '=' => .equals,
                                '>' => .angle_bracket_right,
                                else => unreachable,
                            };
                        },
                        else => |char| {
                            const not_whitespace = std.mem.indexOfScalar(u8, whitespace_set, char) == null;
                            if (not_whitespace) break .tag_token;
                            tokenizer.state = .whitespace;
                            break .tag_whitespace;
                        },
                    }
                },
                .src => {
                    if (tokenizer.index == src.len) break null;
                    const str_start = tokenizer.index;
                    const str_end = std.mem.indexOfAnyPos(u8, src, tokenizer.index, whitespace_set ++ &[_]u8{ '\'', '\"', '/', '=', '>' }) orelse src.len;
                    tokenizer.index = str_end;
                    if (str_start == str_end) break null;
                    break next_helper.rangeInit(str_start, str_end);
                },
            },

            .whitespace => switch (ret_kind) {
                .type => unreachable,
                .src => {
                    if (tokenizer.index == src.len) break null;
                    const str_start = tokenizer.index;
                    const str_end = std.mem.indexOfNonePos(u8, src, tokenizer.index, whitespace_set) orelse src.len;
                    tokenizer.index = str_end;
                    if (str_start != str_end) {
                        break next_helper.rangeInit(str_start, str_end);
                    }
                    tokenizer.state = .blank;
                    break null;
                },
            },

            else => unreachable,
        },

        .comment => switch (tokenizer.state) {
            .eof => break eof_result,

            .blank => switch (ret_kind) {
                .type => {
                    if (tokenizer.index == src.len) {
                        tokenizer.state = .eof;
                        break .eof;
                    }
                    if (src[tokenizer.index] != '-') {
                        break .text_data;
                    }
                    tokenizer.state = .@"-";
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
                        break next_helper.rangeInit(str_start, str_end);
                    }
                    if (src[tokenizer.index] == '-') {
                        tokenizer.state = .@"-";
                        tokenizer.index += 1;
                        continue;
                    }
                    break null;
                },
            },

            .@"-" => switch (ret_kind) {
                .type => {
                    if (tokenizer.index == src.len) break .text_data;
                    if (src[tokenizer.index] != '-') break .text_data;
                    tokenizer.state = .@"--";
                    tokenizer.index += 1;
                    continue;
                },
                .src => {
                    if (tokenizer.index == src.len) {
                        tokenizer.state = .eof;
                        break next_helper.literalInit(.@"-");
                    }
                    if (src[tokenizer.index] != '-') {
                        tokenizer.state = .blank;
                        break next_helper.literalInit(.@"-");
                    }
                    tokenizer.state = .@"--";
                    tokenizer.index += 1;
                    continue;
                },
            },

            .@"--" => switch (ret_kind) {
                .type => {
                    if (tokenizer.index == src.len) {
                        tokenizer.state = .blank;
                        break .invalid_comment_dash_dash;
                    }
                    switch (src[tokenizer.index]) {
                        '-' => {
                            tokenizer.state = .@"---";
                            tokenizer.index += 1;
                            continue;
                        },
                        '>' => {
                            tokenizer.state = .blank;
                            tokenizer.index += 1;
                            break .comment_end;
                        },
                        else => {
                            tokenizer.state = .blank;
                            break .invalid_comment_dash_dash;
                        },
                    }
                },
                .src => {
                    if (tokenizer.index == src.len) break null;
                    if (src[tokenizer.index] != '-') break null;
                    tokenizer.state = .@"---";
                    tokenizer.index += 1;
                    break null;
                },
            },

            .@"---" => switch (ret_kind) {
                .type => {
                    if (tokenizer.index == src.len) {
                        tokenizer.state = .@"-";
                        break .invalid_comment_dash_dash;
                    }
                    switch (src[tokenizer.index]) {
                        '-' => {
                            tokenizer.state = .@"--";
                            tokenizer.index += 1;
                            break .invalid_comment_dash_dash;
                        },
                        '>' => {
                            tokenizer.state = .blank;
                            tokenizer.index += 1;
                            break .invalid_comment_end_triple_dash;
                        },
                        else => {
                            tokenizer.state = .@"-";
                            break .invalid_comment_dash_dash;
                        },
                    }
                },
                .src => unreachable,
            },

            else => unreachable,
        },
        .pi => switch (tokenizer.state) {
            .eof => break eof_result,

            .blank => switch (ret_kind) {
                .type => {
                    if (tokenizer.index == src.len) {
                        tokenizer.state = .eof;
                        break .eof;
                    }
                    if (src[tokenizer.index] != '?') {
                        break .text_data;
                    }
                    tokenizer.state = .@"?";
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
                        break next_helper.rangeInit(str_start, str_end);
                    }
                    tokenizer.state = .@"?";
                    tokenizer.index += 1;
                    continue;
                },
            },

            .@"?" => switch (ret_kind) {
                .type => {
                    if (tokenizer.index == src.len) break .text_data;
                    if (src[tokenizer.index] != '>') break .text_data;
                    tokenizer.state = .blank;
                    tokenizer.index += 1;
                    break .pi_end;
                },
                .src => {
                    if (tokenizer.index == src.len) {
                        tokenizer.state = .blank;
                        break next_helper.literalInit(.@"?");
                    }
                    if (src[tokenizer.index] != '>') {
                        tokenizer.state = .blank;
                        break next_helper.literalInit(.@"?");
                    }
                    break null;
                },
            },

            else => unreachable,
        },
        .cdata => switch (tokenizer.state) {
            .eof => break eof_result,

            .blank => switch (ret_kind) {
                .type => {
                    if (tokenizer.index == src.len) {
                        tokenizer.state = .eof;
                        break .eof;
                    }
                    if (src[tokenizer.index] != ']') {
                        break .text_data;
                    }
                    tokenizer.state = .@"]";
                    tokenizer.index += 1;
                    continue;
                },
                .src => {
                    if (tokenizer.index == src.len) {
                        tokenizer.state = .eof;
                        break null;
                    }
                    const str_start = tokenizer.index;
                    const str_end = std.mem.indexOfScalarPos(u8, src, tokenizer.index, ']') orelse src.len;
                    tokenizer.index = str_end;
                    if (str_start != str_end) {
                        break next_helper.rangeInit(str_start, str_end);
                    }
                    tokenizer.state = .@"]";
                    tokenizer.index += 1;
                    continue;
                },
            },

            inline .@"]", .@"]]", .@"]]]" => |state| switch (next_helper.handlePossibleCdataEnd(tokenizer, state, ret_type)) {
                .@"continue" => continue,
                .@"return" => |result| break result,
            },

            else => unreachable,
        },

        .system_literal_quote_single,
        .system_literal_quote_double,
        => |tag| switch (ret_kind) {
            .type => {
                if (tokenizer.index == src.len) {
                    tokenizer.state = .eof;
                    break .eof;
                }
                const matching_quote_char: u8, //
                const matching_quote_type: TokenType //
                = switch (tag) {
                    .system_literal_quote_single => .{ '\'', .quote_single },
                    .system_literal_quote_double => .{ '\"', .quote_double },
                    else => unreachable,
                };
                if (src[tokenizer.index] != matching_quote_char) {
                    break .text_data;
                }
                tokenizer.index += 1;
                break matching_quote_type;
            },
            .src => {
                if (tokenizer.index == src.len) {
                    tokenizer.state = .eof;
                    break null;
                }
                const matching_quote_char: u8 = switch (tag) {
                    .system_literal_quote_single => '\'',
                    .system_literal_quote_double => '\"',
                    else => unreachable,
                };
                const str_start = tokenizer.index;
                const str_end = std.mem.indexOfScalarPos(u8, src, tokenizer.index, matching_quote_char) orelse src.len;
                tokenizer.index = str_end;
                if (str_start == str_end) break null;
                break next_helper.rangeInit(str_start, str_end);
            },
        },

        .attribute_value_quote_single,
        .attribute_value_quote_double,

        .entity_value_quote_single,
        .entity_value_quote_double,
        => |tag| switch (tokenizer.state) {
            .eof => break eof_result,
            .blank => switch (ret_kind) {
                .type => {
                    if (tokenizer.index == src.len) {
                        tokenizer.state = .eof;
                        break .eof;
                    }

                    const matching_quote_char: u8, //
                    const matching_quote_type: TokenType //
                    = comptime switch (tag) {
                        .attribute_value_quote_single, .entity_value_quote_single => .{ '\'', .quote_single },
                        .attribute_value_quote_double, .entity_value_quote_double => .{ '\"', .quote_double },
                        else => unreachable,
                    };
                    if (src[tokenizer.index] == matching_quote_char) {
                        tokenizer.index += 1;
                        break matching_quote_type;
                    }
                    if (src[tokenizer.index] == '&') {
                        tokenizer.index += 1;
                        break .ampersand;
                    }
                    switch (tag) {
                        .attribute_value_quote_single,
                        .attribute_value_quote_double,
                        => if (src[tokenizer.index] == '%') {
                            tokenizer.index += 1;
                            break .percent;
                        },
                        .entity_value_quote_single,
                        .entity_value_quote_double,
                        => if (src[tokenizer.index] == '<') {
                            tokenizer.index += 1;
                            break .angle_bracket_left;
                        },
                        else => unreachable,
                    }
                    break .text_data;
                },
                .src => {
                    if (tokenizer.index == src.len) break null;
                    const matching_quote_char: u8 = comptime switch (tag) {
                        .attribute_value_quote_single, .entity_value_quote_single => '\'',
                        .attribute_value_quote_double, .entity_value_quote_double => '\"',
                        else => unreachable,
                    };
                    const str_start = tokenizer.index;
                    const str_end = std.mem.indexOfAnyPos(u8, src, tokenizer.index, &[_]u8{ matching_quote_char, '&', '<' }) orelse src.len;
                    tokenizer.index = str_end;
                    if (str_start == str_end) break null;
                    break next_helper.rangeInit(str_start, str_end);
                },
            },
            else => unreachable,
        },

        .reference => {
            const terminal_chars = whitespace_set ++ next_helper.dtd_terminal_characters ++ &[_]u8{'&'};
            switch (tokenizer.state) {
                .blank => switch (ret_kind) {
                    .type => {
                        if (tokenizer.index == src.len) {
                            tokenizer.state = .eof;
                            break .invalid_reference_end;
                        }
                        if (src[tokenizer.index] == ';') {
                            tokenizer.state = .blank;
                            tokenizer.index += 1;
                            break .semicolon;
                        }
                        if (std.mem.indexOfScalar(u8, terminal_chars, src[tokenizer.index]) != null) {
                            tokenizer.state = .blank;
                            break .invalid_reference_end;
                        }
                        break .tag_token;
                    },
                    .src => {
                        if (tokenizer.index == src.len) break null;
                        const str_start = tokenizer.index;
                        const str_end = std.mem.indexOfAnyPos(u8, src, tokenizer.index, terminal_chars) orelse src.len;
                        tokenizer.index = str_end;
                        if (str_start != str_end) {
                            break next_helper.rangeInit(str_start, str_end);
                        }
                        break null;
                    },
                },
                else => unreachable,
            }
        },
    } else error.BufferUnderrun;

    const result: switch (ret_kind) {
        .type => TokenType,
        .src => ?TokenSrc,
    } = switch (assert_eof_specified) {
        true => result_or_err,
        false => try result_or_err,
    };

    if (std.debug.runtime_safety) switch (ret_kind) {
        .type => {
            tokenizer.debug.src_queued_up = result.hasSrc();
        },
        .src => if (result == null) {
            tokenizer.debug.expected_context = null;
            tokenizer.debug.src_queued_up = false;
        },
    };

    return result;
}

const next_helper = struct {
    const dtd_terminal_characters = &[_]u8{
        '[',  ']',  '(', ')',
        '?',  '*',  '+', '|',
        ',',  '#',  '%', ';',
        '\'', '\"', '<', '>',
    };

    inline fn rangeInit(range_start: usize, range_end: usize) TokenSrc {
        return .{ .range = .{ .start = range_start, .end = range_end } };
    }
    inline fn literalInit(literal: Literal) TokenSrc {
        return .{ .literal = literal };
    }

    const ReturnType = enum {
        type,
        type_no_underrun,
        src,
        src_no_underrun,

        const Kind = enum { type, src };
        inline fn kind(ret_type: ReturnType) Kind {
            return switch (ret_type) {
                .type, .type_no_underrun => .type,
                .src, .src_no_underrun => .src,
            };
        }

        fn Type(ret_type: ReturnType) type {
            return switch (ret_type) {
                .type => BufferError!TokenType,
                .type_no_underrun => TokenType,
                .src => BufferError!?TokenSrc,
                .src_no_underrun => ?TokenSrc,
            };
        }
    };

    fn handlePossibleCdataEnd(
        _tokenizer: *Tokenizer,
        comptime state: State,
        comptime ret_type: ReturnType,
    ) union(enum) { @"continue", @"return": ret_type.Type() } {
        const src = _tokenizer.src;
        return switch (state) {
            .@"]" => switch (ret_type.kind()) {
                .type => {
                    if (_tokenizer.index == src.len) return .{ .@"return" = .text_data };
                    if (src[_tokenizer.index] != ']') return .{ .@"return" = .text_data };
                    _tokenizer.state = .@"]]";
                    _tokenizer.index += 1;
                    return .@"continue";
                },
                .src => {
                    if (_tokenizer.index == src.len) {
                        _tokenizer.state = .eof;
                        return .{ .@"return" = literalInit(.@"]") };
                    }
                    if (src[_tokenizer.index] != ']') {
                        _tokenizer.state = .blank;
                        return .{ .@"return" = literalInit(.@"]") };
                    }
                    _tokenizer.state = .@"]]";
                    _tokenizer.index += 1;
                    return .@"continue";
                },
            },
            .@"]]" => switch (ret_type.kind()) {
                .type => {
                    if (_tokenizer.index == src.len) return .{ .@"return" = .text_data };
                    switch (src[_tokenizer.index]) {
                        '>' => {
                            _tokenizer.state = .blank;
                            _tokenizer.index += 1;
                            return .{ .@"return" = .cdata_end };
                        },
                        ']' => return .{ .@"return" = .text_data },
                        else => return .{ .@"return" = .text_data },
                    }
                },
                .src => {
                    if (_tokenizer.index == src.len) {
                        _tokenizer.state = .eof;
                        return .{ .@"return" = literalInit(.@"]]") };
                    }
                    switch (src[_tokenizer.index]) {
                        '>' => return .{ .@"return" = null },
                        ']' => {
                            const prev_str = "]]";
                            if (_tokenizer.index >= prev_str.len) {
                                if (std.debug.runtime_safety) {
                                    const prev_str_start = _tokenizer.index - prev_str.len;
                                    assert(std.mem.eql(u8, prev_str, src[prev_str_start..][0..prev_str.len]));
                                }
                                // If the index is greater than the length of ']]', that means we have not been fed
                                // any input since encountering the first ']', meaning that `src[index - 2][0..3]` is
                                // equal to "]]]". If we're in streaming mode, this doesn't matter, as it is not legal
                                // to convert the `Literal` into a range; however, if we're in non-streaming mode, we
                                // must leave the tokenizer in a state which allows the caller to convert the returned
                                // `Literal` value into a range which accurately points at the first ']' character, and
                                // then return back to this state to continue checking if the ']]' sequence is followed
                                // by a '>' to form ']]>'.
                                // NOTE: we cannot limit this to `assert_eof_specified`, as the `*Stream` API could be
                                // used either equivalently to the `*NoUnderrun` API or during transition to the latter
                                // after `feedEof`.
                                // TODO: measure if limiting this to when `eof_specified` is true is worth the branch
                                // to avoid the future branch switch from `.@"]]]"` to `.@"]]"`.
                                _tokenizer.state = .@"]]]";
                                _tokenizer.index -= 1; // we move backwards one, so that the previous character is the first ']'.
                            } else {
                                _tokenizer.index += 1;
                            }
                            return .{ .@"return" = literalInit(.@"]") };
                        },
                        else => {
                            _tokenizer.state = .blank;
                            return .{ .@"return" = literalInit(.@"]]") };
                        },
                    }
                },
            },
            .@"]]]" => switch (ret_type.kind()) {
                .type => unreachable,
                .src => {
                    // currently the index is just in front of the first ']', indexing the second ']',
                    // so we move forwards two characters, to be just in front of the third ']'.
                    assert(std.mem.eql(u8, src[_tokenizer.index - 1 ..][0.."]]]".len], "]]]"));
                    _tokenizer.state = .@"]]";
                    _tokenizer.index += 2;
                    return .@"continue";
                },
            },
            else => comptime unreachable,
        };
    }
};

const State = enum {
    eof,

    blank,

    @"]",
    @"]]",
    /// At the time of writing, this is primarily used as an intermediate state to allow a non-streaming
    /// tokenizer to be queried for the accurate range for the first ']' character in the source.
    @"]]]",

    @"<",

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
    @"<!DOCTYPE",

    dtd_subtag_start,

    /// Finished returning the src for the invalid tag formed by
    /// '<!', followed by a partial match of a recognized sequence,
    /// including:
    /// * '<!DOCTYPE'
    /// * '<![CDATA['
    /// * '<!ENTITY'
    /// * '<!ELEMENT'
    /// * '<!ATTLIST'
    /// * '<!NOTATION'
    angle_bracket_left_bang_invalid_tag_returned,

    whitespace,

    @"-",
    @"--",
    @"---",

    @"&",

    @"?",
};

const Debug = struct {
    expected_context: if (std.debug.runtime_safety) ?Context else ?noreturn = null,
    src_queued_up: if (std.debug.runtime_safety) bool else void = if (std.debug.runtime_safety) false,
};

fn testTokenizer(
    opts: struct {
        start_size: usize = 1,
        max_buffer: ?usize = null,
    },
    src: []const u8,
    contexts_expected_tokens: []const struct { Context, TokenType, ?[]const u8 },
) !void {
    {
        var actual_src_buf = std.ArrayList(u8).init(std.testing.allocator);
        defer actual_src_buf.deinit();

        for (opts.start_size..opts.max_buffer orelse src.len) |buffer_size| {
            const helper = struct {
                fn getNextType(tokenizer: *Tokenizer, context: Context, feeder: *std.mem.WindowIterator(u8)) TokenType {
                    return tokenizer.nextTypeStream(context) catch while (true) {
                        if (feeder.next()) |input| {
                            tokenizer.feedInput(input);
                        } else {
                            tokenizer.feedEof();
                        }
                        break tokenizer.nextTypeStream(context) catch continue;
                    };
                }
            };

            var feeder = std.mem.window(u8, src, buffer_size, buffer_size);

            var tokenizer = Tokenizer.initStreaming();
            for (contexts_expected_tokens, 0..) |iteration_vals, i| {
                errdefer testingPrint("difference occured on token {d}\n", .{i});

                const context: Context, //
                const expected_tt: TokenType, //
                const expected_src: ?[]const u8 //
                = iteration_vals;
                if (expected_src != null) try std.testing.expect(expected_tt.hasSrc());

                const actual_tt: TokenType = helper.getNextType(&tokenizer, context, &feeder);
                const actual_src: ?[]const u8 = blk: {
                    if (!actual_tt.hasSrc()) break :blk null;

                    actual_src_buf.clearRetainingCapacity();
                    while (true) {
                        const segment = (tokenizer.nextSrcStream(context) catch while (true) {
                            if (feeder.next()) |input| {
                                tokenizer.feedInput(input);
                            } else {
                                tokenizer.feedEof();
                            }
                            break tokenizer.nextSrcStream(context) catch continue;
                        }) orelse break;
                        try actual_src_buf.appendSlice(segment);
                    }

                    assert(actual_src_buf.items.len != 0);
                    break :blk actual_src_buf.items;
                };

                try std.testing.expectEqual(expected_tt, actual_tt);

                const combo = packed struct(u2) {
                    a: bool,
                    b: bool,
                    inline fn combo(a: bool, b: bool) u2 {
                        return @bitCast(@This(){ .a = a, .b = b });
                    }
                }.combo;
                switch (combo(expected_src != null, actual_src != null)) {
                    combo(true, true) => try std.testing.expectEqualStrings(expected_src.?, actual_src.?),
                    combo(false, false) => {},
                    combo(true, false) => {
                        testingPrint("expected '{}', found null\n", .{std.zig.fmtEscapes(expected_src.?)});
                        return error.TestExpectedEqual;
                    },
                    combo(false, true) => {
                        testingPrint("expected null, found '{}'\n", .{std.zig.fmtEscapes(actual_src.?)});
                        return error.TestExpectedEqual;
                    },
                }
            }
            const ctx_count = contexts_expected_tokens.len;
            errdefer testingPrint("difference occured on token {d}\n", .{ctx_count});
            try std.testing.expectEqual(.eof, helper.getNextType(&tokenizer, if (ctx_count != 0) contexts_expected_tokens[ctx_count - 1][0] else .non_markup, &feeder));
        }
    }

    var tokenizer = Tokenizer.initComplete(src);
    for (contexts_expected_tokens, 0..) |iteration_vals, i| {
        errdefer testingPrint("difference occured on token {d}\n", .{i});

        const context: Context, //
        const expected_tt: TokenType, //
        const expected_src: ?[]const u8 //
        = iteration_vals;

        const actual_tt: TokenType = tokenizer.nextTypeNoUnderrun(context);
        const actual_src: ?[]const u8 = blk: {
            if (!actual_tt.hasSrc()) break :blk null;
            const range = tokenizer.nextSrcNoUnderrun(context);
            break :blk range.toStr(tokenizer.src);
        };

        try std.testing.expectEqual(expected_tt, actual_tt);

        const combo = packed struct(u2) {
            a: bool,
            b: bool,
            inline fn combo(a: bool, b: bool) u2 {
                return @bitCast(@This(){ .a = a, .b = b });
            }
        }.combo;
        switch (combo(expected_src != null, actual_src != null)) {
            combo(true, true) => try std.testing.expectEqualStrings(expected_src.?, actual_src.?),
            combo(false, false) => {},
            combo(true, false) => {
                testingPrint("expected '{}', found null\n", .{std.zig.fmtEscapes(expected_src.?)});
                return error.TestExpectedEqual;
            },
            combo(false, true) => {
                testingPrint("expected null, found '{}'\n", .{std.zig.fmtEscapes(actual_src.?)});
                return error.TestExpectedEqual;
            },
        }
    }
    const ctx_count = contexts_expected_tokens.len;
    errdefer testingPrint("difference occured on token {d}\n", .{ctx_count});
    try std.testing.expectEqual(.eof, tokenizer.nextTypeNoUnderrun(if (ctx_count != 0) contexts_expected_tokens[ctx_count - 1][0] else .non_markup));
}

fn testingPrint(comptime fmt_str: []const u8, args: anytype) void {
    if (@inComptime()) {
        @compileError(std.fmt.comptimePrint(fmt_str, args));
    } else if (std.testing.backend_can_print) {
        std.debug.print(fmt_str, args);
    }
}

test "Non-Markup" {
    try testTokenizer(.{}, "  <", &.{
        .{ .non_markup, .text_data, "  " },
        .{ .non_markup, .angle_bracket_left, null },
    });
    try testTokenizer(.{}, "  &", &.{
        .{ .non_markup, .text_data, "  " },
        .{ .non_markup, .ampersand, null },
    });
    try testTokenizer(.{}, "  ]", &.{
        .{ .non_markup, .text_data, "  ]" },
    });
    try testTokenizer(.{}, "  ]]", &.{
        .{ .non_markup, .text_data, "  ]]" },
    });
    try testTokenizer(.{}, "  ]]>", &.{
        .{ .non_markup, .text_data, "  " },
        .{ .non_markup, .cdata_end, null },
    });
}

test "References" {
    try testTokenizer(.{}, ";", &.{ .{ .reference, .semicolon, null }, .{ .non_markup, .eof, null } });
    try testTokenizer(.{}, ";", &.{ .{ .reference, .semicolon, null }, .{ .non_markup, .eof, null } });
    try testTokenizer(.{}, ";", &.{ .{ .reference, .semicolon, null }, .{ .non_markup, .eof, null } });

    try testTokenizer(.{}, " ", &.{ .{ .reference, .invalid_reference_end, null }, .{ .non_markup, .text_data, " " } });
    try testTokenizer(.{}, " ", &.{ .{ .reference, .invalid_reference_end, null }, .{ .attribute_value_quote_single, .text_data, " " } });
    try testTokenizer(.{}, " ", &.{ .{ .reference, .invalid_reference_end, null }, .{ .dtd, .tag_whitespace, " " } });
    try testTokenizer(.{}, "lt", &.{ .{ .reference, .tag_token, "lt" }, .{ .reference, .invalid_reference_end, null }, .{ .dtd, .eof, null } });
    try testTokenizer(.{}, "lt;", &.{ .{ .reference, .tag_token, "lt" }, .{ .reference, .semicolon, null }, .{ .dtd, .eof, null } });
    // TODO: more exhaustive testing of invariants
}

test "Attribute Value" {
    try testTokenizer(.{}, "\'", &.{.{ .attribute_value_quote_single, .quote_single, null }});
    try testTokenizer(.{}, "\"", &.{.{ .attribute_value_quote_double, .quote_double, null }});
    // TODO: more exhaustive testing of invariants
}

test "Element tags" {
    try testTokenizer(.{}, "/foo/bar/>", &.{
        .{ .element_tag, .slash, null },
        .{ .element_tag, .tag_token, "foo" },
        .{ .element_tag, .slash, null },
        .{ .element_tag, .tag_token, "bar" },
        .{ .element_tag, .slash, null },
        .{ .element_tag, .angle_bracket_right, null },
    });
}

test "CDATA" {
    try testTokenizer(.{}, "<![CDATA[", &.{.{ .non_markup, .cdata_start, null }});
    try testTokenizer(.{}, "]]>", &.{.{ .non_markup, .cdata_end, null }}); // invalid token in textual data
    try testTokenizer(.{}, "<![CDATA", &.{
        .{ .non_markup, .invalid_cdata_start, "<![CDATA" },
    });

    try testTokenizer(.{}, "]]>", &.{
        .{ .cdata, .cdata_end, null },
    });
    try testTokenizer(.{}, "]]]>", &.{
        .{ .cdata, .text_data, "]" },
        .{ .cdata, .cdata_end, null },
    });
    try testTokenizer(.{}, "] ]]>", &.{
        .{ .cdata, .text_data, "] " },
        .{ .cdata, .cdata_end, null },
    });
    try testTokenizer(.{}, "]>]]>", &.{
        .{ .cdata, .text_data, "]>" },
        .{ .cdata, .cdata_end, null },
    });
    try testTokenizer(.{}, "]>", &.{
        .{ .cdata, .text_data, "]>" },
    });
    try testTokenizer(.{}, "]>]]", &.{
        .{ .cdata, .text_data, "]>]]" },
    });
    try testTokenizer(.{}, " stuff ]] >]]]>", &.{
        .{ .cdata, .text_data, " stuff ]] >]" },
        .{ .cdata, .cdata_end, null },
    });
}

test "PI" {
    try testTokenizer(.{}, "<?", &.{.{ .non_markup, .pi_start, null }});
    try testTokenizer(.{}, "<?", &.{.{ .dtd, .pi_start, null }});
    try testTokenizer(.{}, "?>", &.{.{ .pi, .pi_end, null }});

    try testTokenizer(.{}, "?", &.{
        .{ .pi, .text_data, "?" },
    });
    try testTokenizer(.{}, "? >", &.{
        .{ .pi, .text_data, "? >" },
    });
    try testTokenizer(.{}, " ? >", &.{
        .{ .pi, .text_data, " ? >" },
    });
    try testTokenizer(.{}, "??>", &.{
        .{ .pi, .text_data, "?" },
        .{ .pi, .pi_end, null },
    });
    try testTokenizer(.{}, ">??>", &.{
        .{ .pi, .text_data, ">?" },
        .{ .pi, .pi_end, null },
    });
    try testTokenizer(.{}, "xml version=\"1.0\" encoding=\"UTF-8\"?>", &.{
        .{ .pi, .text_data, "xml version=\"1.0\" encoding=\"UTF-8\"" },
        .{ .pi, .pi_end, null },
    });
}

test "Comment" {
    try testTokenizer(.{}, "<!--", &.{.{ .non_markup, .comment_start, null }});
    try testTokenizer(.{}, "<!--", &.{.{ .dtd, .comment_start, null }});
    try testTokenizer(.{}, "<!-", &.{.{ .non_markup, .invalid_comment_start_single_dash, null }});
    try testTokenizer(.{}, "<!-", &.{.{ .dtd, .invalid_comment_start_single_dash, null }});
    try testTokenizer(.{}, "-->", &.{.{ .comment, .comment_end, null }});

    try testTokenizer(.{}, "--", &.{.{ .comment, .invalid_comment_dash_dash, null }});
    try testTokenizer(.{}, "--->", &.{.{ .comment, .invalid_comment_end_triple_dash, null }});
    try testTokenizer(.{}, " - ->", &.{.{ .comment, .text_data, " - ->" }});
    try testTokenizer(.{}, "foo-bar -- -->", &.{
        .{ .comment, .text_data, "foo-bar " },
        .{ .comment, .invalid_comment_dash_dash, null },
        .{ .comment, .text_data, " " },
        .{ .comment, .comment_end, null },
    });
}

test "DTD" {
    try testTokenizer(.{}, "<!DOCTYPE", &.{.{ .non_markup, .dtd_start, null }});
    try testTokenizer(.{}, "<!DOCTY", &.{.{ .non_markup, .invalid_dtd_start, "<!DOCTY" }});
    try testTokenizer(.{}, "<!DOCTYPEE", &.{.{ .non_markup, .invalid_dtd_start, "<!DOCTYPEE" }});
    try testTokenizer(.{}, "<!DOCTYPEE>", &.{
        .{ .non_markup, .invalid_dtd_start, "<!DOCTYPEE" },
        .{ .dtd, .angle_bracket_right, null },
    });

    try testTokenizer(.{}, "<!ENTITY> <!ATTLIST> <!ELEMENT> <!NOTATION> <!EEEE> >", &.{
        .{ .dtd, .dtd_decl, "<!ENTITY" },
        .{ .dtd, .angle_bracket_right, null },
        .{ .dtd, .tag_whitespace, " " },

        .{ .dtd, .dtd_decl, "<!ATTLIST" },
        .{ .dtd, .angle_bracket_right, null },
        .{ .dtd, .tag_whitespace, " " },

        .{ .dtd, .dtd_decl, "<!ELEMENT" },
        .{ .dtd, .angle_bracket_right, null },
        .{ .dtd, .tag_whitespace, " " },

        .{ .dtd, .dtd_decl, "<!NOTATION" },
        .{ .dtd, .angle_bracket_right, null },
        .{ .dtd, .tag_whitespace, " " },

        .{ .dtd, .dtd_decl, "<!EEEE" },
        .{ .dtd, .angle_bracket_right, null },
        .{ .dtd, .tag_whitespace, " " },

        .{ .dtd, .angle_bracket_right, null },
    });

    try testTokenizer(.{}, "<!ENTITYYY", &.{.{ .dtd, .dtd_decl, "<!ENTITYYY" }});
    try testTokenizer(.{}, "()?*+|,%[]#", &.{
        .{ .dtd, .lparen, null },
        .{ .dtd, .rparen, null },
        .{ .dtd, .qmark, null },
        .{ .dtd, .asterisk, null },
        .{ .dtd, .plus, null },
        .{ .dtd, .pipe, null },
        .{ .dtd, .comma, null },
        .{ .dtd, .percent, null },
        // .{ .dtd, .tag_token, ";" },
        .{ .dtd, .square_bracket_left, null },
        .{ .dtd, .square_bracket_right, null },
        .{ .dtd, .hashtag, null },
    });
}

test "SystemLiteral" {
    try testTokenizer(.{}, "\'", &.{.{ .system_literal_quote_single, .quote_single, null }});
    try testTokenizer(.{}, "\"", &.{.{ .system_literal_quote_double, .quote_double, null }});
    try testTokenizer(.{}, "\"", &.{.{ .system_literal_quote_single, .text_data, "\"" }});
    try testTokenizer(.{}, "\'", &.{.{ .system_literal_quote_double, .text_data, "\'" }});
    try testTokenizer(.{}, "foo bar\"\'", &.{
        .{ .system_literal_quote_single, .text_data, "foo bar\"" },
        .{ .system_literal_quote_single, .quote_single, null },
    });
    try testTokenizer(.{}, "foo bar\'\"", &.{
        .{ .system_literal_quote_double, .text_data, "foo bar\'" },
        .{ .system_literal_quote_double, .quote_double, null },
    });
}

test "General Test" {
    try testTokenizer(
        .{},
        \\<?xml version="1.0" encoding="UTF-8"?>
        \\
        \\]]>
        \\
        \\ <!-- commented -- -->
        \\ <!-- commented -- --->
        \\
        \\<foo>
        \\  Lorem ipsum
        \\  <bar fizz='buzz'><baz/></bar>
        \\<![CDATA[ stuff ]] >]]]>
        \\</foo>
        \\
    ,
        &[_]struct { Context, TokenType, ?[]const u8 }{
            .{ .non_markup, .pi_start, null },
            .{ .pi, .text_data, "xml version=\"1.0\" encoding=\"UTF-8\"" },
            .{ .pi, .pi_end, null },

            .{ .non_markup, .text_data, "\n\n" },

            .{ .non_markup, .cdata_end, null },

            .{ .non_markup, .text_data, "\n\n " },

            .{ .non_markup, .comment_start, null },
            .{ .comment, .text_data, " commented " },
            .{ .comment, .invalid_comment_dash_dash, null },
            .{ .comment, .text_data, " " },
            .{ .comment, .comment_end, null },

            .{ .non_markup, .text_data, "\n " },

            .{ .non_markup, .comment_start, null },
            .{ .comment, .text_data, " commented " },
            .{ .comment, .invalid_comment_dash_dash, null },
            .{ .comment, .text_data, " " },
            .{ .comment, .invalid_comment_end_triple_dash, null },

            .{ .non_markup, .text_data, "\n\n" },

            .{ .non_markup, .angle_bracket_left, null },
            .{ .element_tag, .tag_token, "foo" },
            .{ .element_tag, .angle_bracket_right, null },
            .{ .non_markup, .text_data, "\n  Lorem ipsum\n  " },

            .{ .non_markup, .angle_bracket_left, null },
            .{ .element_tag, .tag_token, "bar" },
            .{ .element_tag, .tag_whitespace, " " },
            .{ .element_tag, .tag_token, "fizz" },
            .{ .element_tag, .equals, null },
            .{ .element_tag, .quote_single, null },
            .{ .attribute_value_quote_single, .text_data, "buzz" },
            .{ .attribute_value_quote_single, .quote_single, null },
            .{ .element_tag, .angle_bracket_right, null },

            .{ .non_markup, .angle_bracket_left, null },
            .{ .element_tag, .tag_token, "baz" },
            .{ .element_tag, .slash, null },
            .{ .element_tag, .angle_bracket_right, null },

            .{ .non_markup, .angle_bracket_left, null },
            .{ .element_tag, .slash, null },
            .{ .element_tag, .tag_token, "bar" },
            .{ .element_tag, .angle_bracket_right, null },

            .{ .non_markup, .text_data, "\n" },

            .{ .non_markup, .cdata_start, null },
            .{ .cdata, .text_data, " stuff ]] >]" },
            .{ .cdata, .cdata_end, null },

            .{ .non_markup, .text_data, "\n" },

            .{ .non_markup, .angle_bracket_left, null },
            .{ .element_tag, .slash, null },
            .{ .element_tag, .tag_token, "foo" },
            .{ .element_tag, .angle_bracket_right, null },

            .{ .non_markup, .text_data, "\n" },

            .{ .non_markup, .eof, null },
        },
    );
}
