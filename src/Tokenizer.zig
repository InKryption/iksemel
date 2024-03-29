//! The lowest level API for interpreting an XML document.
//! While simple, it provides little to no safety in the event of API misuse.
//! It has two possible modes of operation: streaming and non-streaming.
//!
//! * Streaming Source: tokenizer is initialized with `initStream`, requires
//! the programmer to make use of the `stream.feedInput` method after encountering
//! `error.BufferUnderrun` while using the `stream.nextType` and `stream.nextSrc`
//! methods. `stream.feedEof` must be used to signal the end of the XML source.
//!
//! * Full Source: tokenizer is initialized with `initFull`. This mode allows use
//! of a subset of the stream source API (`stream.nextType`, `stream.nextSrc`), in
//! addition to the mirror methods `full.nextType` & `full.nextSrc`, which take
//! advantage of the assumption that the entirety of the of the XML source has
//! been fed. The methods `stream.feedInput` and `stream.feedEof` are illegal
//! in this mode.
//!
//! Important to note: if the tokenizer was first initialized with `initStream`,
//! and then becomes non-streaming via a call to `feedEof`, immediately following
//! a call to `feedInput` as a response to `error.BufferUnderrun`, the currently
//! in-bound token may still be returned partially, as part of it may have existed
//! in the previous buffer; after the partially returned token is acquired, any
//! subsequent tokens will be available in full, returned as such by the `stream`
//! API, or explicitly by the `full` API.
//!
//! The tokenization of a valid document requires no special considerations,
//! however the tokenizer is error-tolerant, and defines specially recognized
//! invalid cases which should be treated as failures at some point, if not
//! at the point of tokenization.
const Tokenizer = @This();
src: []const u8,
index: usize,
state: State,
eof_specified: bool,
debug: Debug,

/// Public namespace field exposing the stream source API.
///
/// A pointer to this field may be passed around to indicate
/// intent to specifically interact with the stream source API.
///
/// Must not be copied, only used by reference, as its address
/// is only meaningful as a pointer to the `Tokenizer`.
stream: Stream,

/// Public namespace field exposing the full source API.
///
/// A pointer to this field may be passed around to indicate
/// intent to specifically interact with the full source API.
///
/// Must not be copied, only used by reference, as its address
/// is only meaningful as a pointer to the `Tokenizer`.
full: Full,

pub inline fn initStream() Tokenizer {
    return .{
        .src = "",
        .index = 0,
        .state = .blank,
        .eof_specified = false,
        .debug = .{},

        .stream = .{},
        .full = .{},
    };
}

/// Initializes the `Tokenizer` with the full input.
/// Calling `stream.feedInput` or `stream.feedEof` is illegal.
/// Treating `error.BufferUnderrun` as `unreachable` is safe.
/// Equivalent initialising a streaming tokenizer, feeding it
/// `src`, and then feeding it eof.
pub inline fn initFull(src: []const u8) Tokenizer {
    var tokenizer = Tokenizer.initStream();
    tokenizer.stream.feedInput(src);
    tokenizer.stream.feedEof();
    return tokenizer;
}

/// Returns the range into `tokenizer.src` representing the remaining source
/// string which hasn't been returned as a token type or as a string.
pub inline fn remainingSrc(tokenizer: Tokenizer) Range {
    return .{ .start = tokenizer.index, .end = tokenizer.src.len };
}

pub const Stream = struct {
    pub inline fn asTokenizer(stream: *Stream) *Tokenizer {
        return @fieldParentPtr(Tokenizer, "stream", stream);
    }

    pub const BufferError = error{BufferUnderrun};

    pub fn feedInput(stream: *Tokenizer.Stream, src: []const u8) void {
        const tokenizer = stream.asTokenizer();
        assert(!tokenizer.eof_specified);
        assert(tokenizer.index == tokenizer.src.len);
        tokenizer.src = src;
        tokenizer.index = 0;
    }

    pub fn feedEof(stream: *Tokenizer.Stream) void {
        const tokenizer = stream.asTokenizer();
        assert(!tokenizer.eof_specified);
        assert(tokenizer.index == 0 or tokenizer.index == tokenizer.src.len);
        tokenizer.eof_specified = true;
    }

    pub fn nextType(stream: *Tokenizer.Stream, context: Context) BufferError!TokenType {
        const tokenizer = stream.asTokenizer();
        return tokenizer.nextTypeOrSrcImpl(context, .type);
    }

    pub fn nextSrc(stream: *Tokenizer.Stream, context: Context) BufferError!?[]const u8 {
        const tokenizer = stream.asTokenizer();
        const maybe_tok_src: ?TokenSrc = try tokenizer.nextTypeOrSrcImpl(context, .src);
        const tok_src = maybe_tok_src orelse return null;
        return switch (tok_src) {
            .range => |range| range.toStr(tokenizer.src),
            .literal => |literal| literal.toStr(),
        };
    }
};

pub const Full = struct {
    pub inline fn asTokenizer(full: *Full) *Tokenizer {
        const tokenizer = @fieldParentPtr(Tokenizer, "full", full);
        assert(tokenizer.eof_specified);
        return tokenizer;
    }

    pub fn nextType(full: *Tokenizer.Full, context: Context) TokenType {
        const tokenizer = full.asTokenizer();
        return tokenizer.nextTypeOrSrcImpl(context, .type_no_underrun);
    }

    pub fn nextSrc(full: *Tokenizer.Full, context: Context) ?Range {
        const tokenizer = full.asTokenizer();
        const tok_src: TokenSrc = tokenizer.nextTypeOrSrcImpl(context, .src_no_underrun) orelse return null;
        return switch (tok_src) {
            .range => |range| range,
            .literal => |literal| literal.toRange(tokenizer),
        };
    }

    pub fn nextSrcComplete(full: *Tokenizer.Full, context: Context) Range {
        var range: Range = full.nextSrc(context).?;
        while (full.nextSrc(context)) |subsequent_range| {
            assert(range.end == subsequent_range.start);
            range.end = subsequent_range.end;
        }
        return range;
    }
};

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
    /// * `.angle_bracket_left_bang`
    non_markup,

    /// Possible token types are:
    /// * `.eof`
    /// * `.text_data`
    /// * `.cdata_end`
    cdata,

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
    /// * `.hashtag`
    /// * `.tag_token`
    /// * `.invalid_reference_end`
    /// * `.semicolon`
    reference,

    /// Possible token types are:
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
    /// * `.angle_bracket_left_bang`
    /// * `.angle_bracket_left_bang_square_bracket_left`
    /// * `.invalid_comment_start_single_dash`
    /// * `.comment_start`
    /// * `.tag_whitespace`
    /// * `.tag_token`
    dtd,

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
};

pub const TokenType = enum {
    /// A run of any non-markup characters, including whitespace.
    /// This will never be returned consecutively.
    text_data,
    /// A run of one or more whitespace characters inside markup.
    /// This will never be returned consecutively.
    tag_whitespace,
    /// A run of one or more non-whitespace characters inside a markup tag.
    /// This will never be returned consecutively.
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

    /// The '%' token.
    percent,
    /// The '&' token.
    ampersand,
    /// The ';' token.
    semicolon,
    /// Encountered a terminating character other than the ';' token after the ampersand.
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

    /// The '<!' token.
    ///
    /// Outside the DTD this is an invalid token; outside the DTD,
    /// this is potentially the start of a declaration when followed
    /// by an applicable `.tag_token`.
    angle_bracket_left_bang,
    /// The '<![' token.
    ///
    /// This is returned when inside the DTD.
    angle_bracket_left_bang_square_bracket_left,

    /// The '<!DOCTYPE' token.
    dtd_start,
    /// A token which partially matches the '<!DOCTYPE' token.
    ///
    /// The source for the matched token is returned.
    invalid_dtd_start,

    /// Whether or not the token represents any text to be returned by `nextSrc`.
    pub inline fn hasSrc(token_type: TokenType) bool {
        return switch (token_type) {
            .text_data,
            .tag_whitespace,
            .tag_token,

            .invalid_cdata_start,
            .invalid_dtd_start,
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
            .hashtag => "#",
            .qmark => "?",
            .asterisk => "*",
            .plus => "+",

            .slash => "/",

            .quote_single => "\'",
            .quote_double => "\"",

            .angle_bracket_left => "<",
            .angle_bracket_right => ">",

            .square_bracket_left => "[",
            .square_bracket_right => "]",

            .percent => "%",

            .ampersand => "&",
            .semicolon => ";",
            .invalid_reference_end => null,

            .pi_start => "<?",
            .pi_end => "?>",

            .cdata_start => "<![CDATA[",
            .invalid_cdata_start => null,
            .cdata_end => "]]>",

            .comment_start => "<!--",
            .invalid_comment_start_single_dash => "<!-",
            .invalid_comment_dash_dash => "--",
            .invalid_comment_end_triple_dash => "--->",
            .comment_end => "-->",

            .angle_bracket_left_bang => "<!",
            .angle_bracket_left_bang_square_bracket_left => "<![",

            .dtd_start => "<!DOCTYPE",
            .invalid_dtd_start => null,
        };
    }
};

pub const Range = struct {
    start: usize,
    end: usize,

    pub inline fn toStr(range: Range, src: []const u8) []const u8 {
        return src[range.start..range.end];
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

pub const QuoteType = enum {
    single,
    double,

    pub const Char = std.math.IntFittingRange(
        @min('\'', '\"'),
        @max('\'', '\"'),
    );
    pub inline fn toChar(qt: QuoteType) Char {
        return switch (qt) {
            .single => '\'',
            .double => '\"',
        };
    }
    pub inline fn fromChar(char: Char) ?QuoteType {
        return switch (char) {
            '\'' => .single,
            '\"' => .double,
            else => null,
        };
    }

    pub inline fn toTokenType(qt: QuoteType) Tokenizer.TokenType {
        return switch (qt) {
            .single => .quote_single,
            .double => .quote_double,
        };
    }
    pub inline fn fromTokenType(tt: Tokenizer.TokenType) ?QuoteType {
        return switch (tt) {
            .quote_single => .single,
            .quote_double => .double,
            else => null,
        };
    }

    /// Returns `.system_literal_quote_<qt>`.
    pub inline fn systemLiteralCtx(qt: QuoteType) Tokenizer.Context {
        return switch (qt) {
            .single => .system_literal_quote_single,
            .double => .system_literal_quote_double,
        };
    }

    /// Returns `.attribute_value_quote_<qt>`.
    pub inline fn attributeValueCtx(qt: QuoteType) Tokenizer.Context {
        return switch (qt) {
            .single => .attribute_value_quote_single,
            .double => .attribute_value_quote_double,
        };
    }

    /// Returns `.entity_value_quote_<qt>`.
    pub inline fn entityValueCtx(qt: QuoteType) Tokenizer.Context {
        return switch (qt) {
            .single => .entity_value_quote_single,
            .double => .entity_value_quote_double,
        };
    }

    pub inline fn fromCtx(ctx: Tokenizer.Context) ?QuoteType {
        return switch (ctx) {
            .system_literal_quote_single => .single,
            .system_literal_quote_double => .double,

            .attribute_value_quote_single => .single,
            .attribute_value_quote_double => .double,

            .entity_value_quote_single => .single,
            .entity_value_quote_double => .double,

            else => null,
        };
    }
};

const TokenSrc = union(enum) {
    range: Range,
    literal: Literal,
};

const Literal = enum {
    @"/",
    @"?",
    @"-",

    @"]",
    @"]]",

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

    /// For a streaming `Tokenizer`, this is illegal.
    /// For a non-streaming `Tokenizer`, this must be called immediately after it is returned
    /// from `stream.nextSrc` to get the source range, which may then be turned into a string which
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

const nextTypeOrSrcImpl = if (std.debug.runtime_safety)
    nextTypeOrSrcImplDebugChecks
else
    nextTypeOrSrcImplUnchecked;

inline fn nextTypeOrSrcImplDebugChecks(
    tokenizer: *Tokenizer,
    context: Context,
    comptime ret_type: next_helper.ReturnType,
) ret_type.Type() {
    const ret_kind = comptime ret_type.kind();

    if (comptime std.debug.runtime_safety) switch (ret_kind) {
        .type => {
            assert(!tokenizer.debug.src_queued_up);
            if (tokenizer.debug.expected_context) |expected_context| {
                assert(context == expected_context);
            } else {
                tokenizer.debug.expected_context = context;
            }
        },
        .src => {
            assert(tokenizer.debug.src_queued_up);
            assert(context == tokenizer.debug.expected_context.?);
        },
    };

    const result: switch (ret_kind) {
        .type => TokenType,
        .src => ?TokenSrc,
    } = switch (ret_type) {
        .type,
        .src,
        => try nextTypeOrSrcImplUnchecked(tokenizer, context, ret_type),
        .type_no_underrun,
        .src_no_underrun,
        => nextTypeOrSrcImplUnchecked(tokenizer, context, ret_type),
    };

    if (std.debug.runtime_safety) switch (ret_kind) {
        .type => {
            tokenizer.debug.src_queued_up = result.hasSrc();
            if (!tokenizer.debug.src_queued_up) {
                tokenizer.debug.expected_context = null;
            }
        },
        .src => if (result == null) {
            tokenizer.debug.expected_context = null;
            tokenizer.debug.src_queued_up = false;
        },
    };

    return result;
}

/// This is the core of the implementation upon which everything else is built.
fn nextTypeOrSrcImplUnchecked(
    tokenizer: *Tokenizer,
    context: Context,
    comptime ret_type: next_helper.ReturnType,
) ret_type.Type() {
    const ret_kind = comptime ret_type.kind();
    const assert_eof_specified: bool = comptime switch (ret_type) {
        .type, .src => false,
        .type_no_underrun, .src_no_underrun => true,
    };

    const src = tokenizer.src;
    assert(!assert_eof_specified or tokenizer.eof_specified);
    const eof_specified = assert_eof_specified or tokenizer.eof_specified;

    @setEvalBranchQuota(3000);
    const ctxState = next_helper.ctxState;
    return while (tokenizer.index != src.len or eof_specified) break switch (ctxState(context, tokenizer.state)) {
        ctxState(.non_markup, .eof),
        ctxState(.cdata, .eof),
        ctxState(.dtd, .eof),
        ctxState(.element_tag, .eof),
        ctxState(.comment, .eof),
        ctxState(.pi, .eof),
        ctxState(.reference, .eof),

        ctxState(.system_literal_quote_single, .eof),
        ctxState(.system_literal_quote_double, .eof),

        ctxState(.attribute_value_quote_single, .eof),
        ctxState(.attribute_value_quote_double, .eof),

        ctxState(.entity_value_quote_single, .eof),
        ctxState(.entity_value_quote_double, .eof),
        => break switch (ret_kind) {
            .type => .eof,
            .src => null,
        },

        ctxState(.non_markup, .blank) => switch (ret_kind) {
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
                const str_end = std.mem.indexOfAnyPos(u8, src, tokenizer.index, comptime &[_]u8{ ']', '&', '<' }) orelse src.len;
                tokenizer.index = str_end;
                if (str_start != str_end) break next_helper.rangeInit(str_start, str_end);
                if (src[tokenizer.index] != ']') break null;
                tokenizer.state = .@"]";
                tokenizer.index += 1;
                continue;
            },
        },
        ctxState(.non_markup, .@"<") => switch (ret_kind) {
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
        ctxState(.non_markup, .@"<!") => switch (ret_kind) {
            .type => {
                if (tokenizer.index == src.len) {
                    tokenizer.state = .eof;
                    break .angle_bracket_left_bang;
                }
                tokenizer.state = switch (src[tokenizer.index]) {
                    '-' => .@"<!-",
                    '[' => .@"<![",
                    'D' => .@"<!D",
                    else => {
                        tokenizer.state = .blank;
                        break .angle_bracket_left_bang;
                    },
                };
                tokenizer.index += 1;
                continue;
            },
            .src => unreachable,
        },
        ctxState(.non_markup, .@"<!-") => switch (ret_kind) {
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

        ctxState(.non_markup, .@"<!["),
        ctxState(.non_markup, .@"<![C"),
        ctxState(.non_markup, .@"<![CD"),
        ctxState(.non_markup, .@"<![CDA"),
        ctxState(.non_markup, .@"<![CDAT"),
        ctxState(.non_markup, .@"<![CDATA"),
        => |ctx_state| {
            const tag = ctx_state.bits().state;
            switch (ret_kind) {
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
            }
        },

        ctxState(.non_markup, .@"<!D"),
        ctxState(.non_markup, .@"<!DO"),
        ctxState(.non_markup, .@"<!DOC"),
        ctxState(.non_markup, .@"<!DOCT"),
        ctxState(.non_markup, .@"<!DOCTY"),
        ctxState(.non_markup, .@"<!DOCTYP"),
        => |ctx_state| {
            const tag = ctx_state.bits().state;
            switch (ret_kind) {
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
            }
        },
        ctxState(.non_markup, .@"<!DOCTYPE") => switch (ret_kind) {
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
        ctxState(.non_markup, .angle_bracket_left_bang_invalid_tag_returned) => switch (ret_kind) {
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

        ctxState(.non_markup, .@"]"),
        ctxState(.cdata, .@"]"),
        => switch (ret_type.kind()) {
            .type => {
                if (tokenizer.index == src.len) break .text_data;
                if (src[tokenizer.index] != ']') break .text_data;
                tokenizer.state = .@"]]";
                tokenizer.index += 1;
                continue;
            },
            .src => {
                if (tokenizer.index == src.len) {
                    tokenizer.state = .eof;
                    break next_helper.literalInit(.@"]");
                }
                if (src[tokenizer.index] != ']') {
                    tokenizer.state = .blank;
                    break next_helper.literalInit(.@"]");
                }
                tokenizer.state = .@"]]";
                tokenizer.index += 1;
                continue;
            },
        },
        ctxState(.non_markup, .@"]]"),
        ctxState(.cdata, .@"]]"),
        => switch (ret_type.kind()) {
            .type => {
                if (tokenizer.index == src.len) break .text_data;
                switch (src[tokenizer.index]) {
                    '>' => {
                        tokenizer.state = .blank;
                        tokenizer.index += 1;
                        break .cdata_end;
                    },
                    ']' => break .text_data,
                    else => break .text_data,
                }
            },
            .src => {
                if (tokenizer.index == src.len) {
                    tokenizer.state = .eof;
                    break next_helper.literalInit(.@"]]");
                }
                switch (src[tokenizer.index]) {
                    '>' => break null,
                    ']' => {
                        const prev_str = "]]";
                        if (tokenizer.index >= prev_str.len) {
                            if (std.debug.runtime_safety) {
                                const prev_str_start = tokenizer.index - prev_str.len;
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
                            tokenizer.state = .@"]]]";
                            tokenizer.index -= 1; // we move backwards one, so that the previous character is the first ']'.
                        } else {
                            tokenizer.index += 1;
                        }
                        break next_helper.literalInit(.@"]");
                    },
                    else => {
                        tokenizer.state = .blank;
                        break next_helper.literalInit(.@"]]");
                    },
                }
            },
        },
        ctxState(.non_markup, .@"]]]"),
        ctxState(.cdata, .@"]]]"),
        => switch (ret_type.kind()) {
            .type => unreachable,
            .src => {
                // currently the index is just in front of the first ']', indexing the second ']',
                // so we move forwards two characters, to be just in front of the third ']'.
                assert(std.mem.eql(u8, src[tokenizer.index - 1 ..][0.."]]]".len], "]]]"));
                tokenizer.state = .@"]]";
                tokenizer.index += 2;
                continue;
            },
        },

        ctxState(.cdata, .blank) => switch (ret_kind) {
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

        ctxState(.element_tag, .blank) => switch (ret_kind) {
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
        ctxState(.element_tag, .whitespace) => switch (ret_kind) {
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

        ctxState(.comment, .blank) => switch (ret_kind) {
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
        ctxState(.comment, .@"-") => switch (ret_kind) {
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
        ctxState(.comment, .@"--") => switch (ret_kind) {
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
        ctxState(.comment, .@"---") => switch (ret_kind) {
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

        ctxState(.pi, .blank) => switch (ret_kind) {
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
        ctxState(.pi, .@"?") => switch (ret_kind) {
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

        ctxState(.reference, .blank) => {
            const terminal_chars = whitespace_set ++ next_helper.dtd_terminal_characters ++ &[_]u8{'&'};
            switch (ret_kind) {
                .type => {
                    if (tokenizer.index == src.len) {
                        tokenizer.state = .eof;
                        break .eof;
                    }
                    if (src[tokenizer.index] == '#') {
                        tokenizer.index += 1;
                        break .hashtag;
                    }
                    if (src[tokenizer.index] == ';') {
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
            }
        },

        ctxState(.dtd, .blank) => switch (ret_kind) {
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
        ctxState(.dtd, .whitespace) => switch (ret_kind) {
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
        ctxState(.dtd, .@"<") => switch (ret_kind) {
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
        ctxState(.dtd, .@"<!") => switch (ret_kind) {
            .type => {
                if (tokenizer.index == src.len) {
                    tokenizer.state = .eof;
                    break .angle_bracket_left_bang;
                }
                if (src[tokenizer.index] == '[') {
                    tokenizer.state = .blank;
                    tokenizer.index += 1;
                    break .angle_bracket_left_bang_square_bracket_left;
                }
                if (src[tokenizer.index] == '-') {
                    tokenizer.state = .@"<!-";
                    tokenizer.index += 1;
                    continue;
                }
                tokenizer.state = .blank;
                break .angle_bracket_left_bang;
            },
            .src => unreachable,
        },
        ctxState(.dtd, .@"<!-") => switch (ret_kind) {
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
        ctxState(.dtd, .dtd_subtag_start) => switch (ret_kind) {
            .type => unreachable,
            .src => {
                tokenizer.state = .blank; // fall through to the code for tokenizing tag_token
                break next_helper.literalInit(.@"<!");
            },
        },

        ctxState(.system_literal_quote_single, .blank),
        ctxState(.system_literal_quote_double, .blank),
        => |ctx_state| {
            const tag = ctx_state.bits().ctx;
            switch (ret_kind) {
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
            }
        },

        ctxState(.attribute_value_quote_single, .blank),
        ctxState(.attribute_value_quote_double, .blank),

        ctxState(.entity_value_quote_single, .blank),
        ctxState(.entity_value_quote_double, .blank),
        => |ctx_state| {
            const tag = ctx_state.bits().ctx;
            switch (ret_kind) {
                .type => {
                    if (tokenizer.index == src.len) {
                        tokenizer.state = .eof;
                        break .eof;
                    }

                    const matching_quote_char: u8, //
                    const matching_quote_type: TokenType //
                    = switch (tag) {
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
                        => if (src[tokenizer.index] == '<') {
                            tokenizer.index += 1;
                            break .angle_bracket_left;
                        },
                        .entity_value_quote_single,
                        .entity_value_quote_double,
                        => if (src[tokenizer.index] == '%') {
                            tokenizer.index += 1;
                            break .percent;
                        },
                        else => unreachable,
                    }
                    break .text_data;
                },
                .src => {
                    if (tokenizer.index == src.len) break null;
                    const matching_quote_char: u8 = switch (tag) {
                        .attribute_value_quote_single, .entity_value_quote_single => '\'',
                        .attribute_value_quote_double, .entity_value_quote_double => '\"',
                        else => unreachable,
                    };
                    const str_start = tokenizer.index;
                    const str_end = std.mem.indexOfAnyPos(u8, src, tokenizer.index, &[_]u8{ matching_quote_char, '&', '%', '<' }) orelse src.len;
                    tokenizer.index = str_end;
                    if (str_start == str_end) break null;
                    break next_helper.rangeInit(str_start, str_end);
                },
            }
        },
    } else error.BufferUnderrun;
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
                .type => Stream.BufferError!TokenType,
                .type_no_underrun => TokenType,
                .src => Stream.BufferError!?TokenSrc,
                .src_no_underrun => ?TokenSrc,
            };
        }
    };

    inline fn ctxState(ctx: Context, state: State) CtxState {
        return @enumFromInt(CtxState.ctxStateInt(ctx, state));
    }
    const CtxState = enum(CtxStateInt) {
        //! The fields of this enum are based off of the switch prongs in `nextTypeOrSrcImpl`,
        //! rather than the other way around. If/when that function is changed and prongs are
        //! added and/or removed, this should be updated to reflect those additions and/or removals.
        //!
        //! The field names are never used directly, rather all values of this type are computed
        //! via `ctxState`.
        ctx_state_00 = ctxStateInt(.non_markup, .eof),
        ctx_state_01 = ctxStateInt(.cdata, .eof),
        ctx_state_02 = ctxStateInt(.dtd, .eof),
        ctx_state_03 = ctxStateInt(.element_tag, .eof),
        ctx_state_04 = ctxStateInt(.comment, .eof),
        ctx_state_05 = ctxStateInt(.pi, .eof),
        ctx_state_06 = ctxStateInt(.reference, .eof),
        ctx_state_07 = ctxStateInt(.system_literal_quote_single, .eof),
        ctx_state_08 = ctxStateInt(.system_literal_quote_double, .eof),
        ctx_state_09 = ctxStateInt(.attribute_value_quote_single, .eof),
        ctx_state_10 = ctxStateInt(.attribute_value_quote_double, .eof),
        ctx_state_11 = ctxStateInt(.entity_value_quote_single, .eof),
        ctx_state_12 = ctxStateInt(.entity_value_quote_double, .eof),
        ctx_state_13 = ctxStateInt(.non_markup, .blank),
        ctx_state_14 = ctxStateInt(.non_markup, .@"<"),
        ctx_state_15 = ctxStateInt(.non_markup, .@"<!"),
        ctx_state_16 = ctxStateInt(.non_markup, .@"<!-"),
        ctx_state_17 = ctxStateInt(.non_markup, .@"<!["),
        ctx_state_18 = ctxStateInt(.non_markup, .@"<![C"),
        ctx_state_19 = ctxStateInt(.non_markup, .@"<![CD"),
        ctx_state_20 = ctxStateInt(.non_markup, .@"<![CDA"),
        ctx_state_21 = ctxStateInt(.non_markup, .@"<![CDAT"),
        ctx_state_22 = ctxStateInt(.non_markup, .@"<![CDATA"),
        ctx_state_23 = ctxStateInt(.non_markup, .@"<!D"),
        ctx_state_24 = ctxStateInt(.non_markup, .@"<!DO"),
        ctx_state_25 = ctxStateInt(.non_markup, .@"<!DOC"),
        ctx_state_26 = ctxStateInt(.non_markup, .@"<!DOCT"),
        ctx_state_27 = ctxStateInt(.non_markup, .@"<!DOCTY"),
        ctx_state_28 = ctxStateInt(.non_markup, .@"<!DOCTYP"),
        ctx_state_29 = ctxStateInt(.non_markup, .@"<!DOCTYPE"),
        ctx_state_30 = ctxStateInt(.non_markup, .angle_bracket_left_bang_invalid_tag_returned),
        ctx_state_31 = ctxStateInt(.non_markup, .@"]"),
        ctx_state_32 = ctxStateInt(.non_markup, .@"]]"),
        ctx_state_33 = ctxStateInt(.non_markup, .@"]]]"),
        ctx_state_34 = ctxStateInt(.cdata, .@"]"),
        ctx_state_35 = ctxStateInt(.cdata, .@"]]"),
        ctx_state_36 = ctxStateInt(.cdata, .@"]]]"),
        ctx_state_37 = ctxStateInt(.cdata, .blank),
        ctx_state_38 = ctxStateInt(.element_tag, .blank),
        ctx_state_39 = ctxStateInt(.element_tag, .whitespace),
        ctx_state_40 = ctxStateInt(.comment, .blank),
        ctx_state_41 = ctxStateInt(.comment, .@"-"),
        ctx_state_42 = ctxStateInt(.comment, .@"--"),
        ctx_state_43 = ctxStateInt(.comment, .@"---"),
        ctx_state_44 = ctxStateInt(.pi, .blank),
        ctx_state_45 = ctxStateInt(.pi, .@"?"),
        ctx_state_46 = ctxStateInt(.reference, .blank),
        ctx_state_47 = ctxStateInt(.dtd, .blank),
        ctx_state_48 = ctxStateInt(.dtd, .whitespace),
        ctx_state_49 = ctxStateInt(.dtd, .@"<"),
        ctx_state_50 = ctxStateInt(.dtd, .@"<!"),
        ctx_state_51 = ctxStateInt(.dtd, .@"<!-"),
        ctx_state_52 = ctxStateInt(.dtd, .dtd_subtag_start),
        ctx_state_53 = ctxStateInt(.system_literal_quote_single, .blank),
        ctx_state_54 = ctxStateInt(.system_literal_quote_double, .blank),
        ctx_state_55 = ctxStateInt(.attribute_value_quote_single, .blank),
        ctx_state_56 = ctxStateInt(.attribute_value_quote_double, .blank),
        ctx_state_57 = ctxStateInt(.entity_value_quote_single, .blank),
        ctx_state_58 = ctxStateInt(.entity_value_quote_double, .blank),

        const CtxStateBits = packed struct {
            ctx: Context,
            state: State,
        };
        inline fn bits(ctx_state: CtxState) CtxStateBits {
            return @bitCast(@intFromEnum(ctx_state));
        }

        const CtxStateInt = std.meta.Int(.unsigned, @bitSizeOf(CtxStateBits));

        inline fn ctxStateInt(ctx: Context, state: State) CtxStateInt {
            return @bitCast(CtxStateBits{ .ctx = ctx, .state = state });
        }
    };
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
    const combo = packed struct(u2) {
        a: bool,
        b: bool,
        inline fn combo(a: bool, b: bool) u2 {
            return @bitCast(@This(){ .a = a, .b = b });
        }
    }.combo;
    {
        var actual_src_buf = std.ArrayList(u8).init(std.testing.allocator);
        defer actual_src_buf.deinit();

        for (opts.start_size..opts.max_buffer orelse src.len) |buffer_size| {
            const helper = struct {
                fn getNextType(tokenizer: *Tokenizer.Stream, context: Context, feeder: *std.mem.WindowIterator(u8)) TokenType {
                    return tokenizer.nextType(context) catch while (true) {
                        if (feeder.next()) |input| {
                            tokenizer.feedInput(input);
                        } else {
                            tokenizer.feedEof();
                        }
                        break tokenizer.nextType(context) catch continue;
                    };
                }
            };

            var feeder = std.mem.window(u8, src, buffer_size, buffer_size);

            var tokenizer = Tokenizer.initStream();
            for (contexts_expected_tokens, 0..) |iteration_vals, i| {
                errdefer testingPrint("difference occured on token {d}\n", .{i});

                const context: Context, //
                const expected_tt: TokenType, //
                const expected_src: ?[]const u8 //
                = iteration_vals;
                if (expected_src != null) try std.testing.expect(expected_tt.hasSrc());

                const actual_tt: TokenType = helper.getNextType(&tokenizer.stream, context, &feeder);
                const actual_src: ?[]const u8 = blk: {
                    if (!actual_tt.hasSrc()) break :blk null;

                    actual_src_buf.clearRetainingCapacity();
                    while (true) {
                        const segment = (tokenizer.stream.nextSrc(context) catch while (true) {
                            if (feeder.next()) |input| {
                                tokenizer.stream.feedInput(input);
                            } else {
                                tokenizer.stream.feedEof();
                            }
                            break tokenizer.stream.nextSrc(context) catch continue;
                        }) orelse break;
                        try actual_src_buf.appendSlice(segment);
                    }

                    assert(actual_src_buf.items.len != 0);
                    break :blk actual_src_buf.items;
                };

                try std.testing.expectEqual(expected_tt, actual_tt);

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
            try std.testing.expectEqual(.eof, helper.getNextType(&tokenizer.stream, if (ctx_count != 0) contexts_expected_tokens[ctx_count - 1][0] else .non_markup, &feeder));
        }
    }

    var tokenizer = Tokenizer.initFull(src);
    for (contexts_expected_tokens, 0..) |iteration_vals, i| {
        errdefer testingPrint("difference occured on token {d}\n", .{i});

        const context: Context, //
        const expected_tt: TokenType, //
        const expected_src: ?[]const u8 //
        = iteration_vals;

        const actual_tt: TokenType = tokenizer.full.nextType(context);
        const actual_src: ?[]const u8 = blk: {
            if (!actual_tt.hasSrc()) break :blk null;
            const range = tokenizer.full.nextSrcComplete(context);
            break :blk range.toStr(tokenizer.src);
        };

        try std.testing.expectEqual(expected_tt, actual_tt);

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
    try std.testing.expectEqual(.eof, tokenizer.full.nextType(if (ctx_count != 0) contexts_expected_tokens[ctx_count - 1][0] else .non_markup));
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
    try testTokenizer(.{}, "lt", &.{ .{ .reference, .tag_token, "lt" }, .{ .reference, .eof, null } });
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
        .{ .dtd, .angle_bracket_left_bang, null },
        .{ .dtd, .tag_token, "ENTITY" },
        .{ .dtd, .angle_bracket_right, null },
        .{ .dtd, .tag_whitespace, " " },

        .{ .dtd, .angle_bracket_left_bang, null },
        .{ .dtd, .tag_token, "ATTLIST" },
        .{ .dtd, .angle_bracket_right, null },
        .{ .dtd, .tag_whitespace, " " },

        .{ .dtd, .angle_bracket_left_bang, null },
        .{ .dtd, .tag_token, "ELEMENT" },
        .{ .dtd, .angle_bracket_right, null },
        .{ .dtd, .tag_whitespace, " " },

        .{ .dtd, .angle_bracket_left_bang, null },
        .{ .dtd, .tag_token, "NOTATION" },
        .{ .dtd, .angle_bracket_right, null },
        .{ .dtd, .tag_whitespace, " " },

        .{ .dtd, .angle_bracket_left_bang, null },
        .{ .dtd, .tag_token, "EEEE" },
        .{ .dtd, .angle_bracket_right, null },
        .{ .dtd, .tag_whitespace, " " },

        .{ .dtd, .angle_bracket_right, null },
    });

    try testTokenizer(.{}, "<!ENTITYYY", &.{
        .{ .dtd, .angle_bracket_left_bang, null },
        .{ .dtd, .tag_token, "ENTITYYY" },
    });
    try testTokenizer(.{}, "()?*+|,%[]#", &.{
        .{ .dtd, .lparen, null },
        .{ .dtd, .rparen, null },
        .{ .dtd, .qmark, null },
        .{ .dtd, .asterisk, null },
        .{ .dtd, .plus, null },
        .{ .dtd, .pipe, null },
        .{ .dtd, .comma, null },
        .{ .dtd, .percent, null },
        .{ .dtd, .square_bracket_left, null },
        .{ .dtd, .square_bracket_right, null },
        .{ .dtd, .hashtag, null },
    });

    try testTokenizer(.{}, "<![ <![", &.{
        .{ .dtd, .angle_bracket_left_bang_square_bracket_left, null },
        .{ .dtd, .tag_whitespace, " " },
        .{ .dtd, .angle_bracket_left_bang_square_bracket_left, null },
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

const std = @import("std");
const assert = std.debug.assert;
const builtin = @import("builtin");
