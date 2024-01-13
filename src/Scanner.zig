const std = @import("std");
const assert = std.debug.assert;
const builtin = @import("builtin");

const Scanner = @This();
src: []const u8,
index: usize,
/// *Some* of the fields in this struct may be modified directly; read
/// the associated doc comments. This is for fairly niche use cases.
flags: Flags,
state: State,

/// The set of codepoints defined as whitespace. They are all
/// exactly one byte in size.
pub const whitespace_set: []const u8 = &[_]u8{
    '\u{20}',
    '\u{09}',
    '\u{0D}',
    '\u{0A}',
};

pub const InitOptions = struct {
    comment: Comment = .normal,
    doc_type: DocType = .{
        .recover_on_incomplete_token = false,
    },

    pub const Comment = enum {
        /// '--' doesn't terminate the comment, and '--->'
        /// terminates the comment text.
        normal,
        /// '--' will be assumed to end the comment.
        dash_dash_terminates,
        /// '--->' will be tokenized as '--', and
        /// then '->' as part of the comment text.
        ignore_end_triple_dash,
    };

    pub const DocType = packed struct {
        /// After receiving `nextType() = .invalid_token`, where `nextString()` returns
        /// an incomplete slice of '<!DOCTYPE' (starting from '<!D' up to '<!DOCTYP'),
        /// if this is true, it scanning will continue as though it was a complete token.
        recover_on_incomplete_token: bool,
    };
};

/// Initializes the `Scanner` with the full input.
/// Calling `feedInput` or `feedEof` is illegal.
pub inline fn initComplete(options: InitOptions, src: []const u8) Scanner {
    var scanner = Scanner.initStreaming(options);
    scanner.feedInput(src);
    scanner.feedEof();
    return scanner;
}

pub inline fn initStreaming(options: InitOptions) Scanner {
    return .{
        .src = "",
        .index = 0,
        .state = .text_data,
        .flags = .{
            .eof_specified = false,
            .comment = options.comment,
            .doc_type = options.doc_type,
        },
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
/// `nextString` after a call to `feedEof`.
pub fn feedInput(scanner: *Scanner, src: []const u8) void {
    assert(!scanner.flags.eof_specified);
    assert(scanner.index == scanner.src.len);
    scanner.src = src;
    scanner.index = 0;
}

/// Inform the scanner that the entirety of the XML source has been
/// supplied directly after a call to `feedInput`, or directly after
/// encountering `error.BufferUnderrun`.
/// Subsequent calls to this are illegal after the first call.
pub fn feedEof(scanner: *Scanner) void {
    assert(!scanner.flags.eof_specified);
    assert(scanner.index == 0 or scanner.index == scanner.src.len);
    scanner.flags.eof_specified = true;
}

pub const TokenType = enum {
    /// The end of the XML source.
    /// This is the last token that will appear.
    /// Terminates the token sequence.
    eof,

    /// Some form of invalid token.
    /// The invalid token string can be returned,
    invalid_token,

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

    /// The '>' token.
    ///
    /// Ends the token sequence.
    tag_basic_close,

    /// Indicates '=' in an element tag.
    attr_eql,

    // -- complex tokens --

    /// The '\'' token.
    ///
    /// The subsequent token sequence and the way in which it is tokenized will
    /// be described by whichever other token sequence makes use of this one.
    /// Either `.eof` or this same token always terminate the aforementioned token sequence.
    quote_single,
    /// The '\"' token.
    ///
    /// It is equivalent in usage and behaviour to `.quote_single`, save the quote character.
    quote_double,

    /// The '&' token.
    ///
    /// The subsequent token sequence will be one of:
    /// * `.tag_token`.
    /// * `.reference_end`.
    /// * `.reference_end_invalid`.
    reference,
    /// The '%' token.
    ///
    /// The subsequent token sequence will be the same as for `.reference`.
    pe_reference,
    /// The ';' token.
    ///
    /// Ends the token sequence.
    reference_end,
    /// Encountered a terminating character (or EOF) other than the ';' token after the ampersand.
    /// This token simply represents the absence of the ';' token.
    /// Ends the token sequence.
    reference_end_invalid,

    /// The '<?' token.
    ////
    /// The subsequent token sequence will be:
    /// * `.text_data`.
    /// * `.pi_end` ending the token sequence.
    /// * `.eof`.
    pi,
    /// The '?>' token, terminating the PI tag.
    ///
    /// Ends the token sequence.
    pi_end,

    /// The '<!--' token.
    ///
    /// The subsequent token sequence will be one of:
    /// * `.text_data`.
    /// * `.invalid_comment_dash_dash`.
    /// * `.invalid_comment_end_triple_dash`.
    /// * `.comment_end`.
    /// * `.eof`.
    comment,
    /// The invalid token '--'.
    ///
    /// See `InitOptions.Comment.dash_dash_terminates`.
    invalid_comment_dash_dash,
    /// The invalid token '--->'.
    ///
    /// See `InitOptions.Comment.ignore_end_triple_dash`.
    invalid_comment_end_triple_dash,
    /// Indicates '-->' after a comment.
    /// Ends the token sequence.
    comment_end,

    /// The '<' token.
    ///
    /// If outside of any other markup, it starts an element open tag, and
    /// the subsequent token sequence will be one of:
    /// * `.tag_token`.
    /// * `.tag_whitespace`.
    /// * `.attr_eql`.
    /// * `.quote_single` where the subsequent token sequence will be one of the following:
    ///    + `.text_data`.
    ///    + `.reference` followed by a token sequence as described in its own documentation.
    ///    + `.element_open` as an invalid token ('<' in the attribute value).
    ///    + `.quote_single` ending the token sequence.
    ///    + `.eof`.
    /// * `.quote_double` following sequence is equivalent as for `.quote_single`, replacing it with `.quote_double`.
    /// * `.tag_basic_close`.
    /// * `.tag_slash_close`.
    /// * `.eof`.
    element_open,
    /// The '</' token.
    ///
    /// Starts an element close tag.
    /// The subsequent token sequence will be the same as for `.element_open` in
    /// the right context.
    element_close,
    /// The '/>' token.
    /// Ends the token sequence.
    tag_slash_close,

    /// The '<![CDATA[' token.
    ///
    /// Starts a CDATA Section.
    /// The subsequent token sequence will be one of:
    /// * `text_data`.
    /// * `.cdata_end` ending the token sequence.
    /// * `.eof`.
    cdata,
    /// The ']]>' token.
    ///
    /// Ends the CDATA Section, or is an invalid if not preceeded by a matching `.data` token.
    cdata_end,

    /// The '<!DOCTYPE' token.
    ///
    /// Starts the DTD declaration.
    /// The subsequent token sequence will be one of:
    /// * `.tag_token`.
    /// * `.tag_whitespace`.
    /// * `.quote_single`, where the subsequent token sequence will be one of the following:
    ///    + `.text_data`.
    ///    + `.quote_single` ending the token sequence.
    ///    + `.eof`.
    /// * `.quote_double` following sequence is equivalent as for `.quote_single`, replacing it with `.quote_double`.
    /// * `.dtd_int_subset` followed by a token sequence as described in its own documentation.
    /// * `.tag_basic_close`.
    /// * `.eof`.
    dtd,
    /// The '[' token.
    ///
    /// Starts the DTD Internal Subset.
    /// The subsequent token sequence will be one of:
    /// * `.tag_whitespace`.
    /// * `.pe_reference` followed by a token sequence as described in its own documentation..
    /// * `.dtd_int_subset_element` followed by a token sequence as described in its own documentation.
    /// * `.dtd_int_subset_entity` followed by a token sequence as described in its own documentation.
    /// * `.dtd_int_subset_attlist` followed by a token sequence as described in its own documentation.
    /// * `.dtd_int_subset_end`.
    /// * `.eof`.
    dtd_int_subset,
    /// The '<!ELEMENT' token.
    ///
    /// The subsequent token sequence will be one of:
    /// TODO: Document
    dtd_int_subset_element,
    /// The '<!ENTITY' token.
    ///
    /// The subsequent token sequence will be one of:
    /// TODO: Document
    dtd_int_subset_entity,
    /// The '<!ATTLIST' token.
    ///
    /// The subsequent token sequence will be one of:
    /// TODO: Document
    dtd_int_subset_attlist,
    /// The ']' token.
    ///
    /// Ends DTD Internal Subset, or is an invalid token if not preceeded by a matching `.dtd_int_subset` token.
    dtd_int_subset_end,

    /// Whether or not this token should be followed up by a call to `nextSrc`/`nextString`.
    pub inline fn hasString(token_type: TokenType) bool {
        return switch (token_type) {
            .eof => false,

            .invalid_token => true,

            .text_data,
            .tag_whitespace,
            .tag_token,
            => true,

            .tag_basic_close => false,

            .attr_eql => false,

            .quote_single,
            .quote_double,
            => false,

            .reference,
            .pe_reference,
            .reference_end,
            .reference_end_invalid,
            => false,

            .pi, .pi_end => false,

            .comment,
            .invalid_comment_dash_dash,
            .invalid_comment_end_triple_dash,
            .comment_end,
            => false,

            .element_open,
            .element_close,
            .tag_slash_close,
            => false,

            .cdata, .cdata_end => false,

            .dtd,
            .dtd_int_subset,
            .dtd_int_subset_element,
            .dtd_int_subset_entity,
            .dtd_int_subset_attlist,
            .dtd_int_subset_end,
            => false,
        };
    }
};

pub const BufferError = error{BufferUnderrun};

pub const NextTypeError = BufferError;
/// Returns type of the next token. There are two possibilities:
/// * The token type represents a builtin syntactic construct for which
///   there is no further textual information to retrieve.
///   In this case, the caller should simply call `nextType` again to
///   obtain the following token type.
///
/// * The token indicates some textual information which isn't a builtin
///   syntactic construct, like identifiers in markup and textual data.
///   In this case, the caller should call `nextSrc` until it returns null.
///   The caller may interchangeably use `nextString` and `nextSrc`.
pub fn nextType(scanner: *Scanner) NextTypeError!TokenType {
    // TODO: Use `@call(.always_tail, nextType, .{scanner})` in this function once that
    // works for this return type.
    const src = scanner.src;
    const flags = scanner.flags;
    switch (scanner.state) {
        .text_data => {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                return .eof;
            }
            switch (src[scanner.index]) {
                ']' => {
                    scanner.state = .@"text_data,]";
                    scanner.index += 1;
                    // return @call(.always_tail, nextType, .{scanner});
                    return scanner.nextType();
                },
                '&' => {
                    scanner.state = .@"text_data,&";
                    scanner.index += 1;
                    return .reference;
                },
                '<' => {
                    scanner.state = .@"text_data,<";
                    scanner.index += 1;
                    // return @call(.always_tail, nextType, .{scanner});
                    return scanner.nextType();
                },
                else => return .text_data,
            }
        },
        .@"text_data,]" => {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                return .text_data;
            }
            if (src[scanner.index] != ']') {
                return .text_data;
            }
            scanner.state = .@"text_data,]]";
            scanner.index += 1;
            // return @call(.always_tail, nextType, .{scanner});
            return scanner.nextType();
        },
        .@"text_data,]]" => {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                return .text_data;
            }
            if (src[scanner.index] != '>') {
                return .text_data;
            }
            scanner.state = .text_data;
            scanner.index += 1;
            return .cdata_end;
        },
        .@"text_data,&" => {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                scanner.state = .text_data;
                return .reference_end_invalid;
            }
            if (src[scanner.index] == ';') {
                scanner.state = .text_data;
                scanner.index += 1;
                return .reference_end;
            }
            if (std.mem.indexOfScalar(u8, whitespace_set ++ &[_]u8{ ']', '&', '<' }, src[scanner.index]) != null) {
                scanner.state = .text_data;
                return .reference_end_invalid;
            }
            return .tag_token;
        },
        .@"text_data,<" => {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                scanner.state = .text_data;
                return .element_open;
            }
            switch (src[scanner.index]) {
                '?' => {
                    scanner.index += 1;
                    scanner.state = .pi;
                    return .pi;
                },
                '/' => {
                    scanner.state = .element_tag;
                    scanner.index += 1;
                    return .element_close;
                },
                '!' => {
                    scanner.state = .@"text_data,<!";
                    scanner.index += 1;
                    // return @call(.always_tail, nextType, .{scanner});
                    return scanner.nextType();
                },
                else => {
                    scanner.state = .element_tag;
                    return .element_open;
                },
            }
        },
        .@"text_data,<!" => {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            switch (src[scanner.index]) {
                '-' => {
                    scanner.state = .@"text_data,<!-";
                    scanner.index += 1;
                    // return @call(.always_tail, nextType, .{scanner});
                    return scanner.nextType();
                },
                'D' => {
                    scanner.state = .@"<!D";
                    scanner.index += 1;
                    // return @call(.always_tail, nextType, .{scanner});
                    return scanner.nextType();
                },
                '[' => @panic("TODO"),
                else => return .invalid_token,
            }
        },
        .@"text_data,<!-" => {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            if (src[scanner.index] != '-') {
                return .invalid_token;
            }
            scanner.state = .comment;
            scanner.index += 1;
            return .comment;
        },

        .pi => {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                return .eof;
            }
            if (src[scanner.index] != '?') {
                return .text_data;
            }
            scanner.state = .@"pi,?";
            scanner.index += 1;
            // return @call(.always_tail, nextType, .{scanner});
            return scanner.nextType();
        },
        .@"pi,?" => {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                return .text_data;
            }
            if (src[scanner.index] != '>') {
                return .text_data;
            }
            scanner.state = .text_data;
            scanner.index += 1;
            return .pi_end;
        },

        .element_tag => {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                scanner.state = .text_data;
                return .eof;
            }
            switch (src[scanner.index]) {
                '>' => {
                    scanner.state = .text_data;
                    scanner.index += 1;
                    return .tag_basic_close;
                },
                '\'' => {
                    scanner.state = .element_tag_sq;
                    scanner.index += 1;
                    return .quote_single;
                },
                '\"' => {
                    scanner.state = .element_tag_dq;
                    scanner.index += 1;
                    return .quote_double;
                },
                '/' => {
                    scanner.state = .@"element_tag,/";
                    scanner.index += 1;
                    // return @call(.always_tail, nextType, .{scanner});
                    return scanner.nextType();
                },
                '=' => {
                    scanner.index += 1;
                    return .attr_eql;
                },
                else => |char| {
                    const not_whitespace = std.mem.indexOfScalar(u8, whitespace_set, char) == null;
                    if (not_whitespace) return .tag_token;
                    scanner.state = .@"element_tag,whitespace";
                    return .tag_whitespace;
                },
            }
        },
        .@"element_tag,whitespace" => unreachable,
        .@"element_tag,/" => {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                return .tag_token;
            }
            if (src[scanner.index] != '>') {
                return .tag_token;
            }
            scanner.state = .text_data;
            scanner.index += 1;
            return .tag_slash_close;
        },
        inline //
        .element_tag_sq,
        .element_tag_dq,
        => |tag| {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                return .eof;
            }
            const matching_quote_char: u8, //
            const matching_type: TokenType, //
            const on_ref_state: State //
            = comptime switch (tag) {
                .element_tag_sq => .{ '\'', .quote_single, .@"element_tag_sq,&" },
                .element_tag_dq => .{ '\"', .quote_double, .@"element_tag_dq,&" },
                else => unreachable,
            };
            switch (src[scanner.index]) {
                matching_quote_char => {
                    scanner.state = .element_tag;
                    scanner.index += 1;
                    return matching_type;
                },
                '&' => {
                    scanner.state = on_ref_state;
                    scanner.index += 1;
                    return .reference;
                },
                '<' => {
                    scanner.index += 1;
                    return .element_open;
                },
                else => return .text_data,
            }
        },
        inline //
        .@"element_tag_sq,&",
        .@"element_tag_dq,&",
        => |tag| {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                return .eof;
            }
            const matching_quote_char: u8, //
            const matching_state: State //
            = comptime switch (tag) {
                .@"element_tag_sq,&" => .{ '\'', .element_tag_sq },
                .@"element_tag_dq,&" => .{ '\"', .element_tag_dq },
                else => unreachable,
            };
            switch (src[scanner.index]) {
                matching_quote_char => {
                    scanner.state = matching_state;
                    return .reference_end_invalid;
                },
                ';' => {
                    scanner.state = matching_state;
                    scanner.index += 1;
                    return .reference_end;
                },
                else => return .tag_token,
            }
        },

        .comment => {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                scanner.state = .text_data;
                return .eof;
            }
            if (src[scanner.index] == '-') {
                scanner.state = .@"comment,-";
                scanner.index += 1;
                // return @call(.always_tail, nextType, .{scanner});
                return scanner.nextType();
            }
            return .text_data;
        },
        .@"comment,-" => {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                return .text_data;
            }
            if (src[scanner.index] != '-') {
                return .text_data;
            }
            switch (flags.comment) {
                .normal => {
                    scanner.state = .@"comment,--";
                    scanner.index += 1;
                    // return @call(.always_tail, nextType, .{scanner});
                    return scanner.nextType();
                },
                .dash_dash_terminates => {
                    scanner.state = .text_data;
                    scanner.index += 1;
                    return .invalid_comment_dash_dash;
                },
                .ignore_end_triple_dash => {
                    scanner.state = .comment;
                    scanner.index += 1;
                    return .invalid_comment_dash_dash;
                },
            }
        },
        .@"comment,--" => {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                scanner.state = .text_data;
                return .invalid_comment_dash_dash;
            }
            switch (src[scanner.index]) {
                '-' => {
                    scanner.state = .@"comment,---";
                    scanner.index += 1;
                    // return @call(.always_tail, nextType, .{scanner});
                    return scanner.nextType();
                },
                '>' => {
                    scanner.state = .text_data;
                    scanner.index += 1;
                    return .comment_end;
                },
                else => {
                    scanner.state = .comment;
                    return .invalid_comment_dash_dash;
                },
            }
        },
        .@"comment,---" => {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                scanner.state = .@"comment,-";
                return .invalid_comment_dash_dash;
            }
            switch (src[scanner.index]) {
                '-' => {
                    scanner.state = .@"comment,--";
                    scanner.index += 1;
                    return .invalid_comment_dash_dash;
                },
                '>' => {
                    scanner.state = .text_data;
                    scanner.index += 1;
                    return .invalid_comment_end_triple_dash;
                },
                else => {
                    scanner.state = .@"comment,-";
                    return .invalid_comment_dash_dash;
                },
            }
        },

        inline //
        .@"<!D",
        .@"<!DO",
        .@"<!DOC",
        .@"<!DOCT",
        .@"<!DOCTY",
        => |tag| {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            const expected_char: u8, const state_on_match = comptime switch (tag) {
                .@"<!D" => .{ 'O', .@"<!DO" },
                .@"<!DO" => .{ 'C', .@"<!DOC" },
                .@"<!DOC" => .{ 'T', .@"<!DOCT" },
                .@"<!DOCT" => .{ 'Y', .@"<!DOCTY" },
                .@"<!DOCTY" => .{ 'P', .@"<!DOCTYP" },
                else => unreachable,
            };
            if (src[scanner.index] != expected_char) {
                return .invalid_token;
            }
            scanner.state = state_on_match;
            scanner.index += 1;
            // return @call(.always_tail, nextType, .{scanner});
            return scanner.nextType();
        },
        .@"<!DOCTYP" => {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            if (src[scanner.index] != 'E') {
                return .invalid_token;
            }
            scanner.state = .dtd;
            scanner.index += 1;
            return .dtd;
        },
        .dtd => @panic("TODO"),
    }
}

pub const NextStringError = NextSrcError;
/// Helper function for returning the result of `nextSrc` directly as a string.
/// The returned string is either a string literal or a reference to `scanner.src`,
/// meaning it is only guaranteed to remain valid for as long as `scanner.src` is.
/// All other behaviour remains equivalent to `nextSrc`.
pub inline fn nextString(scanner: *Scanner) NextStringError!?[]const u8 {
    const maybe_tok_src = try scanner.nextSrc();
    return switch (maybe_tok_src orelse return null) {
        .range => |range| scanner.src[range.start..range.end],
        .literal => |literal| literal.toStr(),
    };
}

pub const NextSrcError = BufferError;
/// The returned token reference is either a range referencing the current `scanner.src` value,
/// which will remain valid for as long as the referenced slice is valid and accessible.
/// It should be called repeatedly until it returns `null`. If `error.BufferUnderrun` is returned,
/// the caller should invoke `feedInput`, and then proceed.
pub fn nextSrc(scanner: *Scanner) NextSrcError!?TokenSrc {
    // TODO: Make calling this when it's already returned null be checked illegal behaviour.
    // TODO: Use `@call(.always_tail, nextSrc, .{scanner})` in this function once that
    // works for this return type.

    const helper = struct {
        inline fn rangeInit(range_start: usize, range_end: usize) TokenSrc {
            return .{ .range = .{ .start = range_start, .end = range_end } };
        }
        inline fn literalInit(literal: TokenSrc.Literal) TokenSrc {
            return .{ .literal = literal };
        }
    };

    const src = scanner.src;
    const flags = scanner.flags;
    switch (scanner.state) {
        .text_data => {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                return null;
            }
            const str_start = scanner.index;
            const str_end = std.mem.indexOfAnyPos(u8, src, scanner.index, &[_]u8{ ']', '&', '<' }) orelse src.len;
            scanner.index = str_end;
            if (str_start != str_end) {
                return helper.rangeInit(str_start, str_end);
            }
            switch (src[scanner.index]) {
                ']' => {
                    scanner.state = .@"text_data,]";
                    scanner.index += 1;
                    // return @call(.always_tail, nextSrc, .{scanner});
                    return scanner.nextSrc();
                },
                '&', '<' => {},
                else => unreachable,
            }
            return null;
        },
        .@"text_data,]" => {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                scanner.state = .text_data;
                return helper.literalInit(.@"]");
            }
            if (src[scanner.index] != ']') {
                scanner.state = .text_data;
                return helper.literalInit(.@"]");
            }
            scanner.state = .@"text_data,]]";
            scanner.index += 1;
            // return @call(.always_tail, nextSrc, .{scanner});
            return scanner.nextSrc();
        },
        .@"text_data,]]" => {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                scanner.state = .text_data;
                return helper.literalInit(.@"]]");
            }
            if (src[scanner.index] != '>') {
                scanner.state = .text_data;
                return helper.literalInit(.@"]]");
            }
            return null;
        },
        .@"text_data,&" => {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                return null;
            }
            const str_start = scanner.index;
            const str_end = std.mem.indexOfAnyPos(u8, src, scanner.index, whitespace_set ++ &[_]u8{ ';', ']', '&', '<' }) orelse src.len;
            scanner.index = str_end;
            if (str_start != scanner.index) {
                return helper.rangeInit(str_start, scanner.index);
            }
            return null;
        },
        .@"text_data,<" => unreachable,

        .pi => {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                return null;
            }

            const str_start = scanner.index;
            const str_end = std.mem.indexOfScalarPos(u8, src, scanner.index, '?') orelse src.len;
            scanner.index = str_end;
            if (str_start != str_end) {
                return helper.rangeInit(str_start, str_end);
            }
            scanner.state = .@"pi,?";
            scanner.index += 1;
            // return @call(.always_tail, nextSrc, .{scanner});
            return scanner.nextSrc();
        },
        .@"pi,?" => {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                scanner.state = .pi;
                return helper.literalInit(.@"?");
            }
            if (src[scanner.index] != '>') {
                scanner.state = .pi;
                return helper.literalInit(.@"?");
            }
            return null;
        },

        .element_tag => {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                return null;
            }
            const str_start = scanner.index;
            const str_end = std.mem.indexOfAnyPos(u8, src, scanner.index, whitespace_set ++ &[_]u8{ '>', '\'', '\"', '/', '=' }) orelse src.len;
            scanner.index = str_end;
            if (str_start != str_end) {
                return helper.rangeInit(str_start, str_end);
            }
            return null;
        },
        .@"element_tag,whitespace" => {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                return null;
            }
            const str_start = scanner.index;
            const str_end = std.mem.indexOfNonePos(u8, src, scanner.index, whitespace_set) orelse src.len;
            scanner.index = str_end;
            if (str_start != str_end) {
                return helper.rangeInit(str_start, str_end);
            }
            scanner.state = .element_tag;
            return null;
        },
        .@"element_tag,/" => {
            scanner.state = .element_tag;
            return helper.literalInit(.@"/");
        },
        inline //
        .element_tag_sq,
        .element_tag_dq,
        => |tag| {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                return null;
            }
            const matching_quote_char: u8 = comptime switch (tag) {
                .element_tag_sq => '\'',
                .element_tag_dq => '\"',
                else => unreachable,
            };
            const str_start = scanner.index;
            const str_end = std.mem.indexOfAnyPos(u8, src, scanner.index, &[_]u8{ matching_quote_char, '&', '<' }) orelse src.len;
            scanner.index = str_end;
            if (str_start != str_end) {
                return helper.rangeInit(str_start, str_end);
            }
            return null;
        },
        inline //
        .@"element_tag_sq,&",
        .@"element_tag_dq,&",
        => |tag| {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                return null;
            }
            const matching_quote_char: u8 = comptime switch (tag) {
                .@"element_tag_sq,&" => '\'',
                .@"element_tag_dq,&" => '\"',
                else => unreachable,
            };
            const str_start = scanner.index;
            const str_end = std.mem.indexOfAnyPos(u8, src, scanner.index, whitespace_set ++ &[_]u8{ ';', matching_quote_char, '&', '<' }) orelse src.len;
            scanner.index = str_end;
            if (str_start != str_end) {
                return helper.rangeInit(str_start, str_end);
            }
            return null;
        },

        .comment => {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                return null;
            }
            const str_start = scanner.index;
            const str_end = std.mem.indexOfScalarPos(u8, src, scanner.index, '-') orelse src.len;
            scanner.index = str_end;
            if (str_start != str_end) {
                return helper.rangeInit(str_start, str_end);
            }
            if (src[scanner.index] == '-') {
                scanner.state = .@"comment,-";
                scanner.index += 1;
                // return @call(.always_tail, nextSrc, .{scanner});
                return scanner.nextSrc();
            }
            return null;
        },
        .@"comment,-" => {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                scanner.state = .comment;
                return helper.literalInit(.@"-");
            }
            if (src[scanner.index] != '-') {
                scanner.state = .comment;
                return helper.literalInit(.@"-");
            }
            scanner.state = .@"comment,--";
            scanner.index += 1;
            // return @call(.always_tail, nextSrc, .{scanner});
            return scanner.nextSrc();
        },
        .@"comment,--" => {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                return null;
            }
            switch (src[scanner.index]) {
                '-' => {
                    scanner.state = .@"comment,---";
                    scanner.index += 1;
                    return null;
                },
                else => return null,
            }
        },
        .@"comment,---" => unreachable,

        .dtd => @panic("TODO"),

        // invalid tokens

        inline //
        .@"text_data,<!",
        .@"text_data,<!-",
        => |tag| {
            comptime assert(std.mem.startsWith(u8, @tagName(tag), "text_data,"));
            return helper.literalInit(@field(TokenSrc.Literal, @tagName(tag)["text_data,".len..]));
        },

        .@"<!D",
        .@"<!DO",
        .@"<!DOC",
        .@"<!DOCT",
        .@"<!DOCTY",
        .@"<!DOCTYP",
        => |tag| {
            if (scanner.index == src.len) {
                if (!flags.eof_specified) return error.BufferUnderrun;
                return null;
            }
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
    }
}

pub const TokenSrc = union(enum) {
    range: Range,
    literal: Literal,

    /// Refers to a range in the `src` field of the Scanner.
    pub const Range = struct {
        start: usize,
        end: usize,

        pub inline fn getStr(tok_range: Range, scanner: *const Scanner) ?[]const u8 {
            if (tok_range.isSentinel()) return null;
            return scanner.src[tok_range.start..tok_range.end];
        }
    };

    /// Refers to a string literal which was possibly recognized only after a call to `feedInput`,
    /// making it impossible to return a range referring to it in the Scanner's current `src` field.
    pub const Literal = enum {
        @"]",
        @"]]",
        @"/",
        @"?",
        @"<!",
        @"<!-",
        @"<!D",
        @"<!DO",
        @"<!DOC",
        @"<!DOCT",
        @"<!DOCTY",
        @"<!DOCTYP",
        @"-",

        pub inline fn toStr(literal_tok: Literal) []const u8 {
            return @tagName(literal_tok);
        }

        /// If the call to `nextType` just prior, and the calls to `nextSrc`/`nextString` never
        /// returned `error.BufferUnderrun`, the caller may use this function to turn the
        /// literal into a range immediately after receiving it.
        pub inline fn toRange(literal_tok: Literal, scanner: *const Scanner) Range {
            const len = literal_tok.toStr().len;
            return scanner.src[scanner.index - len ..][0..len];
        }
    };
};

const Flags = packed struct {
    /// Do not be modify directly. See `feedEof`.
    eof_specified: bool,

    /// May be modified before the next call to `nextType` or
    /// after the call to `nextString`/`nextSrc` which returns null.
    comment: InitOptions.Comment,

    /// May be modified before the next call to `nextType` or
    /// after the call to `nextString`/`nextSrc` which returns null.
    doc_type: InitOptions.DocType,
};

const State = enum {
    text_data,
    @"text_data,]",
    @"text_data,]]",
    @"text_data,&",
    @"text_data,<",
    @"text_data,<!",
    @"text_data,<!-",

    pi,
    @"pi,?",

    element_tag,
    @"element_tag,whitespace",
    @"element_tag,/",
    element_tag_sq,
    element_tag_dq,
    @"element_tag_sq,&",
    @"element_tag_dq,&",

    comment,
    @"comment,-",
    @"comment,--",
    @"comment,---",

    @"<!D",
    @"<!DO",
    @"<!DOC",
    @"<!DOCT",
    @"<!DOCTY",
    @"<!DOCTYP",
    dtd,
};

fn testingPrint(comptime fmt_str: []const u8, args: anytype) void {
    if (@inComptime()) {
        @compileError(std.fmt.comptimePrint(fmt_str, args));
    } else if (std.testing.backend_can_print) {
        std.debug.print(fmt_str, args);
    }
}

fn expectNextType(scanner: *Scanner, expected: NextTypeError!TokenType) !void {
    const actual = scanner.nextType();
    try std.testing.expectEqual(expected, actual);
}
fn expectNextString(scanner: *Scanner, expected: NextSrcError!?[]const u8) !void {
    const actual = scanner.nextString();

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
fn expectNextStringSeq(scanner: *Scanner, expected_list: []const NextSrcError!?[]const u8) !void {
    for (expected_list) |expected| try scanner.expectNextString(expected);
}
fn expectNextTypeStringSeq(scanner: *Scanner, expected_tt: TokenType, expected_list: []const NextSrcError!?[]const u8) !void {
    try scanner.expectNextType(expected_tt);
    try std.testing.expect(expected_tt.hasString());
    try scanner.expectNextStringSeq(expected_list);
}

test "Scanner Invalid Markup ('<'/'<![CDATA['/'<!DOCTYPE')" {
    if (true) return error.SkipZigTest;
    var scanner: Scanner = undefined;

    for ([_][]const u8{
        "<!",
        "<!-",
        "<![",
        "<![C",
        "<![CD",
        "<![CDA",
        "<![CDAT",
        "<![CDATA",
        "<!D",
        "<!DO",
        "<!DOC",
        "<!DOCT",
        "<!DOCTY",
        "<!DOCTYP",
    }) |incomplete_mk_str| {
        scanner = Scanner.initComplete(.{}, incomplete_mk_str);
        try scanner.expectNextType(.invalid_token);
        try scanner.expectNextString(incomplete_mk_str);
        try scanner.expectNextString(null);
        try scanner.expectNextType(.eof);

        scanner = Scanner.initStreaming(.{});
        for (0..incomplete_mk_str.len) |i| {
            scanner.feedInput(incomplete_mk_str[i..][0..1]);
            try scanner.expectNextType(error.BufferUnderrun);
        }

        var eof_copy = scanner;
        eof_copy.feedEof();
        try eof_copy.expectNextType(.invalid_token);
        try eof_copy.expectNextString(incomplete_mk_str);
        try eof_copy.expectNextString(null);
        try eof_copy.expectNextType(.eof);

        scanner.feedInput("a");
        scanner.feedEof();
        try scanner.expectNextType(.invalid_token);
        try scanner.expectNextString(incomplete_mk_str);
        try scanner.expectNextString(null);
        try scanner.expectNextType(.text_data);
        try scanner.expectNextString("a");
        try scanner.expectNextString(null);
        try scanner.expectNextType(.eof);
    }

    scanner = Scanner.initComplete(.{}, "<![CDATAR a");
    try scanner.expectNextType(.invalid_token);
    try scanner.expectNextString("<![CDATA");
    try scanner.expectNextString(null);
    try scanner.expectNextType(.text_data);
    try scanner.expectNextString("R a");
    try scanner.expectNextString(null);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "<!DOCTYPR a");
    try scanner.expectNextType(.invalid_token);
    try scanner.expectNextString("<!DOCTYP");
    try scanner.expectNextString(null);
    try scanner.expectNextType(.text_data);
    try scanner.expectNextString("R a");
    try scanner.expectNextString(null);
    try scanner.expectNextType(.eof);
}

test "Scanner Processing Instructions" {
    var scanner: Scanner = undefined;

    scanner = Scanner.initComplete(.{}, "<??>");
    try scanner.expectNextType(.pi);
    try scanner.expectNextType(.pi_end);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "<?");
    try scanner.expectNextType(.pi);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "<? ?>");
    try scanner.expectNextType(.pi);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ " ", null });
    try scanner.expectNextType(.pi_end);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "<?foo?>");
    try scanner.expectNextType(.pi);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "foo", null });
    try scanner.expectNextType(.pi_end);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "<?foo ?>");
    try scanner.expectNextType(.pi);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "foo ", null });
    try scanner.expectNextType(.pi_end);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "<?foo bar?>");
    try scanner.expectNextType(.pi);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "foo bar", null });
    try scanner.expectNextType(.pi_end);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "<?" ++ "???" ++ "?>");
    try scanner.expectNextType(.pi);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "?", "?", "?", null });
    try scanner.expectNextType(.pi_end);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initStreaming(.{});
    scanner.feedInput("<?foo");
    try scanner.expectNextType(.pi);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "foo", error.BufferUnderrun });
    scanner.feedInput("?>");
    try scanner.expectNextString(null);
    try scanner.expectNextType(.pi_end);
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextType(.eof);

    scanner = Scanner.initStreaming(.{});
    scanner.feedInput("<?foo");
    try scanner.expectNextType(.pi);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "foo", error.BufferUnderrun });
    scanner.feedInput("?");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(">");
    try scanner.expectNextString(null);
    try scanner.expectNextType(.pi_end);
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextType(.eof);

    scanner = Scanner.initStreaming(.{});
    scanner.feedInput("<?");
    try scanner.expectNextType(.pi);
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedInput("fizz?>");
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "fizz", null });
    try scanner.expectNextType(.pi_end);
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextType(.eof);

    scanner = Scanner.initStreaming(.{});
    scanner.feedInput("<?bar");
    try scanner.expectNextType(.pi);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "bar", error.BufferUnderrun });
    scanner.feedInput("?");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("?");
    try scanner.expectNextStringSeq(&.{ "?", error.BufferUnderrun });
    scanner.feedInput("baz");
    try scanner.expectNextStringSeq(&.{ "?", "baz", error.BufferUnderrun });
    scanner.feedInput("?>");
    try scanner.expectNextString(null);
    try scanner.expectNextType(.pi_end);
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextType(.eof);

    scanner = Scanner.initStreaming(.{});
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedInput("<");
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedInput("?");
    try scanner.expectNextType(.pi);
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedInput("fo");
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "fo", error.BufferUnderrun });
    scanner.feedInput("o?");
    try scanner.expectNextStringSeq(&.{ "o", error.BufferUnderrun });
    scanner.feedInput("bar");
    try scanner.expectNextStringSeq(&.{ "?", "bar", error.BufferUnderrun });
    scanner.feedInput(" ");
    try scanner.expectNextStringSeq(&.{ " ", error.BufferUnderrun });
    scanner.feedInput("?");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(" ");
    try scanner.expectNextStringSeq(&.{ "?", " ", error.BufferUnderrun });
    scanner.feedInput("?");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(">");
    try scanner.expectNextString(null);
    try scanner.expectNextType(.pi_end);
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextType(.eof);
}

test "Scanner Comments" {
    var scanner: Scanner = undefined;

    scanner = Scanner.initComplete(.{}, "<!--" ++ "-->");
    try scanner.expectNextType(.comment);
    try scanner.expectNextString(null);
    try scanner.expectNextType(.comment_end);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "<!--" ++ "-" ++ "-->");
    try scanner.expectNextType(.comment);
    try scanner.expectNextString(null);
    try scanner.expectNextType(.invalid_comment_end_triple_dash);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "<!--" ++ "--" ++ "-->");
    try scanner.expectNextType(.comment);
    try scanner.expectNextType(.invalid_comment_dash_dash);
    try scanner.expectNextType(.comment_end);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "<!--" ++ "--" ++ "-" ++ "-->");
    try scanner.expectNextType(.comment);
    try scanner.expectNextType(.invalid_comment_dash_dash);
    try scanner.expectNextType(.invalid_comment_end_triple_dash);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "<!-- <foo bar> -->");
    try scanner.expectNextType(.comment);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ " <foo bar> ", null });
    try scanner.expectNextType(.comment_end);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initStreaming(.{});
    scanner.feedInput("<!--");
    try scanner.expectNextType(.comment);
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedInput("--");
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedInput(">");
    try scanner.expectNextType(.comment_end);
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextType(.eof);

    scanner = Scanner.initStreaming(.{});
    scanner.feedInput("<");
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedInput("!");
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedInput("-");
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedInput("-");
    try scanner.expectNextType(.comment);
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedInput("-");
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedInput("-");
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedInput(">");
    try scanner.expectNextType(.comment_end);
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextType(.eof);

    scanner = Scanner.initStreaming(.{});
    scanner.feedInput("<!--");
    try scanner.expectNextType(.comment);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("--a");
    try scanner.expectNextType(.invalid_comment_dash_dash);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "a", error.BufferUnderrun });
    scanner.feedInput("-->");
    try scanner.expectNextString(null);
    try scanner.expectNextType(.comment_end);
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextType(.eof);
}

test "Scanner CDATA Sections" {
    if (true) return error.SkipZigTest;
    var scanner: Scanner = undefined;

    scanner = Scanner.initComplete(.{}, "<![CDATA[" ++ "]]>");
    try scanner.expectNextType(.cdata);
    try scanner.expectNextString(null);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "<![CDATA[" ++ " foo " ++ "]]>");
    try scanner.expectNextType(.cdata);
    try scanner.expectNextStringSeq(&.{ " foo ", null });
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "<![CDATA[" ++ "]]" ++ "]]>");
    try scanner.expectNextType(.cdata);
    try scanner.expectNextStringSeq(&.{ "]", "]", null });
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "<![CDATA[" ++ "]]]" ++ "]]>");
    try scanner.expectNextType(.cdata);
    try scanner.expectNextStringSeq(&.{ "]", "]", "]", null });
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "<![CDATA[" ++ "]>" ++ "]]>");
    try scanner.expectNextType(.cdata);
    try scanner.expectNextStringSeq(&.{ "]", ">", null });
    try scanner.expectNextType(.eof);

    scanner = Scanner.initStreaming(.{});
    for ("<![CDATA[") |c| {
        try scanner.expectNextType(error.BufferUnderrun);
        scanner.feedInput(&.{c});
    }
    try scanner.expectNextType(.cdata);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("foo");
    try scanner.expectNextString("foo");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("]]");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("]");
    try scanner.expectNextString("]");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("]]]");
    try scanner.expectNextStringSeq(&.{ "]", "]", "]", error.BufferUnderrun }); // from the first to `feedInput`s, not the latest one
    scanner.feedInput(">");
    try scanner.expectNextString(null);
    scanner.feedEof();
    try scanner.expectNextType(.eof);
}

test "Scanner Character/Entity References" {
    var scanner: Scanner = undefined;

    scanner = Scanner.initComplete(.{}, "&abc;");
    try scanner.expectNextType(.reference);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "abc", null });
    try scanner.expectNextType(.reference_end);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "&;");
    try scanner.expectNextType(.reference);
    try scanner.expectNextType(.reference_end);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "&");
    try scanner.expectNextType(.reference);
    try scanner.expectNextType(.reference_end_invalid);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "&foo");
    try scanner.expectNextType(.reference);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try scanner.expectNextType(.reference_end_invalid);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "&foo ");
    try scanner.expectNextType(.reference);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try scanner.expectNextType(.reference_end_invalid);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ " ", null });
    try scanner.expectNextType(.eof);

    scanner = Scanner.initStreaming(.{});
    scanner.feedInput("&");
    try scanner.expectNextType(.reference);
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedInput(";");
    try scanner.expectNextType(.reference_end);
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextType(.eof);

    scanner = Scanner.initStreaming(.{});
    scanner.feedInput("&");
    try scanner.expectNextType(.reference);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("foo");
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", error.BufferUnderrun });
    scanner.feedInput("bar");
    try scanner.expectNextStringSeq(&.{ "bar", error.BufferUnderrun });
    scanner.feedEof();
    try scanner.expectNextString(null);
    try scanner.expectNextType(.reference_end_invalid);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initStreaming(.{});
    scanner.feedInput("&");
    try scanner.expectNextType(.reference);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("foo");
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", error.BufferUnderrun });
    scanner.feedInput("bar");
    try scanner.expectNextStringSeq(&.{ "bar", error.BufferUnderrun });
    scanner.feedInput(";");
    try scanner.expectNextString(null);
    try scanner.expectNextType(.reference_end);
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextType(.eof);

    scanner = Scanner.initStreaming(.{});
    scanner.feedInput("&");
    try scanner.expectNextType(.reference);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("foo");
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", error.BufferUnderrun });
    scanner.feedInput("bar");
    try scanner.expectNextStringSeq(&.{ "bar", error.BufferUnderrun });
    scanner.feedInput(" ");
    try scanner.expectNextString(null);
    try scanner.expectNextType(.reference_end_invalid);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ " ", error.BufferUnderrun });
    scanner.feedEof();
    try scanner.expectNextString(null);
    try scanner.expectNextType(.eof);
}

test "Scanner Text Data" {
    var scanner: Scanner = undefined;

    scanner = Scanner.initComplete(.{}, "");
    try scanner.expectNextType(.eof);

    scanner = Scanner.initStreaming(.{});
    scanner.feedEof();
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "foo bar");
    try scanner.expectNextType(.text_data);
    try scanner.expectNextString("foo bar");
    try scanner.expectNextString(null);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "foo bar");
    try scanner.expectNextType(.text_data);
    try scanner.expectNextString("foo bar");
    try scanner.expectNextString(null);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initStreaming(.{});
    scanner.feedInput("foo");
    try scanner.expectNextType(.text_data);
    try scanner.expectNextString("foo");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(" bar");
    try scanner.expectNextString(" bar");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextString(null);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "]]>");
    try scanner.expectNextType(.cdata_end);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initStreaming(.{});
    scanner.feedInput("]]>");
    try scanner.expectNextType(.cdata_end);
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedInput("foo");
    try scanner.expectNextType(.text_data);
    try scanner.expectNextString("foo");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextString(null);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "foo]]> bar");
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "foo", null });
    try scanner.expectNextType(.cdata_end);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ " bar", null });
    try scanner.expectNextType(.eof);

    scanner = Scanner.initStreaming(.{});
    scanner.feedInput("]");
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "]", null });
    try scanner.expectNextType(.eof);

    scanner = Scanner.initStreaming(.{});
    scanner.feedInput("]]");
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "]]", null });
    try scanner.expectNextType(.eof);

    scanner = Scanner.initStreaming(.{});
    scanner.feedInput("]]>");
    try scanner.expectNextType(.cdata_end);
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextType(.eof);
}

test "Scanner Element Closing Tags" {
    var scanner: Scanner = undefined;

    scanner = Scanner.initComplete(.{}, "</foo>");
    try scanner.expectNextType(.element_close);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "</foo >");
    try scanner.expectNextType(.element_close);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "</ >");
    try scanner.expectNextType(.element_close);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "</>");
    try scanner.expectNextType(.element_close);
    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextType(.eof);
}

test "Scanner Element Opening Tags" {
    var scanner: Scanner = undefined;

    scanner = Scanner.initComplete(.{}, "<");
    try scanner.expectNextType(.element_open);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "<foo  />");
    try scanner.expectNextType(.element_open);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ "  ", null });
    try scanner.expectNextType(.tag_slash_close);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "<foo  >");
    try scanner.expectNextType(.element_open);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ "  ", null });
    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "<foo  / >");
    try scanner.expectNextType(.element_open);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ "  ", null });
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "/", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "<foo bar='fizz'>");
    try scanner.expectNextType(.element_open);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "bar", null });
    try scanner.expectNextType(.attr_eql);
    try scanner.expectNextType(.quote_single);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "fizz", null });
    try scanner.expectNextType(.quote_single);
    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "<foo bar = 'fizz' >");
    try scanner.expectNextType(.element_open);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "bar", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextType(.attr_eql);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextType(.quote_single);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "fizz", null });
    try scanner.expectNextType(.quote_single);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "<foo bar=\"fizz\">");
    try scanner.expectNextType(.element_open);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "bar", null });
    try scanner.expectNextType(.attr_eql);
    try scanner.expectNextType(.quote_double);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "fizz", null });
    try scanner.expectNextType(.quote_double);
    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "<foo bar='&baz;'>");
    try scanner.expectNextType(.element_open);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "bar", null });
    try scanner.expectNextType(.attr_eql);
    try scanner.expectNextType(.quote_single);
    try scanner.expectNextType(.reference);
    try scanner.expectNextStringSeq(&.{ "baz", null });
    try scanner.expectNextType(.reference_end);
    try scanner.expectNextType(.quote_single);
    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "<foo  bar='fizz&baz;buzz'>");
    try scanner.expectNextType(.element_open);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ "  ", null });
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "bar", null });
    try scanner.expectNextType(.attr_eql);
    try scanner.expectNextType(.quote_single);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "fizz", null });
    try scanner.expectNextType(.reference);
    try scanner.expectNextStringSeq(&.{ "baz", null });
    try scanner.expectNextType(.reference_end);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "buzz", null });
    try scanner.expectNextType(.quote_single);
    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initStreaming(.{});
    scanner.feedInput("<fo");
    try scanner.expectNextType(.element_open);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "fo", error.BufferUnderrun });
    scanner.feedInput("o  ba");
    try scanner.expectNextStringSeq(&.{ "o", null });

    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ "  ", null });

    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "ba", error.BufferUnderrun });
    scanner.feedInput("r='fi");
    try scanner.expectNextStringSeq(&.{ "r", null });

    try scanner.expectNextType(.attr_eql);

    try scanner.expectNextType(.quote_single);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "fi", error.BufferUnderrun });
    scanner.feedInput("zz&ba");
    try scanner.expectNextStringSeq(&.{ "zz", null });

    try scanner.expectNextType(.reference);
    try scanner.expectNextString("ba");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("z;bu");
    try scanner.expectNextStringSeq(&.{ "z", null });
    try scanner.expectNextType(.reference_end);

    try scanner.expectNextTypeStringSeq(.text_data, &.{ "bu", error.BufferUnderrun });
    scanner.feedInput("zz'");
    try scanner.expectNextStringSeq(&.{ "zz", null });
    try scanner.expectNextType(.quote_single);

    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedInput(">");
    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextType(.eof);
}

test "Scanner Document Type Definition" {
    if (true) return error.SkipZigTest;
    var scanner: Scanner = undefined;

    scanner = Scanner.initComplete(.{}, "<!DOCTYPE");
    try scanner.expectNextType(.dtd);
    try scanner.expectNextType(.eof);
    scanner = Scanner.initComplete(.{}, "<!DOCTYPE>");
    try scanner.expectNextType(.dtd);
    try scanner.expectNextType(.dtd_end);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "<!DOCTYPEfoo");
    try scanner.expectNextType(.dtd);
    try scanner.expectNextType(.dtd_token);
    try scanner.expectNextStringSeq(&.{ "foo", null });
    try scanner.expectNextType(.eof);

    scanner = Scanner.initStreaming(.{});
    scanner.feedInput("<!DOCTYPEfoo");
    try scanner.expectNextType(.dtd);
    try scanner.expectNextType(.dtd_token);
    try scanner.expectNextStringSeq(&.{ "foo", error.BufferUnderrun });
    scanner.feedEof();
    try scanner.expectNextString(null);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "<!DOCTYPE foo SYSTEM 'bar");
    try scanner.expectNextType(.dtd);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTypeStringSeq(.dtd_token, &.{ "foo", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTypeStringSeq(.dtd_token, &.{ "SYSTEM", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextType(.dtd_literal_quote_single);
    try scanner.expectNextStringSeq(&.{ "bar", null });
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "<!DOCTYPE foo SYSTEM 'bar' >");
    try scanner.expectNextType(.dtd);

    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTypeStringSeq(.dtd_token, &.{ "foo", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTypeStringSeq(.dtd_token, &.{ "SYSTEM", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });

    try scanner.expectNextType(.dtd_literal_quote_single);
    try scanner.expectNextStringSeq(&.{ "bar", null });
    try scanner.expectNextType(.dtd_literal_end);

    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextType(.dtd_end);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "<!DOCTYPE []>");
    try scanner.expectNextType(.dtd);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextType(.dtd_int_subset);
    try scanner.expectNextType(.dtd_int_subset_end);
    try scanner.expectNextType(.dtd_end);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "<!DOCTYPE [ asdf ]>");
    try scanner.expectNextType(.dtd);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextType(.dtd_int_subset);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTypeStringSeq(.invalid_token, &.{ "asdf", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextType(.dtd_int_subset_end);
    try scanner.expectNextType(.dtd_end);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "<!DOCTYPE [ asdf%foo; ]>");
    try scanner.expectNextType(.dtd);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextType(.dtd_int_subset);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTypeStringSeq(.invalid_token, &.{ "asdf", null });
    try scanner.expectNextTypeStringSeq(.dtd_int_subset_pe_ref, &.{ "foo", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextType(.dtd_int_subset_end);
    try scanner.expectNextType(.dtd_end);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(.{}, "<!DOCTYPE foo PUBLIC 'bar' [\n\n] >");
    try scanner.expectNextType(.dtd);

    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTypeStringSeq(.dtd_token, &.{ "foo", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTypeStringSeq(.dtd_token, &.{ "PUBLIC", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });

    try scanner.expectNextType(.dtd_literal_quote_single);
    try scanner.expectNextStringSeq(&.{ "bar", null });
    try scanner.expectNextType(.dtd_literal_end);

    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });

    try scanner.expectNextType(.dtd_int_subset);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ "\n\n", null });
    try scanner.expectNextType(.dtd_int_subset_end);

    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });

    try scanner.expectNextType(.dtd_end);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(
        \\<!DOCTYPE [
        \\  <!ELEMENT foo EMPTY>
        \\]>
    );
    try scanner.expectNextType(.dtd);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextType(.dtd_int_subset);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ "\n  ", null });
    try scanner.expectNextType(.dtd_int_subset_element);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTypeStringSeq(.dtd_token, &.{ "foo", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTypeStringSeq(.dtd_token, &.{ "EMPTY", null });
    try scanner.expectNextType(.dtd_int_subset_element_end);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ "\n", null });
    try scanner.expectNextType(.dtd_int_subset_end);
    try scanner.expectNextType(.dtd_end);
    try scanner.expectNextType(.eof);

    scanner = Scanner.initComplete(
        \\<!DOCTYPE [
        \\  <!ELEMENT foo
        \\]>
    );
}

test Scanner {
    var feeder = std.mem.window(u8,
        \\<?xml version="1.0" encoding="UTF-8"?>
        \\
        \\<foo>
        \\  Lorem ipsum
        \\  <bar fizz='buzz'><baz/></bar>
        \\</foo>
        \\
    , 2, 2);

    var scanner = Scanner.initStreaming(.{});
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextType(.pi);
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextType(.text_data);
    for ([_][]const u8{
        "xm", "l ", "ve", "rs", "io", "n=", "\"1", ".0", "\" ", "en", "co", "di", "ng", "=\"", "UT", "F-", "8\"",
    }) |expected| {
        try scanner.expectNextStringSeq(&.{ expected, error.BufferUnderrun });
        scanner.feedInput(feeder.next().?);
    }
    try scanner.expectNextType(.pi_end);

    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "\n\n", error.BufferUnderrun });
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);

    try scanner.expectNextType(.element_open);
    try scanner.expectNextString("f");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextString("oo");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextString(null);
    try scanner.expectNextType(.tag_basic_close);

    try scanner.expectNextType(.text_data);
    try scanner.expectNextString("\n");

    for (comptime &[_]*const [2:0]u8{
        "  ", "Lo", "re", "m ", "ip", "su", "m\n", "  ",
    }) |expected| {
        try scanner.expectNextString(error.BufferUnderrun);
        scanner.feedInput(feeder.next().?);
        try scanner.expectNextString(expected);
    }
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextString(null);

    try scanner.expectNextType(.element_open);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "b", error.BufferUnderrun });
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextStringSeq(&.{ "ar", error.BufferUnderrun });
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextString(null);

    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "f", error.BufferUnderrun });
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextStringSeq(&.{ "iz", error.BufferUnderrun });
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextStringSeq(&.{ "z", null });

    try scanner.expectNextType(.attr_eql);

    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextType(.quote_single);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "b", error.BufferUnderrun });
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextStringSeq(&.{ "uz", error.BufferUnderrun });
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextStringSeq(&.{ "z", null });
    try scanner.expectNextType(.quote_single);
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextType(.tag_basic_close);

    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextType(.element_open);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "ba", error.BufferUnderrun });
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextStringSeq(&.{ "z", null });
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextType(.tag_slash_close);

    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextType(.element_close);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "b", error.BufferUnderrun });
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextStringSeq(&.{ "ar", error.BufferUnderrun });
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextString(null);

    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "\n", error.BufferUnderrun });
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextString(null);

    try scanner.expectNextType(.element_close);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextType(.tag_token);
    try scanner.expectNextString("fo");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextString("o");
    try scanner.expectNextString(null);
    try scanner.expectNextType(.tag_basic_close);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextType(.text_data);
    try scanner.expectNextString("\n");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextString(null);
    try scanner.expectNextType(.eof);
}
