//! The lowest level API for interpreting an XML document.
//! While simple, it provides little to no safety in the event of API misuse.

const std = @import("std");
const assert = std.debug.assert;
const builtin = @import("builtin");

const Scanner = @This();
src: []const u8,
index: usize,
state: State,
eof_specified: bool,

/// Initializes the `Scanner` with the full input.
/// Calling `feedInput` or `feedEof` is illegal.
/// Treating `error.BufferUnderrun` as `unreachable` is safe.
pub inline fn initComplete(src: []const u8) Scanner {
    var scanner = Scanner.initStreaming();
    scanner.feedInput(src);
    scanner.feedEof();
    return scanner;
}

pub inline fn initStreaming() Scanner {
    return .{
        .src = "",
        .index = 0,
        .state = .text_data,
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
/// `nextString` after a call to `feedEof`.
pub inline fn feedInput(scanner: *Scanner, src: []const u8) void {
    assert(!scanner.eof_specified);
    assert(scanner.index == scanner.src.len);
    scanner.src = src;
    scanner.index = 0;
}

/// Inform the scanner that the entirety of the XML source has been
/// supplied directly after a call to `feedInput`, or directly after
/// encountering `error.BufferUnderrun`.
/// Subsequent calls to this are illegal after the first call.
pub fn feedEof(scanner: *Scanner) void {
    assert(!scanner.eof_specified);
    assert(scanner.index == 0 or scanner.index == scanner.src.len);
    scanner.eof_specified = true;
}

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
pub inline fn nextType(scanner: *Scanner) NextTypeError!TokenType {
    return scanner.nextTypeImpl();
}

pub const NextSrcError = BufferError;
/// The returned token reference is either a range referencing the current `scanner.src` value,
/// which will remain valid for as long as the referenced slice is valid and accessible.
/// It should be called repeatedly until it returns `null`. If `error.BufferUnderrun` is returned,
/// the caller should invoke `feedInput`, and then proceed.
pub inline fn nextSrc(scanner: *Scanner) NextSrcError!?TokenSrc {
    return scanner.nextSrcImpl();
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

pub const TokenType = enum {
    /// The end of the XML source.
    /// This is the last token that will appear.
    /// Terminates the token sequence.
    eof,

    /// Some form of contextually invalid token.
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

    /// Indicates '(' in a DTD tag Internal Subset declaration tag.
    dtd_lparen,
    /// Indicates ')' in a DTD tag Internal Subset declaration tag.
    dtd_rparen,
    /// Indicates '|' in a DTD tag Internal Subset declaration tag.
    dtd_pipe,
    /// Indicates ',' in a DTD tag Internal Subset declaration tag.
    dtd_comma,
    /// Indicates '?' in a DTD tag Internal Subset declaration tag.
    dtd_qmark,
    /// Indicates '*' in a DTD tag Internal Subset declaration tag.
    dtd_asterisk,
    /// Indicates '+' in a DTD tag Internal Subset declaration tag.
    dtd_plus,

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
    /// * `.element_dec` followed by a token sequence as described in its own documentation.
    /// * `.entity_decl` followed by a token sequence as described in its own documentation.
    /// * `.attlist_decl` followed by a token sequence as described in its own documentation.
    /// * `.notation_decl` followed by a token sequence as described in its own documentation.
    /// * `.dtd_int_subset_end`.
    /// * `.eof`.
    dtd_int_subset,
    /// The '<!ELEMENT' token.
    ///
    /// The subsequent token sequence will be one of:
    /// TODO: Document
    element_decl,
    /// The '<!ENTITY' token.
    ///
    /// The subsequent token sequence will be one of:
    /// TODO: Document
    entity_decl,
    /// The '<!ATTLIST' token.
    ///
    /// The subsequent token sequence will be one of:
    /// TODO: Document
    attlist_decl,
    /// The '<!NOTATION' token.
    ///
    /// The subsequent token sequence will be one of:
    /// TODO: Document
    notation_decl,
    /// The ']' token.
    ///
    /// Ends DTD Internal Subset, or is an invalid token if not preceeded by a matching `.dtd_int_subset` token.
    dtd_int_subset_end,

    /// Whether or not the token represents any text to be returned by `nextSrc`/`nextString`.
    pub inline fn hasString(token_type: TokenType) bool {
        return switch (token_type) {
            .invalid_token,
            .text_data,
            .tag_whitespace,
            .tag_token,
            => true,

            .eof,
            .tag_basic_close,

            .attr_eql,

            .dtd_lparen,
            .dtd_rparen,
            .dtd_pipe,
            .dtd_comma,
            .dtd_qmark,
            .dtd_asterisk,
            .dtd_plus,

            .quote_single,
            .quote_double,

            .reference,
            .pe_reference,
            .reference_end,
            .reference_end_invalid,

            .pi,
            .pi_end,

            .comment,
            .invalid_comment_dash_dash,
            .invalid_comment_end_triple_dash,
            .comment_end,

            .element_open,
            .element_close,
            .tag_slash_close,

            .cdata,
            .cdata_end,

            .dtd,
            .dtd_int_subset,
            .element_decl,
            .entity_decl,
            .attlist_decl,
            .notation_decl,
            .dtd_int_subset_end,
            => false,
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

fn nextTypeImpl(scanner: *Scanner) NextTypeError!TokenType {
    // TODO: Use `@call(.always_tail, nextTypeImpl, .{scanner})` in this function once that
    // works for this return type.
    const src = scanner.src;
    const eof_specified = scanner.eof_specified;
    switch (scanner.state) {
        .text_data => {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
                return .eof;
            }
            switch (src[scanner.index]) {
                ']' => {
                    scanner.state = .@"text_data,]";
                    scanner.index += 1;
                    // return @call(.always_tail, nextTypeImpl, .{scanner});
                    return scanner.nextTypeImpl();
                },
                '&' => {
                    scanner.state = .@"text_data,&";
                    scanner.index += 1;
                    return .reference;
                },
                '<' => {
                    scanner.state = .@"text_data,<";
                    scanner.index += 1;
                    // return @call(.always_tail, nextTypeImpl, .{scanner});
                    return scanner.nextTypeImpl();
                },
                else => return .text_data,
            }
        },
        .@"text_data,]" => {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
                return .text_data;
            }
            if (src[scanner.index] != ']') {
                return .text_data;
            }
            scanner.state = .@"text_data,]]";
            scanner.index += 1;
            // return @call(.always_tail, nextTypeImpl, .{scanner});
            return scanner.nextTypeImpl();
        },
        .@"text_data,]]" => {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
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
                if (!eof_specified) return error.BufferUnderrun;
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
                if (!eof_specified) return error.BufferUnderrun;
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
                    // return @call(.always_tail, nextTypeImpl, .{scanner});
                    return scanner.nextTypeImpl();
                },
                else => {
                    scanner.state = .element_tag;
                    return .element_open;
                },
            }
        },

        .@"text_data,<!" => {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            switch (src[scanner.index]) {
                '-' => {
                    scanner.state = .@"text_data,<!-";
                    scanner.index += 1;
                    // return @call(.always_tail, nextTypeImpl, .{scanner});
                    return scanner.nextTypeImpl();
                },
                'D' => {
                    scanner.state = .@"<!D";
                    scanner.index += 1;
                    // return @call(.always_tail, nextTypeImpl, .{scanner});
                    return scanner.nextTypeImpl();
                },
                '[' => {
                    scanner.state = .@"<![";
                    scanner.index += 1;
                    // return @call(.always_tail, nextTypeImpl, .{scanner});
                    return scanner.nextTypeImpl();
                },
                else => return .invalid_token,
            }
        },
        .@"text_data,<!-" => {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
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
                if (!eof_specified) return error.BufferUnderrun;
                return .eof;
            }
            if (src[scanner.index] != '?') {
                return .text_data;
            }
            scanner.state = .@"pi,?";
            scanner.index += 1;
            // return @call(.always_tail, nextTypeImpl, .{scanner});
            return scanner.nextTypeImpl();
        },
        .@"pi,?" => {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
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
                if (!eof_specified) return error.BufferUnderrun;
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
                    // return @call(.always_tail, nextTypeImpl, .{scanner});
                    return scanner.nextTypeImpl();
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
                if (!eof_specified) return error.BufferUnderrun;
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
                if (!eof_specified) return error.BufferUnderrun;
                return .eof;
            }
            const matching_quote_char: u8, //
            const matching_quote_type: TokenType, //
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
                    return matching_quote_type;
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
                if (!eof_specified) return error.BufferUnderrun;
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
                if (!eof_specified) return error.BufferUnderrun;
                scanner.state = .text_data;
                return .eof;
            }
            if (src[scanner.index] == '-') {
                scanner.state = .@"comment,-";
                scanner.index += 1;
                // return @call(.always_tail, nextTypeImpl, .{scanner});
                return scanner.nextTypeImpl();
            }
            return .text_data;
        },
        .@"comment,-" => {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
                return .text_data;
            }
            if (src[scanner.index] != '-') {
                return .text_data;
            }
            scanner.state = .@"comment,--";
            scanner.index += 1;
            // return @call(.always_tail, nextTypeImpl, .{scanner});
            return scanner.nextTypeImpl();
        },
        .@"comment,--" => {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
                scanner.state = .text_data;
                return .invalid_comment_dash_dash;
            }
            switch (src[scanner.index]) {
                '-' => {
                    scanner.state = .@"comment,---";
                    scanner.index += 1;
                    // return @call(.always_tail, nextTypeImpl, .{scanner});
                    return scanner.nextTypeImpl();
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
                if (!eof_specified) return error.BufferUnderrun;
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
        .@"<![",
        .@"<![C",
        .@"<![CD",
        .@"<![CDA",
        .@"<![CDAT",
        => |tag| {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            const expected_char: u8, const state_on_match: State = comptime switch (tag) {
                .@"<![" => .{ 'C', .@"<![C" },
                .@"<![C" => .{ 'D', .@"<![CD" },
                .@"<![CD" => .{ 'A', .@"<![CDA" },
                .@"<![CDA" => .{ 'T', .@"<![CDAT" },
                .@"<![CDAT" => .{ 'A', .@"<![CDATA" },
                else => unreachable,
            };
            if (src[scanner.index] != expected_char) {
                return .invalid_token;
            }
            scanner.state = state_on_match;
            scanner.index += 1;
            // return @call(.always_tail, nextTypeImpl, .{scanner});
            return scanner.nextTypeImpl();
        },
        .@"<![CDATA" => {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            if (src[scanner.index] != '[') {
                return .invalid_token;
            }
            scanner.state = .cdata;
            scanner.index += 1;
            return .cdata;
        },
        .cdata => {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
                return .eof;
            }
            if (src[scanner.index] != ']') {
                return .text_data;
            }
            scanner.state = .@"cdata,]";
            scanner.index += 1;
            // return @call(.always_tail, nextTypeImpl, .{scanner});
            return scanner.nextTypeImpl();
        },
        .@"cdata,]" => {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
                return .text_data;
            }
            if (src[scanner.index] != ']') {
                return .text_data;
            }
            scanner.state = .@"cdata,]]";
            scanner.index += 1;
            // return @call(.always_tail, nextTypeImpl, .{scanner});
            return scanner.nextTypeImpl();
        },
        .@"cdata,]]" => {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
                return .text_data;
            }
            if (src[scanner.index] != '>') {
                return .text_data;
            }
            scanner.state = .text_data;
            scanner.index += 1;
            return .cdata_end;
        },

        inline //
        .@"<!D",
        .@"<!DO",
        .@"<!DOC",
        .@"<!DOCT",
        .@"<!DOCTY",
        => |tag| {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
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
            // return @call(.always_tail, nextTypeImpl, .{scanner});
            return scanner.nextTypeImpl();
        },
        .@"<!DOCTYP" => {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            if (src[scanner.index] != 'E') {
                return .invalid_token;
            }
            scanner.state = .dtd;
            scanner.index += 1;
            return .dtd;
        },
        .dtd => {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
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
                    scanner.state = .dtd_sq;
                    scanner.index += 1;
                    return .quote_single;
                },
                '\"' => {
                    scanner.state = .dtd_dq;
                    scanner.index += 1;
                    return .quote_double;
                },
                '[' => {
                    scanner.state = .dtd_int_subset;
                    scanner.index += 1;
                    return .dtd_int_subset;
                },
                else => {
                    const non_whitespace = std.mem.indexOfScalar(u8, whitespace_set, src[scanner.index]) == null;
                    scanner.state = if (non_whitespace) .dtd_token else .dtd_whitespace;
                    return if (non_whitespace) .tag_token else .tag_whitespace;
                },
            }
        },

        .dtd_sq,
        .dtd_dq,
        => |tag| {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
                scanner.state = .text_data;
                return .eof;
            }
            const matching_quote_char: u8, //
            const matching_quote_type: TokenType //
            = switch (tag) {
                .dtd_sq => .{ '\'', .quote_single },
                .dtd_dq => .{ '\"', .quote_double },
                else => unreachable,
            };
            if (src[scanner.index] != matching_quote_char) {
                return .text_data;
            }
            scanner.state = .dtd;
            scanner.index += 1;
            return matching_quote_type;
        },
        .dtd_sq_ref => @panic("TODO"),
        .dtd_dq_ref => @panic("TODO"),
        .dtd_token => unreachable,
        .dtd_whitespace => unreachable,

        .dtd_int_subset => {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
                scanner.state = .text_data;
                return .eof;
            }
            switch (src[scanner.index]) {
                ']' => {
                    scanner.state = .dtd;
                    scanner.index += 1;
                    return .dtd_int_subset_end;
                },
                '%' => {
                    scanner.state = .dtd_int_subset_pe_reference;
                    scanner.index += 1;
                    return .pe_reference;
                },
                '<' => {
                    scanner.state = .@"dtd_int_subset,<";
                    scanner.index += 1;
                    // return @call(.always_tail, nextTypeImpl, .{scanner});
                    return scanner.nextTypeImpl();
                },
                else => {
                    const non_whitespace = std.mem.indexOfScalar(u8, whitespace_set, src[scanner.index]) == null;
                    scanner.state = if (non_whitespace) .dtd_int_subset_token else .dtd_int_subset_whitespace;
                    return if (non_whitespace) .invalid_token else .tag_whitespace;
                },
            }
        },
        .dtd_int_subset_token => unreachable,
        .dtd_int_subset_whitespace => unreachable,
        .dtd_int_subset_pe_reference,
        .dtd_int_subset_element_pe_reference,
        => |tag| {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
                scanner.state = .text_data;
                return .reference_end_invalid;
            }
            const on_end_state: State = switch (tag) {
                .dtd_int_subset_pe_reference => .dtd_int_subset,
                .dtd_int_subset_element_pe_reference => .dtd_int_subset_element,
                else => unreachable,
            };
            if (src[scanner.index] == ';') {
                scanner.state = on_end_state;
                scanner.index += 1;
                return .reference_end;
            }
            if (std.mem.indexOfScalar(u8, whitespace_set ++ &[_]u8{ ']', '&', '<' }, src[scanner.index]) != null) {
                scanner.state = on_end_state;
                return .reference_end_invalid;
            }
            return .tag_token;
        },

        .@"dtd_int_subset,<" => {
            if (scanner.index == src.len) {
                if (eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            switch (src[scanner.index]) {
                '?' => {
                    scanner.state = .@"dtd_int_subset,<?";
                    scanner.index += 1;
                    // return @call(.always_tail, nextTypeImpl, .{scanner});
                    return scanner.nextTypeImpl();
                },
                '!' => {
                    scanner.state = .@"dtd_int_subset,<!";
                    scanner.index += 1;
                    // return @call(.always_tail, nextTypeImpl, .{scanner});
                    return scanner.nextTypeImpl();
                },
                else => return .invalid_token,
            }
        },
        .@"dtd_int_subset,<?" => @panic("TODO"),
        .@"dtd_int_subset,<!" => {
            if (scanner.index == src.len) {
                if (eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            switch (src[scanner.index]) {
                'E' => {
                    scanner.state = .@"dtd_int_subset,<!E";
                    scanner.index += 1;
                    // return @call(.always_tail, nextTypeImpl, .{scanner});
                    return scanner.nextTypeImpl();
                },
                'A' => {
                    scanner.state = .@"dtd_int_subset,<!A";
                    scanner.index += 1;
                    // return @call(.always_tail, nextTypeImpl, .{scanner});
                    return scanner.nextTypeImpl();
                },
                else => return .invalid_token,
            }
        },

        .@"dtd_int_subset,<!E" => {
            if (scanner.index == src.len) {
                if (eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            switch (src[scanner.index]) {
                'N' => {
                    scanner.state = .@"dtd_int_subset,<!EN";
                    scanner.index += 1;
                    // return @call(.always_tail, nextTypeImpl, .{scanner});
                    return scanner.nextTypeImpl();
                },
                'L' => {
                    scanner.state = .@"dtd_int_subset,<!EL";
                    scanner.index += 1;
                    // return @call(.always_tail, nextTypeImpl, .{scanner});
                    return scanner.nextTypeImpl();
                },
                else => return .invalid_token,
            }
        },

        inline //
        .@"dtd_int_subset,<!EN",
        .@"dtd_int_subset,<!ENT",
        .@"dtd_int_subset,<!ENTI",

        .@"dtd_int_subset,<!EL",
        .@"dtd_int_subset,<!ELE",
        .@"dtd_int_subset,<!ELEM",
        .@"dtd_int_subset,<!ELEME",

        .@"dtd_int_subset,<!A",
        .@"dtd_int_subset,<!AT",
        .@"dtd_int_subset,<!ATT",
        .@"dtd_int_subset,<!ATTL",
        .@"dtd_int_subset,<!ATTLI",

        .@"dtd_int_subset,<!N",
        .@"dtd_int_subset,<!NO",
        .@"dtd_int_subset,<!NOT",
        .@"dtd_int_subset,<!NOTA",
        .@"dtd_int_subset,<!NOTAT",
        .@"dtd_int_subset,<!NOTATI",
        => |tag| {
            if (scanner.index == src.len) {
                if (eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            const expected_char: u8, //
            const state_on_match: State //
            = comptime switch (tag) {
                .@"dtd_int_subset,<!EN" => .{ 'T', .@"dtd_int_subset,<!ENT" },
                .@"dtd_int_subset,<!ENT" => .{ 'I', .@"dtd_int_subset,<!ENTI" },
                .@"dtd_int_subset,<!ENTI" => .{ 'T', .@"dtd_int_subset,<!ENTIT" },

                .@"dtd_int_subset,<!EL" => .{ 'E', .@"dtd_int_subset,<!ELE" },
                .@"dtd_int_subset,<!ELE" => .{ 'M', .@"dtd_int_subset,<!ELEM" },
                .@"dtd_int_subset,<!ELEM" => .{ 'E', .@"dtd_int_subset,<!ELEME" },
                .@"dtd_int_subset,<!ELEME" => .{ 'N', .@"dtd_int_subset,<!ELEMEN" },

                .@"dtd_int_subset,<!A" => .{ 'T', .@"dtd_int_subset,<!AT" },
                .@"dtd_int_subset,<!AT" => .{ 'T', .@"dtd_int_subset,<!ATT" },
                .@"dtd_int_subset,<!ATT" => .{ 'L', .@"dtd_int_subset,<!ATTL" },
                .@"dtd_int_subset,<!ATTL" => .{ 'I', .@"dtd_int_subset,<!ATTLI" },
                .@"dtd_int_subset,<!ATTLI" => .{ 'S', .@"dtd_int_subset,<!ATTLIS" },

                .@"dtd_int_subset,<!N" => .{ 'O', .@"dtd_int_subset,<!NO" },
                .@"dtd_int_subset,<!NO" => .{ 'T', .@"dtd_int_subset,<!NOT" },
                .@"dtd_int_subset,<!NOT" => .{ 'A', .@"dtd_int_subset,<!NOTA" },
                .@"dtd_int_subset,<!NOTA" => .{ 'T', .@"dtd_int_subset,<!NOTAT" },
                .@"dtd_int_subset,<!NOTAT" => .{ 'I', .@"dtd_int_subset,<!NOTATI" },
                .@"dtd_int_subset,<!NOTATI" => .{ 'O', .@"dtd_int_subset,<!NOTATIO" },

                else => unreachable,
            };
            if (src[scanner.index] != expected_char) {
                return .invalid_token;
            }
            scanner.state = state_on_match;
            scanner.index += 1;
            // return @call(.always_tail, nextTypeImpl, .{scanner});
            return scanner.nextTypeImpl();
        },

        .@"dtd_int_subset,<!ENTIT" => {
            if (scanner.index == src.len) {
                if (eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            if (src[scanner.index] != 'Y') {
                return .invalid_token;
            }
            scanner.state = .dtd_int_subset_entity;
            scanner.index += 1;
            return .entity_decl;
        },
        .dtd_int_subset_entity => @panic("TODO"),

        .@"dtd_int_subset,<!ELEMEN" => {
            if (scanner.index == src.len) {
                if (eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            if (src[scanner.index] != 'T') {
                return .invalid_token;
            }
            scanner.state = .dtd_int_subset_element;
            scanner.index += 1;
            return .element_decl;
        },
        .dtd_int_subset_element => {
            if (scanner.index == src.len) {
                if (eof_specified) return error.BufferUnderrun;
                scanner.state = .text_data;
                return .eof;
            }
            switch (src[scanner.index]) {
                '(' => {
                    scanner.index += 1;
                    return .dtd_lparen;
                },
                '|' => {
                    scanner.index += 1;
                    return .dtd_pipe;
                },
                ',' => {
                    scanner.index += 1;
                    return .dtd_comma;
                },
                ')' => {
                    scanner.index += 1;
                    return .dtd_rparen;
                },
                '?' => {
                    scanner.index += 1;
                    return .dtd_qmark;
                },
                '*' => {
                    scanner.index += 1;
                    return .dtd_asterisk;
                },
                '+' => {
                    scanner.index += 1;
                    return .dtd_plus;
                },
                '%' => {
                    scanner.state = .dtd_int_subset_element_pe_reference;
                    scanner.index += 1;
                    return .pe_reference;
                },
                '>' => {
                    scanner.state = .dtd_int_subset;
                    scanner.index += 1;
                    return .tag_basic_close;
                },
                else => {
                    const non_whitespace = std.mem.indexOfScalar(u8, whitespace_set, src[scanner.index]) == null;
                    scanner.state = if (non_whitespace) .dtd_int_subset_element_token else .dtd_int_subset_element_whitespace;
                    return if (non_whitespace) .tag_token else .tag_whitespace;
                },
            }
        },
        .dtd_int_subset_element_token => @panic("TODO"),
        .dtd_int_subset_element_whitespace => @panic("TODO"),

        .@"dtd_int_subset,<!ATTLIS" => {
            if (scanner.index == src.len) {
                if (eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            if (src[scanner.index] != 'T') {
                return .invalid_token;
            }
            scanner.state = .dtd_int_subset_attlist;
            scanner.index += 1;
            return .attlist_decl;
        },
        .dtd_int_subset_attlist => @panic("TODO"),

        .@"dtd_int_subset,<!NOTATIO" => {
            if (scanner.index == src.len) {
                if (eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            if (src[scanner.index] != 'N') {
                return .invalid_token;
            }
            scanner.state = .dtd_int_subset_notation;
            scanner.index += 1;
            return .notation_decl;
        },
        .dtd_int_subset_notation => @panic("TODO"),

        .@"<!:incomplete" => @panic("TODO"),
        .@"<!--:incomplete" => @panic("TODO"),
        .@"<![CDATA[:incomplete" => @panic("TODO"),
        .@"<!DOCTYPE:incomplete" => @panic("TODO"),
    }
}

fn nextSrcImpl(scanner: *Scanner) NextSrcError!?TokenSrc {
    // TODO: Use `@call(.always_tail, nextSrcImpl, .{scanner})` in this function once that
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
    const eof_specified = scanner.eof_specified;
    switch (scanner.state) {
        .text_data => {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
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
                    // return @call(.always_tail, nextSrcImpl, .{scanner});
                    return scanner.nextSrc();
                },
                '&', '<' => {},
                else => unreachable,
            }
            return null;
        },
        .@"text_data,]" => {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
                scanner.state = .text_data;
                return helper.literalInit(.@"]");
            }
            if (src[scanner.index] != ']') {
                scanner.state = .text_data;
                return helper.literalInit(.@"]");
            }
            scanner.state = .@"text_data,]]";
            scanner.index += 1;
            // return @call(.always_tail, nextSrcImpl, .{scanner});
            return scanner.nextSrc();
        },
        .@"text_data,]]" => {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
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
                if (!eof_specified) return error.BufferUnderrun;
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
                if (!eof_specified) return error.BufferUnderrun;
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
            // return @call(.always_tail, nextSrcImpl, .{scanner});
            return scanner.nextSrc();
        },
        .@"pi,?" => {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
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
                if (!eof_specified) return error.BufferUnderrun;
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
                if (!eof_specified) return error.BufferUnderrun;
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
                if (!eof_specified) return error.BufferUnderrun;
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
                if (!eof_specified) return error.BufferUnderrun;
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
                if (!eof_specified) return error.BufferUnderrun;
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
                // return @call(.always_tail, nextSrcImpl, .{scanner});
                return scanner.nextSrc();
            }
            return null;
        },
        .@"comment,-" => {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
                scanner.state = .comment;
                return helper.literalInit(.@"-");
            }
            if (src[scanner.index] != '-') {
                scanner.state = .comment;
                return helper.literalInit(.@"-");
            }
            scanner.state = .@"comment,--";
            scanner.index += 1;
            // return @call(.always_tail, nextSrcImpl, .{scanner});
            return scanner.nextSrc();
        },
        .@"comment,--" => {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
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

        .cdata => {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
                scanner.state = .text_data;
                return null;
            }
            const str_start = scanner.index;
            const str_end = std.mem.indexOfScalarPos(u8, src, scanner.index, ']') orelse src.len;
            scanner.index = str_end;
            if (str_start != str_end) {
                return helper.rangeInit(str_start, str_end);
            }
            scanner.state = .@"cdata,]";
            scanner.index += 1;
            // return @call(.always_tail, nextSrcImpl, .{scanner});
            return scanner.nextSrc();
        },
        .@"cdata,]" => {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
                scanner.state = .text_data;
                return helper.literalInit(.@"]");
            }
            if (src[scanner.index] != ']') {
                scanner.state = .cdata;
                return helper.literalInit(.@"]");
            }
            scanner.state = .@"cdata,]]";
            scanner.index += 1;
            // return @call(.always_tail, nextSrcImpl, .{scanner});
            return scanner.nextSrc();
        },
        .@"cdata,]]" => {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
                scanner.state = .text_data;
                return helper.literalInit(.@"]]");
            }
            switch (src[scanner.index]) {
                '>' => return null,
                ']' => {
                    scanner.index += 1;
                    return helper.literalInit(.@"]");
                },
                else => {
                    scanner.state = .cdata;
                    return helper.literalInit(.@"]]");
                },
            }
        },

        .dtd => @panic("TODO"),

        .dtd_sq,
        .dtd_dq,
        => |tag| {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
                scanner.state = .text_data;
                return null;
            }
            const matching_quote_char: u8 = switch (tag) {
                .dtd_sq => '\'',
                .dtd_dq => '\"',
                else => unreachable,
            };
            const str_start = scanner.index;
            const str_end = std.mem.indexOfScalarPos(u8, src, scanner.index, matching_quote_char) orelse src.len;
            scanner.index = str_end;
            if (str_start != str_end) {
                return helper.rangeInit(str_start, str_end);
            }
            return null;
        },
        .dtd_sq_ref => @panic("TODO"),
        .dtd_dq_ref => @panic("TODO"),
        .dtd_token => {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
                scanner.state = .text_data;
                return null;
            }
            const str_start = scanner.index;
            const str_end = std.mem.indexOfAnyPos(u8, src, scanner.index, whitespace_set ++ &[_]u8{ '>', '[', '\'', '\"' }) orelse src.len;
            scanner.index = str_end;
            if (str_start != str_end) {
                return helper.rangeInit(str_start, str_end);
            }
            scanner.state = .dtd;
            return null;
        },
        .dtd_whitespace => {
            if (scanner.index == src.len) {
                if (eof_specified) return error.BufferUnderrun;
                scanner.state = .text_data;
                return null;
            }
            const str_start = scanner.index;
            const str_end = std.mem.indexOfNonePos(u8, src, scanner.index, whitespace_set) orelse src.len;
            scanner.index = str_end;
            if (str_start != str_end) {
                return helper.rangeInit(str_start, str_end);
            }
            scanner.state = .dtd;
            return null;
        },

        .dtd_int_subset => unreachable,
        .dtd_int_subset_token => {
            if (scanner.index == src.len) {
                if (eof_specified) return error.BufferUnderrun;
                scanner.state = .text_data;
                return null;
            }
            const str_start = scanner.index;
            const str_end = std.mem.indexOfAnyPos(u8, src, scanner.index, whitespace_set ++ &[_]u8{ '%', ']', '>' }) orelse src.len;
            scanner.index = str_end;
            if (str_start != str_end) {
                return helper.rangeInit(str_start, str_end);
            }
            scanner.state = .dtd_int_subset;
            return null;
        },
        .dtd_int_subset_whitespace => {
            if (scanner.index == src.len) {
                if (eof_specified) return error.BufferUnderrun;
                scanner.state = .text_data;
                return null;
            }
            const str_start = scanner.index;
            const str_end = std.mem.indexOfNonePos(u8, src, scanner.index, whitespace_set) orelse src.len;
            scanner.index = str_end;
            if (str_start != str_end) {
                return helper.rangeInit(str_start, str_end);
            }
            scanner.state = .dtd_int_subset;
            return null;
        },
        .dtd_int_subset_pe_reference,
        .dtd_int_subset_element_pe_reference,
        => {
            if (scanner.index == src.len) {
                if (eof_specified) return error.BufferUnderrun;
                scanner.state = .text_data;
                return null;
            }
            const str_start = scanner.index;
            const str_end = std.mem.indexOfAnyPos(u8, src, scanner.index, whitespace_set ++ &[_]u8{ ';', '%', '<', ']', '>' }) orelse src.len;
            scanner.index = str_end;
            if (str_start != str_end) {
                return helper.rangeInit(str_start, str_end);
            }
            return null;
        },

        .dtd_int_subset_entity => @panic("TODO"),

        .dtd_int_subset_element => unreachable,
        .dtd_int_subset_element_token => {
            if (scanner.index == src.len) {
                if (!eof_specified) return error.BufferUnderrun;
                scanner.state = .text_data;
                return null;
            }
            const str_start = scanner.index;
            const str_end = std.mem.indexOfAnyPos(u8, src, scanner.index, whitespace_set ++ &[_]u8{
                '(', '|', ',', ')',
                '?', '*', '+', //
                '%', '>',
            }) orelse src.len;
            scanner.index = str_end;
            if (str_start != str_end) {
                return helper.rangeInit(str_start, str_end);
            }
            scanner.state = .dtd_int_subset_element;
            return null;
        },
        .dtd_int_subset_element_whitespace => {
            if (scanner.index == src.len) {
                if (eof_specified) return error.BufferUnderrun;
                scanner.state = .text_data;
                return null;
            }
            const str_start = scanner.index;
            const str_end = std.mem.indexOfNonePos(u8, src, scanner.index, whitespace_set) orelse src.len;
            scanner.index = str_end;
            if (str_start != str_end) {
                return helper.rangeInit(str_start, str_end);
            }
            scanner.state = .dtd_int_subset_element;
            return null;
        },

        .dtd_int_subset_attlist => @panic("TODO"),
        .dtd_int_subset_notation => @panic("TODO"),

        // invalid tokens

        .@"text_data,<!" => {
            scanner.state = .@"<!:incomplete";
            return helper.literalInit(.@"<!");
        },
        .@"text_data,<!-" => {
            scanner.state = .@"<!--:incomplete";
            return helper.literalInit(.@"<!-");
        },

        .@"<![",
        .@"<![C",
        .@"<![CD",
        .@"<![CDA",
        .@"<![CDAT",
        .@"<![CDATA",
        => |tag| {
            scanner.state = .@"<![CDATA[:incomplete";
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

        .@"<!D",
        .@"<!DO",
        .@"<!DOC",
        .@"<!DOCT",
        .@"<!DOCTY",
        .@"<!DOCTYP",
        => |tag| {
            scanner.state = .@"<!DOCTYPE:incomplete";
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

        .@"dtd_int_subset,<" => @panic("TODO"),

        .@"dtd_int_subset,<?" => @panic("TODO"),
        .@"dtd_int_subset,<!" => @panic("TODO"),

        .@"dtd_int_subset,<!E" => @panic("TODO"),

        .@"dtd_int_subset,<!EN" => @panic("TODO"),
        .@"dtd_int_subset,<!ENT" => @panic("TODO"),
        .@"dtd_int_subset,<!ENTI" => @panic("TODO"),
        .@"dtd_int_subset,<!ENTIT" => @panic("TODO"),

        .@"dtd_int_subset,<!EL" => @panic("TODO"),
        .@"dtd_int_subset,<!ELE" => @panic("TODO"),
        .@"dtd_int_subset,<!ELEM" => @panic("TODO"),
        .@"dtd_int_subset,<!ELEME" => @panic("TODO"),
        .@"dtd_int_subset,<!ELEMEN" => @panic("TODO"),

        .@"dtd_int_subset,<!A" => @panic("TODO"),
        .@"dtd_int_subset,<!AT" => @panic("TODO"),
        .@"dtd_int_subset,<!ATT" => @panic("TODO"),
        .@"dtd_int_subset,<!ATTL" => @panic("TODO"),
        .@"dtd_int_subset,<!ATTLI" => @panic("TODO"),
        .@"dtd_int_subset,<!ATTLIS" => @panic("TODO"),

        .@"dtd_int_subset,<!N" => @panic("TODO"),
        .@"dtd_int_subset,<!NO" => @panic("TODO"),
        .@"dtd_int_subset,<!NOT" => @panic("TODO"),
        .@"dtd_int_subset,<!NOTA" => @panic("TODO"),
        .@"dtd_int_subset,<!NOTAT" => @panic("TODO"),
        .@"dtd_int_subset,<!NOTATI" => @panic("TODO"),
        .@"dtd_int_subset,<!NOTATIO" => @panic("TODO"),

        .@"<!:incomplete" => return null,
        .@"<!--:incomplete" => return null,
        .@"<![CDATA[:incomplete" => return null,
        .@"<!DOCTYPE:incomplete" => return null,
    }
}

pub const TokenSrc = union(enum) {
    range: Range,
    literal: Literal,

    /// Refers to a range in the `src` field of the Scanner.
    pub const Range = struct {
        start: usize,
        end: usize,

        pub inline fn getStr(tok_range: Range, scanner: *const Scanner) []const u8 {
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

    @"<![",
    @"<![C",
    @"<![CD",
    @"<![CDA",
    @"<![CDAT",
    @"<![CDATA",
    cdata,
    @"cdata,]",
    @"cdata,]]",

    @"<!D",
    @"<!DO",
    @"<!DOC",
    @"<!DOCT",
    @"<!DOCTY",
    @"<!DOCTYP",
    dtd,
    dtd_sq,
    dtd_dq,
    dtd_sq_ref,
    dtd_dq_ref,
    dtd_token,
    dtd_whitespace,

    dtd_int_subset,
    dtd_int_subset_token,
    dtd_int_subset_whitespace,
    dtd_int_subset_pe_reference,

    @"dtd_int_subset,<",
    @"dtd_int_subset,<?",
    @"dtd_int_subset,<!",

    @"dtd_int_subset,<!E",

    @"dtd_int_subset,<!EN",
    @"dtd_int_subset,<!ENT",
    @"dtd_int_subset,<!ENTI",
    @"dtd_int_subset,<!ENTIT",
    dtd_int_subset_entity,

    @"dtd_int_subset,<!EL",
    @"dtd_int_subset,<!ELE",
    @"dtd_int_subset,<!ELEM",
    @"dtd_int_subset,<!ELEME",
    @"dtd_int_subset,<!ELEMEN",
    dtd_int_subset_element,
    dtd_int_subset_element_token,
    dtd_int_subset_element_whitespace,
    dtd_int_subset_element_pe_reference,

    @"dtd_int_subset,<!A",
    @"dtd_int_subset,<!AT",
    @"dtd_int_subset,<!ATT",
    @"dtd_int_subset,<!ATTL",
    @"dtd_int_subset,<!ATTLI",
    @"dtd_int_subset,<!ATTLIS",
    dtd_int_subset_attlist,

    @"dtd_int_subset,<!N",
    @"dtd_int_subset,<!NO",
    @"dtd_int_subset,<!NOT",
    @"dtd_int_subset,<!NOTA",
    @"dtd_int_subset,<!NOTAT",
    @"dtd_int_subset,<!NOTATI",
    @"dtd_int_subset,<!NOTATIO",
    dtd_int_subset_notation,

    @"<!:incomplete",
    @"<!--:incomplete",
    @"<![CDATA[:incomplete",
    @"<!DOCTYPE:incomplete",
};

/// A checked version of the scanner, mainly used for testing and debugging.
/// Panics on API misuse instead of reacting with undefined behaviour.
/// Acts as a drop-in replacement to facilitate this use case.
pub const CheckedScanner = struct {
    raw: Scanner,
    is_streaming: bool,
    prev_output: PrevOutput,

    const PrevOutput = union(enum) {
        init,
        tok: TokenType,
        str: enum { non_null, null },
    };

    pub inline fn initComplete(src: []const u8) CheckedScanner {
        return .{
            .raw = Scanner.initComplete(src),
            .is_streaming = false,
            .prev_output = .init,
        };
    }

    pub inline fn initStreaming() CheckedScanner {
        return .{
            .raw = Scanner.initStreaming(),
            .is_streaming = true,
            .prev_output = .init,
        };
    }

    pub fn feedInput(checked: *CheckedScanner, src: []const u8) void {
        if (!checked.is_streaming) @panic("Can't feed input to a non-streaming scanner");
        checked.raw.feedInput(src);
    }

    pub fn feedEof(checked: *CheckedScanner) void {
        if (!checked.is_streaming) @panic("Can't feed input to a non-streaming scanner");
        checked.raw.feedEof();
    }

    pub fn nextType(checked: *CheckedScanner) NextTypeError!TokenType {
        switch (checked.prev_output) {
            .init => {},
            .tok => |prev| if (prev.hasString()) @panic(switch (prev) {
                inline else => |tag| "Can't call `nextType` when the previous token '." ++ @tagName(tag) ++
                    "' has a string pending - call `nextSrc` or `nextString` first.",
            }),
            .str => |str| switch (str) {
                .non_null => @panic("Can't call `nextType` when there is still a string pending - call `nextSrc` or `nextString` until they return null."),
                .null => {},
            },
        }
        const tok_type = try checked.raw.nextType();
        checked.prev_output = .{ .tok = tok_type };
        return tok_type;
    }

    pub fn nextSrc(checked: *CheckedScanner) NextSrcError!?TokenSrc {
        switch (checked.prev_output) {
            .init => {},
            .tok => |prev| if (!prev.hasString()) @panic(switch (prev) {
                inline else => |tag| "Can't call `nextSrc`/`nextString` for token type '." ++ @tagName(tag) ++ "'. Call `nextType` instead.",
            }),
            .str => |str| switch (str) {
                .non_null => {},
                .null => @panic("Can't call `nextSrc` or `nextString` after one of them have already returned null. Call `nextType` instead."),
            },
        }
        const tok_src = try checked.raw.nextSrc();
        checked.prev_output = .{ .str = if (tok_src != null) .non_null else .null };
        return tok_src;
    }

    pub fn nextString(checked: *CheckedScanner) NextStringError!?[]const u8 {
        const tok_src = try checked.nextSrc();
        return switch (tok_src orelse return null) {
            .range => |range| range.getStr(&checked.raw),
            .literal => |literal| literal.toStr(),
        };
    }

    pub fn expectNextType(scanner: *CheckedScanner, expected: NextTypeError!TokenType) !void {
        comptime assert(builtin.is_test);
        const actual = scanner.nextType();
        try std.testing.expectEqual(expected, actual);
    }
    pub fn expectNextString(scanner: *CheckedScanner, expected: NextSrcError!?[]const u8) !void {
        comptime assert(builtin.is_test);
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
    pub fn expectNextStringSeq(scanner: *CheckedScanner, expected_list: []const NextSrcError!?[]const u8) !void {
        for (expected_list) |expected| try scanner.expectNextString(expected);
    }
    pub fn expectNextTypeStringSeq(scanner: *CheckedScanner, expected_tt: TokenType, expected_list: []const NextSrcError!?[]const u8) !void {
        try scanner.expectNextType(expected_tt);
        try std.testing.expect(expected_tt.hasString());
        try scanner.expectNextStringSeq(expected_list);
    }
};

fn testingPrint(comptime fmt_str: []const u8, args: anytype) void {
    if (@inComptime()) {
        @compileError(std.fmt.comptimePrint(fmt_str, args));
    } else if (std.testing.backend_can_print) {
        std.debug.print(fmt_str, args);
    }
}

test "Scanner Incomplete Markup ('<!--'/'<![CDATA['/'<!DOCTYPE')" {
    if (true) return error.SkipZigTest;
    var scanner: CheckedScanner = undefined;

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
        scanner = CheckedScanner.initComplete(incomplete_mk_str);
        try scanner.expectNextType(.invalid_token);
        try scanner.expectNextString(incomplete_mk_str);
        try scanner.expectNextString(null);
        try scanner.expectNextType(.eof);

        scanner = CheckedScanner.initStreaming();
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

    scanner = CheckedScanner.initComplete("<! a <");
    try scanner.expectNextTypeStringSeq(.invalid_token, &.{ "<!", null });
    try scanner.expectNextTypeStringSeq(.text_data, &.{ " a ", null });
    try scanner.expectNextType(.element_open);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("<! a");
    try scanner.expectNextTypeStringSeq(.invalid_token, &.{ "<!", null });
    try scanner.expectNextTypeStringSeq(.text_data, &.{ " a", null });
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("<![CDATAR a");
    try scanner.expectNextType(.invalid_token);
    try scanner.expectNextString("<![CDATA");
    try scanner.expectNextString(null);
    try scanner.expectNextType(.text_data);
    try scanner.expectNextString("R a");
    try scanner.expectNextString(null);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("<!DOCTYPR a");
    try scanner.expectNextType(.invalid_token);
    try scanner.expectNextString("<!DOCTYP");
    try scanner.expectNextString(null);
    try scanner.expectNextType(.text_data);
    try scanner.expectNextString("R a");
    try scanner.expectNextString(null);
    try scanner.expectNextType(.eof);
}

test "Scanner Processing Instructions" {
    var scanner: CheckedScanner = undefined;

    scanner = CheckedScanner.initComplete("<??>");
    try scanner.expectNextType(.pi);
    try scanner.expectNextType(.pi_end);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("<?");
    try scanner.expectNextType(.pi);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("<? ?>");
    try scanner.expectNextType(.pi);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ " ", null });
    try scanner.expectNextType(.pi_end);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("<?foo?>");
    try scanner.expectNextType(.pi);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "foo", null });
    try scanner.expectNextType(.pi_end);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("<?foo ?>");
    try scanner.expectNextType(.pi);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "foo ", null });
    try scanner.expectNextType(.pi_end);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("<?foo bar?>");
    try scanner.expectNextType(.pi);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "foo bar", null });
    try scanner.expectNextType(.pi_end);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("<?" ++ "???" ++ "?>");
    try scanner.expectNextType(.pi);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "?", "?", "?", null });
    try scanner.expectNextType(.pi_end);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initStreaming();
    scanner.feedInput("<?foo");
    try scanner.expectNextType(.pi);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "foo", error.BufferUnderrun });
    scanner.feedInput("?>");
    try scanner.expectNextString(null);
    try scanner.expectNextType(.pi_end);
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initStreaming();
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

    scanner = CheckedScanner.initStreaming();
    scanner.feedInput("<?");
    try scanner.expectNextType(.pi);
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedInput("fizz?>");
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "fizz", null });
    try scanner.expectNextType(.pi_end);
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initStreaming();
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

    scanner = CheckedScanner.initStreaming();
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
    var scanner: CheckedScanner = undefined;

    scanner = CheckedScanner.initComplete("<!--" ++ "-->");
    try scanner.expectNextType(.comment);
    try scanner.expectNextType(.comment_end);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("<!--" ++ "-" ++ "-->");
    try scanner.expectNextType(.comment);
    try scanner.expectNextType(.invalid_comment_end_triple_dash);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("<!--" ++ "--" ++ "-->");
    try scanner.expectNextType(.comment);
    try scanner.expectNextType(.invalid_comment_dash_dash);
    try scanner.expectNextType(.comment_end);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("<!--" ++ "--" ++ "-" ++ "-->");
    try scanner.expectNextType(.comment);
    try scanner.expectNextType(.invalid_comment_dash_dash);
    try scanner.expectNextType(.invalid_comment_end_triple_dash);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("<!-- <foo bar> -->");
    try scanner.expectNextType(.comment);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ " <foo bar> ", null });
    try scanner.expectNextType(.comment_end);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initStreaming();
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

    scanner = CheckedScanner.initStreaming();
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

    scanner = CheckedScanner.initStreaming();
    scanner.feedInput("<!--");
    try scanner.expectNextType(.comment);
    try scanner.expectNextType(error.BufferUnderrun);
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
    var scanner: CheckedScanner = undefined;

    scanner = CheckedScanner.initComplete("<![CDATA[" ++ "]]>");
    try scanner.expectNextType(.cdata);
    try scanner.expectNextType(.cdata_end);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("<![CDATA[" ++ " foo " ++ "]]>");
    try scanner.expectNextType(.cdata);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ " foo ", null });
    try scanner.expectNextType(.cdata_end);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("<![CDATA[" ++ "]]" ++ "]]>");
    try scanner.expectNextType(.cdata);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "]", "]", null });
    try scanner.expectNextType(.cdata_end);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("<![CDATA[" ++ "]" ++ "]]" ++ "]]>");
    try scanner.expectNextType(.cdata);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "]", "]", "]", null });
    try scanner.expectNextType(.cdata_end);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("<![CDATA[" ++ "]>" ++ "]]>");
    try scanner.expectNextType(.cdata);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "]", ">", null });
    try scanner.expectNextType(.cdata_end);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initStreaming();
    for ("<![CDATA[") |c| {
        try scanner.expectNextType(error.BufferUnderrun);
        scanner.feedInput(&.{c});
    }
    try scanner.expectNextType(.cdata);
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedInput("foo");
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "foo", error.BufferUnderrun });
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
    try scanner.expectNextType(.cdata_end);
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextType(.eof);
}

test "Scanner Character/Entity References" {
    var scanner: CheckedScanner = undefined;

    scanner = CheckedScanner.initComplete("&abc;");
    try scanner.expectNextType(.reference);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "abc", null });
    try scanner.expectNextType(.reference_end);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("&;");
    try scanner.expectNextType(.reference);
    try scanner.expectNextType(.reference_end);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("&");
    try scanner.expectNextType(.reference);
    try scanner.expectNextType(.reference_end_invalid);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("&foo");
    try scanner.expectNextType(.reference);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try scanner.expectNextType(.reference_end_invalid);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("&foo ");
    try scanner.expectNextType(.reference);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try scanner.expectNextType(.reference_end_invalid);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ " ", null });
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initStreaming();
    scanner.feedInput("&");
    try scanner.expectNextType(.reference);
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedInput(";");
    try scanner.expectNextType(.reference_end);
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initStreaming();
    scanner.feedInput("&");
    try scanner.expectNextType(.reference);
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedInput("foo");
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", error.BufferUnderrun });
    scanner.feedInput("bar");
    try scanner.expectNextStringSeq(&.{ "bar", error.BufferUnderrun });
    scanner.feedEof();
    try scanner.expectNextString(null);
    try scanner.expectNextType(.reference_end_invalid);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initStreaming();
    scanner.feedInput("&");
    try scanner.expectNextType(.reference);
    try scanner.expectNextType(error.BufferUnderrun);
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

    scanner = CheckedScanner.initStreaming();
    scanner.feedInput("&");
    try scanner.expectNextType(.reference);
    try scanner.expectNextType(error.BufferUnderrun);
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
    var scanner: CheckedScanner = undefined;

    scanner = CheckedScanner.initComplete("");
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initStreaming();
    scanner.feedEof();
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("foo bar");
    try scanner.expectNextType(.text_data);
    try scanner.expectNextString("foo bar");
    try scanner.expectNextString(null);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("foo bar");
    try scanner.expectNextType(.text_data);
    try scanner.expectNextString("foo bar");
    try scanner.expectNextString(null);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initStreaming();
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

    scanner = CheckedScanner.initComplete("]]>");
    try scanner.expectNextType(.cdata_end);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initStreaming();
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

    scanner = CheckedScanner.initComplete("foo]]> bar");
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "foo", null });
    try scanner.expectNextType(.cdata_end);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ " bar", null });
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initStreaming();
    scanner.feedInput("]");
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "]", null });
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initStreaming();
    scanner.feedInput("]]");
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "]]", null });
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initStreaming();
    scanner.feedInput("]]>");
    try scanner.expectNextType(.cdata_end);
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextType(.eof);
}

test "Scanner Element Closing Tags" {
    var scanner: CheckedScanner = undefined;

    scanner = CheckedScanner.initComplete("</foo>");
    try scanner.expectNextType(.element_close);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("</foo >");
    try scanner.expectNextType(.element_close);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("</ >");
    try scanner.expectNextType(.element_close);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("</>");
    try scanner.expectNextType(.element_close);
    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextType(.eof);
}

test "Scanner Element Opening Tags" {
    var scanner: CheckedScanner = undefined;

    scanner = CheckedScanner.initComplete("<");
    try scanner.expectNextType(.element_open);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("<foo  />");
    try scanner.expectNextType(.element_open);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ "  ", null });
    try scanner.expectNextType(.tag_slash_close);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("<foo  >");
    try scanner.expectNextType(.element_open);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ "  ", null });
    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("<foo  / >");
    try scanner.expectNextType(.element_open);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ "  ", null });
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "/", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("<foo bar='fizz'>");
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

    scanner = CheckedScanner.initComplete("<foo bar = 'fizz' >");
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

    scanner = CheckedScanner.initComplete("<foo bar=\"fizz\">");
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

    scanner = CheckedScanner.initComplete("<foo bar='&baz;'>");
    try scanner.expectNextType(.element_open);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "bar", null });
    try scanner.expectNextType(.attr_eql);
    try scanner.expectNextType(.quote_single);
    try scanner.expectNextType(.reference);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "baz", null });
    try scanner.expectNextType(.reference_end);
    try scanner.expectNextType(.quote_single);
    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("<foo  bar='fizz&baz;buzz'>");
    try scanner.expectNextType(.element_open);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ "  ", null });
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "bar", null });
    try scanner.expectNextType(.attr_eql);
    try scanner.expectNextType(.quote_single);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "fizz", null });
    try scanner.expectNextType(.reference);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "baz", null });
    try scanner.expectNextType(.reference_end);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "buzz", null });
    try scanner.expectNextType(.quote_single);
    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initStreaming();
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
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "ba", error.BufferUnderrun });
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
    var scanner: CheckedScanner = undefined;

    scanner = CheckedScanner.initComplete("<!DOCTYPE");
    try scanner.expectNextType(.dtd);
    try scanner.expectNextType(.eof);
    scanner = CheckedScanner.initComplete("<!DOCTYPE>");
    try scanner.expectNextType(.dtd);
    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("<!DOCTYPEfoo");
    try scanner.expectNextType(.dtd);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initStreaming();
    scanner.feedInput("<!DOCTYPEfoo");
    try scanner.expectNextType(.dtd);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", error.BufferUnderrun });
    scanner.feedEof();
    try scanner.expectNextString(null);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("<!DOCTYPE foo SYSTEM 'bar");
    try scanner.expectNextType(.dtd);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "SYSTEM", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextType(.quote_single);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "bar", null });
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("<!DOCTYPE foo SYSTEM 'bar' >");
    try scanner.expectNextType(.dtd);

    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "SYSTEM", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });

    try scanner.expectNextType(.quote_single);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "bar", null });
    try scanner.expectNextType(.quote_single);

    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("<!DOCTYPE []>");
    try scanner.expectNextType(.dtd);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextType(.dtd_int_subset);
    try scanner.expectNextType(.dtd_int_subset_end);
    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("<!DOCTYPE [ asdf ]>");
    try scanner.expectNextType(.dtd);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextType(.dtd_int_subset);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTypeStringSeq(.invalid_token, &.{ "asdf", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextType(.dtd_int_subset_end);
    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("<!DOCTYPE [ asdf%foo; ]>");
    try scanner.expectNextType(.dtd);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextType(.dtd_int_subset);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTypeStringSeq(.invalid_token, &.{ "asdf", null });
    try scanner.expectNextType(.pe_reference);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try scanner.expectNextType(.reference_end);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextType(.dtd_int_subset_end);
    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete("<!DOCTYPE foo PUBLIC 'bar' [\n\n] >");
    try scanner.expectNextType(.dtd);

    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "PUBLIC", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });

    try scanner.expectNextType(.quote_single);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "bar", null });
    try scanner.expectNextType(.quote_single);

    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });

    try scanner.expectNextType(.dtd_int_subset);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ "\n\n", null });
    try scanner.expectNextType(.dtd_int_subset_end);

    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });

    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete(
        \\<!DOCTYPE [
        \\  <!ELEMENT foo EMPTY>
        \\]>
    );
    try scanner.expectNextType(.dtd);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextType(.dtd_int_subset);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ "\n  ", null });
    try scanner.expectNextType(.element_decl);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "foo", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "EMPTY", null });
    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ "\n", null });
    try scanner.expectNextType(.dtd_int_subset_end);
    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextType(.eof);

    scanner = CheckedScanner.initComplete(
        \\<!DOCTYPE[
        \\  <!ELEMENT br EMPTY>
        \\  <!ELEMENT p (#PCDATA|emph)* >
        \\  <!ELEMENT %name.para; %content.para; >
        \\]>
        \\
    );
    try scanner.expectNextType(.dtd);
    try scanner.expectNextType(.dtd_int_subset);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ "\n  ", null });

    try scanner.expectNextType(.element_decl);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "br", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "EMPTY", null });
    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ "\n  ", null });

    try scanner.expectNextType(.element_decl);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "p", null });
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextType(.dtd_lparen);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "#PCDATA", null });
    try scanner.expectNextType(.dtd_pipe);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "emph", null });
    try scanner.expectNextType(.dtd_rparen);
    try scanner.expectNextType(.dtd_asterisk);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ "\n  ", null });

    try scanner.expectNextType(.element_decl);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextType(.pe_reference);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "name.para", null });
    try scanner.expectNextType(.reference_end);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextType(.pe_reference);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "content.para", null });
    try scanner.expectNextType(.reference_end);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextType(.tag_basic_close);
    try scanner.expectNextTypeStringSeq(.tag_whitespace, &.{ "\n", null });

    try scanner.expectNextType(.dtd_int_subset_end);
    try scanner.expectNextType(.tag_basic_close);

    try scanner.expectNextTypeStringSeq(.text_data, &.{ "\n", null });
    try scanner.expectNextType(.eof);
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

    var scanner = CheckedScanner.initStreaming();
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
    try scanner.expectNextString(null);
    try scanner.expectNextType(.pi_end);

    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "\n\n", error.BufferUnderrun });
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextString(null);

    try scanner.expectNextType(.element_open);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "f", error.BufferUnderrun });
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextStringSeq(&.{ "oo", error.BufferUnderrun });
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextString(null);
    try scanner.expectNextType(.tag_basic_close);

    try scanner.expectNextTypeStringSeq(.text_data, &.{ "\n", error.BufferUnderrun });

    for (comptime &[_]*const [2:0]u8{
        "  ", "Lo", "re", "m ", "ip", "su", "m\n", "  ",
    }) |expected| {
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
    try scanner.expectNextType(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextTypeStringSeq(.tag_token, &.{ "fo", error.BufferUnderrun });
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextStringSeq(&.{ "o", null });
    try scanner.expectNextType(.tag_basic_close);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextTypeStringSeq(.text_data, &.{ "\n", error.BufferUnderrun });
    scanner.feedEof();
    try scanner.expectNextString(null);
    try scanner.expectNextType(.eof);
}
