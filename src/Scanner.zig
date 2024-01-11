const std = @import("std");
const assert = std.debug.assert;

const Scanner = @This();
src: []const u8,
index: usize,
state: State,
flags: Flags,

/// The set of codepoints defined as whitespace. They are all
/// exactly one byte in size.
pub const whitespace_set: []const u8 = &[_]u8{
    '\u{20}',
    '\u{09}',
    '\u{0D}',
    '\u{0A}',
};

/// Initializes the `Scanner` with the full input.
/// Calling `feedInput` or `feedEof` is illegal.
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
        .state = .check_next_tok_type,
        .flags = .{
            .eof_specified = false,
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

// TODO: revise the comments on all of these tags to describe
// meaning and expectations in a more consistent manner.
pub const TokenType = enum {
    /// The end of the XML source.
    eof,

    /// Encountered a sequence of characters that formed invalid markup.
    /// Call `nextString` until it returns null to retrieve the invalid
    /// markup string.
    /// If the markup was expected to be an opening or closing tag ('</', '<--', '/>', '<![CDATA[', etc),
    /// scanning will proceed scanning assuming text data.
    /// If the invalid markup is inside a markup tag, tokenization will continue under the same pretense.
    // TODO: remember to document about the fact that a function can be called to replace the text data
    // assumption.
    invalid_token,

    /// Indicates whitespace in a markup tag. Call `nextString` until it
    /// returns null to retrieve the whitespace.
    tag_whitespace,

    /// The opening of a character/entity reference '&' in the
    /// midst of regular text data.
    ///
    /// Call `nextString` repeatedly to get segments of the numeric
    /// value or entity name token until it returns `null`.
    ///
    /// No validation is  performed.
    char_ent_ref,
    /// Plain non-markup text data.
    ///
    /// Call `nextString` repeatedly to get segments of the text data,
    /// until it returns `null`.
    ///
    /// If there are any invalid ']]>' tokens in the midst of the text, it will be
    /// tokenized as normal, and in addition is guaranteed to be returned as a
    /// singular string, such that the error can be easily flagged and deferred
    /// to later code.
    text_data,

    /// The markup tag is a PI (Processing Instructions) tag ('<?').
    /// Call `nextString` to get segments of the PI target name until
    /// it returns null.
    pi_target,

    /// The data that comes after the PI target, up to the '?>' token.
    pi_data,

    /// The opening tag '<!--'.
    /// Call `nextString` to get segments of the comment text.
    ///
    /// After `nextString` returns null, `nextTokenType` should be called next,
    /// which will return one of:
    /// * `.invalid_comment_dash_dash`: there is an invalid token '--' in the middle of the string,
    ///   and `nextString` should be called again, repeating the process.
    /// * `.invalid_comment_end_triple_dash`: the comment ends with the invalid token '--->'.
    /// * `.comment_end`: the comment ends with the token '-->'.
    comment,
    /// The invalid token '--'.
    /// This can be ignored, and resume calling `nextString` as described for `.comment`,
    /// hence the error may be reported at a later time when it can be discerned
    /// into possibly more useful information, or grouped with other errors.
    invalid_comment_dash_dash,
    /// Indicates '--->' after a comment. This is an error, but can be deferred
    /// to later, and tokenization may proceed.
    invalid_comment_end_triple_dash,
    /// Indicates '-->' after a comment.
    comment_end,

    /// Encountered token '<!DOCTYPE'.
    /// The next two tokens should be `.tag_whitespace` and then `.dtd_name`,
    /// in a valid document, however `<!DOCTYPEname` will be tokenized as one would expect.
    dtd,

    /// Any unquoted tokens in the midst of the DOCTYPE decl. This includes the identifier,
    /// the 'SYSTEM' and 'PUBLIC' tokens, and any other non-whitespace character data.
    /// Call `nextString` until it returns null to get the token string.
    /// Validation of the tokens is to be done by the caller.
    dtd_token,
    /// The start of a literal in the DOCTYPE declaration, starting with a single quote ('\'').
    /// Call `nextString` until it returns null to get the contents of the literal.
    dtd_literal_quote_single,
    /// Equivalent to `.dtd_literal_quote_single`, but with a double quote ('\"').
    dtd_literal_quote_double,
    /// Represents the matching quote to the latest doctype literal quote.
    dtd_literal_end,
    /// Encountered the '[' token, indicating the start of the DTD Internal Subset.
    dtd_int_subset,
    /// Encountered the '%' token, indicating the start of a Parsed Entity Reference.
    /// Call `nextString` until it returns null to get the entity name.
    dtd_int_subset_pe_ref,
    /// Encountered the '<!ELEMENT' token, indicating the start of an element decl.
    dtd_int_subset_element,
    /// Encountered the '>' token, indicating the end of the element decl.
    dtd_int_subset_element_end,
    /// Encountered the '<!ENTITY' token, indicating the start of an entity decl.
    dtd_int_subset_entity,
    /// Encountered the '>' token, indicating the end of the entity decl.
    dtd_int_subset_entity_end,
    /// Encountered the '<!ATTLIST', indicating the start of an attribute list decl.
    dtd_int_subset_attlist,
    /// Encountered the '>' token, indicating the end of the attribute list decl.
    dtd_int_subset_attlist_end,
    /// Encountered the ']' token, terminating the DTD Internal Subset.
    dtd_int_subset_end,

    /// Indicates the end of the DTD (the '>' token).
    dtd_end,

    /// The markup tag is a CDATA Section ('<![CDATA[').
    ///
    /// Call `nextString` to get the CDATA text segments until it returns `null`.
    cdata,
    /// Indicates ']]>' without a preceding '<![CDATA[' token.
    /// This can be ignored, and resume calling `nextString` as described for `.text_data`,
    /// hence the error may be reported at a later time when it can be discerned
    /// into possibly more useful information, or grouped with other errors.
    invalid_cdata_stray_end,

    /// Indicates the start of an element tag `'<' Name`.
    /// Call `nextString` to get segments of the element name until it returns `null`.
    element_open,
    /// Indicates '>', terminating the latest `.element_open` tag.
    element_open_end,
    /// Indicates '/>', terminating the latest `.element_open` tag and closing
    /// the element inline.
    element_open_end_close_inline,

    /// Indicates an attribute name.
    /// Call `nextString` to get the segments of the attribute name until it returns `null`.
    /// No validation is performed on the name, it is simply a string of non-whitespace,
    /// non-quote and non-equals symbols. Invalid invariants are left to the caller to
    /// verify.
    attr_name,
    /// Indicates '=' in an element tag.
    attr_eql,
    /// Indicates the start of a quoted attribute value (single quotes).
    /// First call `nextString` until it returns null, and then `nextTokenType` will
    /// either return:
    /// * `.attr_value_end`: indicating the end of the attribute value.
    /// * `.char_ent_ref`: indicating a character/entity reference in the attribute value,
    ///   which should be tokenized as normal, before calling `nextString` again, and
    ///   repeating the process.
    attr_value_quote_single,
    /// Indicates the start of a quoted attribute value (double quotes).
    /// Equivalent to `.attr_value_quote_single`, save for the quotes.
    attr_value_quote_double,
    /// Indicates the end of a quoted attribute value, with whether
    /// it's a single or double quote being based off of the latest
    /// opened attribute value quote.
    attr_value_end,

    /// Indicates the start of an element closing tag `'</' Name`.
    /// Call `nextString` to get segments of the element name until it returns `null`.
    /// Unlike for `element_open`, this may include whitespace after the name,
    /// and any other stray characters before the '>', for the purposes of allowing
    /// the caller to handle invalid invariants of `</.*>`.
    element_close,

    /// Whether or not this token should be followed up by a call to `nextString`.
    /// `.eof` returns false.
    pub inline fn hasString(token_type: TokenType) bool {
        return switch (token_type) {
            .invalid_token,
            .tag_whitespace,
            .char_ent_ref,
            .text_data,
            .pi_target,
            .pi_data,
            .comment,
            .invalid_comment_dash_dash,
            .cdata,

            .element_open,
            .attr_name,
            .attr_value_quote_single,
            .attr_value_quote_double,
            .element_close,

            .dtd_token,
            .dtd_literal_quote_single,
            .dtd_literal_quote_double,
            .dtd_int_subset_pe_ref,
            => true,

            .eof,
            .invalid_comment_end_triple_dash,
            .comment_end,
            .invalid_cdata_stray_end,

            .element_open_end,
            .element_open_end_close_inline,
            .attr_eql,
            .attr_value_end,

            .dtd,
            .dtd_literal_end,
            .dtd_int_subset,
            .dtd_end,
            .dtd_int_subset_end,
            .dtd_int_subset_element,
            .dtd_int_subset_element_end,
            .dtd_int_subset_entity,
            .dtd_int_subset_entity_end,
            .dtd_int_subset_attlist,
            .dtd_int_subset_attlist_end,
            => false,
        };
    }
};

pub const BufferError = error{BufferUnderrun};
pub const EofError = error{PrematureEof};
pub const BufferOrEofError = BufferError || EofError;

pub const NextTokenTypeError = BufferOrEofError;
/// See the comments on the data type tag for more information.
pub fn nextTokenType(scanner: *Scanner) NextTokenTypeError!TokenType {
    switch (scanner.state) {
        .check_next_tok_type => {
            if (scanner.index == scanner.src.len) {
                if (!scanner.flags.eof_specified) return error.BufferUnderrun;
                return .eof;
            }
            switch (scanner.src[scanner.index]) {
                '<' => {
                    scanner.state = .markup_tag;
                    scanner.index += 1;
                    // return @call(.always_tail, nextTokenType, .{scanner});
                    return scanner.nextTokenType();
                },
                '&' => {
                    scanner.state = .ref_segment;
                    scanner.index += 1;
                    return .char_ent_ref;
                },
                else => {
                    scanner.state = .text_data;
                    return .text_data;
                },
            }
        },

        .text_data => unreachable,
        .@"text_data,]" => unreachable,
        .@"text_data,]]" => unreachable,
        .@"text_data,]]>" => unreachable,
        .text_data_end => unreachable,
        .text_data_end_eof => unreachable,

        .ref_segment => unreachable,
        .ref_semicolon => unreachable,

        .markup_tag => {
            if (scanner.index == scanner.src.len) {
                if (!scanner.flags.eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            switch (scanner.src[scanner.index]) {
                '?' => {
                    scanner.state = .pi_target;
                    scanner.index += 1;
                    return .pi_target;
                },
                '!' => {
                    scanner.state = .@"<!";
                    scanner.index += 1;
                    // return @call(.always_tail, nextTokenType, .{scanner});
                    return scanner.nextTokenType();
                },
                '/' => {
                    scanner.state = .@"</";
                    scanner.index += 1;
                    return .element_close;
                },
                else => {
                    scanner.state = .element_open;
                    return .element_open;
                },
            }
        },

        .pi_target => unreachable,
        .pi_target_qm => unreachable,
        .pi_target_end, .pi_data => return .pi_data,
        .pi_data_qm => unreachable,

        .@"<!" => {
            if (scanner.index == scanner.src.len) {
                if (!scanner.flags.eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            switch (scanner.src[scanner.index]) {
                '-' => {
                    scanner.state = .@"<!-";
                    scanner.index += 1;
                    // return @call(.always_tail, nextTokenType, .{scanner});
                    return scanner.nextTokenType();
                },
                '[' => {
                    scanner.state = .@"<![";
                    scanner.index += 1;
                    // return @call(.always_tail, nextTokenType, .{scanner});
                    return scanner.nextTokenType();
                },
                'D' => {
                    scanner.state = .@"<!D";
                    scanner.index += 1;
                    return scanner.nextTokenType();
                },
                else => return .invalid_token,
            }
        },

        .@"<!-" => {
            if (scanner.index == scanner.src.len) {
                if (!scanner.flags.eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            switch (scanner.src[scanner.index]) {
                '-' => {
                    scanner.state = .comment;
                    scanner.index += 1;
                    return .comment;
                },
                else => return .invalid_token,
            }
        },
        .comment => unreachable,
        .@"comment,-" => unreachable,
        .@"comment,--" => {
            assert(scanner.index != scanner.src.len);
            scanner.state = .comment;
            return .invalid_comment_dash_dash;
        },
        .@"comment,-->" => {
            scanner.state = .check_next_tok_type;
            return .comment_end;
        },
        .@"comment,---" => {
            scanner.state = .@"comment,-";
            return .invalid_comment_dash_dash;
        },
        .@"comment,--->" => {
            scanner.state = .check_next_tok_type;
            return .invalid_comment_end_triple_dash;
        },

        inline //
        .@"<!D",
        .@"<!DO",
        .@"<!DOC",
        .@"<!DOCT",
        .@"<!DOCTY",
        => |tag| {
            if (scanner.index == scanner.src.len) {
                if (!scanner.flags.eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            const next_state: State, const expected_char: u8 = comptime switch (tag) {
                .@"<!D" => .{ .@"<!DO", 'O' },
                .@"<!DO" => .{ .@"<!DOC", 'C' },
                .@"<!DOC" => .{ .@"<!DOCT", 'T' },
                .@"<!DOCT" => .{ .@"<!DOCTY", 'Y' },
                .@"<!DOCTY" => .{ .@"<!DOCTYP", 'P' },
                else => unreachable,
            };
            if (scanner.src[scanner.index] != expected_char) return .invalid_token;
            scanner.state = next_state;
            scanner.index += 1;
            // return @call(.always_tail, nextTokenType, .{scanner});
            return scanner.nextTokenType();
        },
        .@"<!DOCTYP" => {
            if (scanner.index == scanner.src.len) {
                if (!scanner.flags.eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            if (scanner.src[scanner.index] != 'E') return .invalid_token;
            scanner.state = .dtd_decl;
            scanner.index += 1;
            return .dtd;
        },
        .dtd_decl => {
            if (scanner.index == scanner.src.len) {
                if (!scanner.flags.eof_specified) return error.BufferUnderrun;
                scanner.state = .check_next_tok_type;
                // return @call(.always_tail, nextTokenType, .{scanner});
                return scanner.nextTokenType();
            }
            switch (scanner.src[scanner.index]) {
                '>' => {
                    scanner.state = .check_next_tok_type;
                    scanner.index += 1;
                    return .dtd_end;
                },
                '\'' => {
                    scanner.state = .dtd_quote_single;
                    scanner.index += 1;
                    return .dtd_literal_quote_single;
                },
                '\"' => {
                    scanner.state = .dtd_quote_double;
                    scanner.index += 1;
                    return .dtd_literal_quote_double;
                },
                '[' => {
                    scanner.state = .dtd_int_subset;
                    scanner.index += 1;
                    return .dtd_int_subset;
                },
                else => {
                    if (std.mem.indexOfScalar(u8, whitespace_set, scanner.src[scanner.index]) != null) {
                        scanner.state = .dtd_whitespace;
                        return .tag_whitespace;
                    }
                    scanner.state = .dtd_token;
                    return .dtd_token;
                },
            }
        },
        .dtd_whitespace => unreachable,
        .dtd_token => unreachable,
        .dtd_quote_single,
        .dtd_quote_double,
        => |tag| {
            if (scanner.index == scanner.src.len) {
                assert(scanner.flags.eof_specified);
                scanner.state = .check_next_tok_type;
                // return @call(.always_tail, nextTokenType, .{scanner});
                return scanner.nextTokenType();
            }
            const matching_quote: u8 = switch (tag) {
                .dtd_quote_single => '\'',
                .dtd_quote_double => '\"',
                else => unreachable,
            };
            assert(scanner.src[scanner.index] == matching_quote);
            scanner.state = .dtd_decl;
            scanner.index += 1;
            return .dtd_literal_end;
        },
        .dtd_int_subset => {
            if (scanner.index == scanner.src.len) {
                if (scanner.flags.eof_specified) return error.BufferUnderrun;
                scanner.state = .dtd_decl;
                // return @call(.always_tail, nextTokenType, .{scanner});
                return scanner.nextTokenType();
            }
            switch (scanner.src[scanner.index]) {
                '<' => {
                    scanner.state = .@"dtd_int_subset:<";
                    scanner.index += 1;
                    // return @call(.always_tail, nextTokenType, .{scanner});
                    return scanner.nextTokenType();
                },
                '%' => {
                    scanner.state = .dtd_int_subset_pe_ref;
                    scanner.index += 1;
                    return .dtd_int_subset_pe_ref;
                },
                ']' => {
                    scanner.state = .dtd_decl;
                    scanner.index += 1;
                    return .dtd_int_subset_end;
                },
                else => if (std.mem.indexOfScalar(u8, whitespace_set, scanner.src[scanner.index]) != null) {
                    scanner.state = .dtd_int_subset_whitespace;
                    return .tag_whitespace;
                } else {
                    scanner.state = .dtd_int_subset_invalid;
                    return .invalid_token;
                },
            }
        },
        .dtd_int_subset_whitespace => unreachable,
        .dtd_int_subset_pe_ref => unreachable,
        .dtd_int_subset_invalid => unreachable,

        .@"dtd_int_subset:<" => {
            if (scanner.index == scanner.src.len) {
                if (!scanner.flags.eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            if (scanner.src[scanner.index] != '!') {
                return .invalid_token;
            }
            scanner.state = .@"dtd_int_subset:<!";
            scanner.index += 1;
            // return @call(.always_tail, nextTokenType, .{scanner});
            return scanner.nextTokenType();
        },

        .@"dtd_int_subset:<!" => {
            if (scanner.index == scanner.src.len) {
                if (!scanner.flags.eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            switch (scanner.src[scanner.index]) {
                'E' => {
                    scanner.state = .@"dtd_int_subset:<!E";
                    scanner.index += 1;
                    // return @call(.always_tail, nextTokenType, .{scanner});
                    return scanner.nextTokenType();
                },
                'A' => {
                    scanner.state = .@"dtd_int_subset:<!A";
                    scanner.index += 1;
                    // return @call(.always_tail, nextTokenType, .{scanner});
                    return scanner.nextTokenType();
                },
                else => return .invalid_token,
            }
        },

        .@"dtd_int_subset:<!E" => {
            if (scanner.index == scanner.src.len) {
                if (!scanner.flags.eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            switch (scanner.src[scanner.index]) {
                'L' => {
                    scanner.state = .@"dtd_int_subset:<!EL";
                    scanner.index += 1;
                    // return @call(.always_tail, nextTokenType, .{scanner});
                    return scanner.nextTokenType();
                },
                'N' => {
                    scanner.state = .@"dtd_int_subset:<!EN";
                    scanner.index += 1;
                    // return @call(.always_tail, nextTokenType, .{scanner});
                    return scanner.nextTokenType();
                },
                else => return .invalid_token,
            }
        },

        .@"dtd_int_subset:<!EL",
        .@"dtd_int_subset:<!ELE",
        .@"dtd_int_subset:<!ELEM",
        .@"dtd_int_subset:<!ELEME",
        => |tag| {
            if (scanner.index == scanner.src.len) {
                if (!scanner.flags.eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            const next_state: State, const expected_char: u8 = switch (tag) {
                .@"dtd_int_subset:<!EL" => .{ .@"dtd_int_subset:<!ELE", 'E' },
                .@"dtd_int_subset:<!ELE" => .{ .@"dtd_int_subset:<!ELEM", 'M' },
                .@"dtd_int_subset:<!ELEM" => .{ .@"dtd_int_subset:<!ELEME", 'E' },
                .@"dtd_int_subset:<!ELEME" => .{ .@"dtd_int_subset:<!ELEMEN", 'N' },
                else => unreachable,
            };
            if (scanner.src[scanner.index] != expected_char) return .invalid_token;
            scanner.state = next_state;
            scanner.index += 1;
            // return @call(.always_tail, nextTokenType, .{scanner});
            return scanner.nextTokenType();
        },
        .@"dtd_int_subset:<!ELEMEN" => {
            if (scanner.index == scanner.src.len) {
                if (!scanner.flags.eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            if (scanner.src[scanner.index] != 'T') return .invalid_token;
            scanner.state = .dtd_int_subset_element;
            scanner.index += 1;
            return .dtd_int_subset_element;
        },

        .@"dtd_int_subset:<!EN",
        .@"dtd_int_subset:<!ENT",
        .@"dtd_int_subset:<!ENTI",
        => |tag| {
            if (scanner.index == scanner.src.len) {
                if (!scanner.flags.eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            const next_state: State, const expected_char: u8 = switch (tag) {
                .@"dtd_int_subset:<!EN" => .{ .@"dtd_int_subset:<!ENT", 'T' },
                .@"dtd_int_subset:<!ENT" => .{ .@"dtd_int_subset:<!ENTI", 'I' },
                .@"dtd_int_subset:<!ENTI" => .{ .@"dtd_int_subset:<!ENTIT", 'T' },
                else => unreachable,
            };
            if (scanner.src[scanner.index] != expected_char) return .invalid_token;
            scanner.state = next_state;
            scanner.index += 1;
            // return @call(.always_tail, nextTokenType, .{scanner});
            return scanner.nextTokenType();
        },
        .@"dtd_int_subset:<!ENTIT" => {
            if (scanner.index == scanner.src.len) {
                if (!scanner.flags.eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            if (scanner.src[scanner.index] != 'Y') return .invalid_token;
            scanner.state = .dtd_int_subset_entity;
            scanner.index += 1;
            return .dtd_int_subset_entity;
        },

        .@"dtd_int_subset:<!A",
        .@"dtd_int_subset:<!AT",
        .@"dtd_int_subset:<!ATT",
        .@"dtd_int_subset:<!ATTL",
        .@"dtd_int_subset:<!ATTLI",
        => |tag| {
            if (scanner.index == scanner.src.len) {
                if (!scanner.flags.eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            const next_state: State, const expected_char: u8 = switch (tag) {
                .@"dtd_int_subset:<!EN" => .{ .@"dtd_int_subset:<!ENT", 'T' },
                .@"dtd_int_subset:<!ENT" => .{ .@"dtd_int_subset:<!ENTI", 'I' },
                .@"dtd_int_subset:<!ENTI" => .{ .@"dtd_int_subset:<!ENTIT", 'T' },
                else => unreachable,
            };
            if (scanner.src[scanner.index] != expected_char) return .invalid_token;
            scanner.state = next_state;
            scanner.index += 1;
            // return @call(.always_tail, nextTokenType, .{scanner});
            return scanner.nextTokenType();
        },
        .@"dtd_int_subset:<!ATTLIS" => {
            if (scanner.index == scanner.src.len) {
                if (!scanner.flags.eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            if (scanner.src[scanner.index] != 'T') return .invalid_token;
            scanner.state = .dtd_int_subset_attlist;
            scanner.index += 1;
            return .dtd_int_subset_attlist;
        },

        .dtd_int_subset_element => {
            if (scanner.index == scanner.src.len) {
                if (!scanner.flags.eof_specified) return error.BufferUnderrun;
                scanner.state = .check_next_tok_type;
                // return @call(.always_tail, nextTokenType, .{scanner});
                return scanner.nextTokenType();
            }
            switch (scanner.src[scanner.index]) {
                '(' => @panic("TODO"),
                ')' => @panic("TODO"),
                ',' => @panic("TODO"),
                '%' => @panic("TODO"),

                '>' => {
                    scanner.state = .dtd_int_subset;
                    scanner.index += 1;
                    return .dtd_int_subset_element_end;
                },

                else => {
                    if (std.mem.indexOfScalar(u8, whitespace_set, scanner.src[scanner.index]) != null) {
                        scanner.state = .dtd_int_subset_element_whitespace;
                        return .tag_whitespace;
                    }
                    scanner.state = .dtd_int_subset_element_token;
                    return .dtd_token;
                },
            }
        },
        .dtd_int_subset_element_whitespace => unreachable,
        .dtd_int_subset_element_token => unreachable,

        .dtd_int_subset_entity => @panic("TODO"),
        .dtd_int_subset_entity_whitespace => unreachable,
        .dtd_int_subset_entity_token => unreachable,

        .dtd_int_subset_attlist => @panic("TODO"),
        .dtd_int_subset_attlist_whitespace => unreachable,
        .dtd_int_subset_attlist_token => unreachable,

        inline //
        .@"<![",
        .@"<![C",
        .@"<![CD",
        .@"<![CDA",
        .@"<![CDAT",
        => |tag| {
            if (scanner.index == scanner.src.len) {
                if (!scanner.flags.eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            const next_state: State, const expected_char: u8 = comptime switch (tag) {
                .@"<![" => .{ .@"<![C", 'C' },
                .@"<![C" => .{ .@"<![CD", 'D' },
                .@"<![CD" => .{ .@"<![CDA", 'A' },
                .@"<![CDA" => .{ .@"<![CDAT", 'T' },
                .@"<![CDAT" => .{ .@"<![CDATA", 'A' },
                else => unreachable,
            };
            if (scanner.src[scanner.index] != expected_char) return .invalid_token;
            scanner.state = next_state;
            scanner.index += 1;
            // return @call(.always_tail, nextTokenType, .{scanner});
            return scanner.nextTokenType();
        },
        .@"<![CDATA" => {
            if (scanner.index == scanner.src.len) {
                if (!scanner.flags.eof_specified) return error.BufferUnderrun;
                return .invalid_token;
            }
            if (scanner.src[scanner.index] != '[') return .invalid_token;
            scanner.state = .cdata;
            scanner.index += 1;
            return .cdata;
        },
        .cdata => unreachable,
        .@"cdata,]" => unreachable,
        .@"cdata,]]" => unreachable,

        .element_open => unreachable,
        .element_open_name_end => {
            try scanner.checkForEof();
            if (std.mem.indexOfScalar(u8, whitespace_set, scanner.src[scanner.index]) != null) {
                scanner.state = .element_open_name_ws;
                return .tag_whitespace;
            }
            switch (scanner.src[scanner.index]) {
                else => {
                    if (std.mem.indexOfScalar(u8, whitespace_set, scanner.src[scanner.index]) != null) {
                        scanner.state = .element_open_name_ws;
                        return .tag_whitespace;
                    }
                    scanner.state = .attr_name;
                    return .attr_name;
                },
                '=' => {
                    scanner.index += 1;
                    return .attr_eql;
                },
                '\'' => {
                    scanner.state = .attr_value_sq;
                    scanner.index += 1;
                    return .attr_value_quote_single;
                },
                '\"' => {
                    scanner.state = .attr_value_dq;
                    scanner.index += 1;
                    return .attr_value_quote_double;
                },
                '>' => {
                    scanner.state = .check_next_tok_type;
                    scanner.index += 1;
                    return .element_open_end;
                },
                '/' => {
                    scanner.state = .@"element_open,/";
                    scanner.index += 1;
                    return scanner.nextTokenType();
                },
            }
        },
        .element_open_name_ws => unreachable,

        .attr_name => unreachable,

        .attr_val_sq_ref_segment => unreachable,
        .attr_val_dq_ref_segment => unreachable,
        .attr_val_sq_ref_semicolon => unreachable,
        .attr_val_dq_ref_semicolon => unreachable,

        .attr_value_sq => unreachable,
        .attr_value_dq => unreachable,

        inline //
        .attr_value_sq_ref,
        .attr_value_dq_ref,
        => |itag| {
            scanner.state = comptime switch (itag) {
                .attr_value_sq_ref => .attr_val_sq_ref_segment,
                .attr_value_dq_ref => .attr_val_dq_ref_segment,
                else => unreachable,
            };
            return .char_ent_ref;
        },
        .attr_value_end => {
            scanner.state = .element_open_name_end;
            return .attr_value_end;
        },

        .@"element_open,/" => {
            try scanner.checkForEof();
            switch (scanner.src[scanner.index]) {
                '>' => {
                    scanner.state = .check_next_tok_type;
                    scanner.index += 1;
                    return .element_open_end_close_inline;
                },
                else => return .invalid_token,
            }
        },

        .@"</" => unreachable,
        .element_close_end => unreachable,
    }
}

pub const NextStringError = BufferOrEofError;
/// The returned string is valid for as long as `scanner.src` is valid.
/// Calling this after it returns null is illegal behaviour.
// TODO: Make calling this when it's already returned null be checked illegal behaviour.
// TODO: Use `@call(.always_tail, nextString, .{scanner})` in this function once that
// works for this return type.
pub fn nextString(scanner: *Scanner) NextStringError!?[]const u8 {
    switch (scanner.state) {
        .check_next_tok_type => return null,

        .text_data => {
            try scanner.checkForEof();
            const str_start = scanner.index;
            while (std.mem.indexOfAnyPos(u8, scanner.src, scanner.index, &.{ '&', '<', ']' })) |idx| {
                scanner.index = idx;
                switch (scanner.src[scanner.index]) {
                    '<', '&' => {
                        if (str_start != idx) break;
                        scanner.state = .text_data_end;
                        // return @call(.always_tail, nextString, .{scanner});
                        return scanner.nextString();
                    },
                    ']' => {},
                    else => unreachable,
                }

                scanner.index += 1;
                if (scanner.index == scanner.src.len) {
                    if (scanner.flags.eof_specified) {
                        scanner.state = .text_data_end;
                        break;
                    }
                    scanner.state = .@"text_data,]";
                    // if (str_start == idx) return @call(.always_tail, nextString, .{scanner});
                    if (str_start == idx) return scanner.nextString();
                    return scanner.src[str_start..idx];
                }
                if (scanner.src[scanner.index] != ']') continue;

                scanner.index += 1;
                if (scanner.index == scanner.src.len) {
                    if (scanner.flags.eof_specified) {
                        scanner.state = .text_data_end;
                        break;
                    }
                    scanner.state = .@"text_data,]]";
                    if (str_start == idx) return scanner.nextString();
                    return scanner.src[str_start..idx];
                }
                if (scanner.src[scanner.index] != '>') continue;
                scanner.state = .@"text_data,]]>";
                scanner.index += 1;
                if (str_start != idx) return scanner.src[str_start..idx];
                // return @call(.always_tail, nextString, .{scanner});
                return scanner.nextString();
            } else {
                scanner.state = .text_data_end_eof;
                scanner.index = scanner.src.len;
            }
            return scanner.src[str_start..scanner.index];
        },
        .@"text_data,]" => {
            if (scanner.index == scanner.src.len) {
                if (!scanner.flags.eof_specified) return error.BufferUnderrun;
                scanner.state = .text_data_end;
                return "]";
            }
            if (scanner.src[scanner.index] != ']') {
                scanner.state = .text_data;
                return "]";
            }
            scanner.state = .@"cdata,]]";
            scanner.index += 1;
            // return @call(.always_tail, nextString, .{scanner});
            return scanner.nextString();
        },
        .@"text_data,]]" => {
            if (scanner.index == scanner.src.len) {
                if (!scanner.flags.eof_specified) return error.BufferUnderrun;
                scanner.state = .text_data_end;
                return "]]";
            }
            switch (scanner.src[scanner.index]) {
                '>' => {
                    scanner.state = .text_data;
                    scanner.index += 1;
                    return "]]>";
                },
                ']' => {
                    scanner.index += 1;
                    return "]";
                },
                else => {
                    scanner.state = .text_data;
                    return "]]";
                },
            }
        },
        .@"text_data,]]>" => {
            if (!scanner.flags.eof_specified and scanner.index == scanner.src.len) return error.BufferUnderrun;
            scanner.state = if (scanner.flags.eof_specified and scanner.index == scanner.src.len) .text_data_end else .text_data;
            return "]]>";
        },
        .text_data_end => {
            scanner.state = .check_next_tok_type;
            return null;
        },
        .text_data_end_eof => {
            if (scanner.index != scanner.src.len) {
                scanner.state = .text_data;
                // return @call(.always_tail, nextString, .{scanner});
                return scanner.nextString();
            }
            if (scanner.flags.eof_specified) {
                scanner.state = .check_next_tok_type;
                return null;
            }
            return error.BufferUnderrun;
        },

        inline //
        .ref_segment,
        .attr_val_sq_ref_segment,
        .attr_val_dq_ref_segment,
        => |itag| {
            try scanner.checkForEof();
            const segment: State, const semicolon: State = switch (itag) {
                .ref_segment => .{ .ref_segment, .ref_semicolon },
                .attr_val_sq_ref_segment => .{ .attr_val_sq_ref_segment, .attr_val_sq_ref_semicolon },
                .attr_val_dq_ref_segment => .{ .attr_val_dq_ref_segment, .attr_val_dq_ref_semicolon },
                else => unreachable,
            };

            const str_start = scanner.index;
            const semicolon_idx = std.mem.indexOfScalarPos(u8, scanner.src, scanner.index, ';');

            scanner.state = if (semicolon_idx == null) segment else semicolon;
            scanner.index = if (semicolon_idx) |idx| idx else scanner.src.len;
            if (str_start != scanner.index) return scanner.src[str_start..scanner.index];
            // return @call(.always_tail, nextString, .{scanner});
            return scanner.nextString();
        },

        inline //
        .ref_semicolon,
        .attr_val_sq_ref_semicolon,
        .attr_val_dq_ref_semicolon,
        => |itag| {
            assert(scanner.src[scanner.index] == ';');
            scanner.state = comptime switch (itag) {
                .ref_semicolon => .check_next_tok_type,
                .attr_val_sq_ref_semicolon => .attr_value_sq,
                .attr_val_dq_ref_semicolon => .attr_value_dq,
                else => unreachable,
            };
            scanner.index += 1;
            return null;
        },

        .markup_tag => {
            scanner.state = .check_next_tok_type;
            return "<";
        },

        .pi_target => {
            try scanner.checkForEof();
            const str_start = scanner.index;
            const str_end = std.mem.indexOfAnyPos(u8, scanner.src, scanner.index, whitespace_set ++ &[_]u8{'?'}) orelse {
                scanner.index = scanner.src.len;
                return scanner.src[str_start..];
            };
            scanner.index = str_end;
            switch (scanner.src[scanner.index]) {
                '?' => {
                    scanner.state = .pi_target_qm;
                    scanner.index += 1;
                    if (str_start != str_end) return scanner.src[str_start..str_end];
                    // return @call(.always_tail, nextString, .{scanner});
                    return scanner.nextString();
                },
                else => {
                    scanner.state = .pi_target_end;
                    if (str_start != str_end) return scanner.src[str_start..str_end];
                    // return @call(.always_tail, nextString, .{scanner});
                    return scanner.nextString();
                },
            }
        },
        .pi_target_qm => {
            try scanner.checkForEof();
            switch (scanner.src[scanner.index]) {
                '>' => {
                    scanner.state = .check_next_tok_type;
                    scanner.index += 1;
                    return null;
                },
                else => {
                    scanner.state = .pi_target;
                    return "?";
                },
            }
        },
        .pi_target_end => {
            scanner.state = .pi_data;
            return null;
        },
        .pi_data => {
            try scanner.checkForEof();
            const str_start = scanner.index;
            const str_end = std.mem.indexOfScalarPos(u8, scanner.src, scanner.index, '?') orelse {
                scanner.index = scanner.src.len;
                return scanner.src[str_start..];
            };
            scanner.index = str_end + 1;
            scanner.state = .pi_data_qm;
            if (str_start != str_end) return scanner.src[str_start..str_end];
            // return @call(.always_tail, nextString, .{scanner});
            return scanner.nextString();
        },
        .pi_data_qm => {
            try scanner.checkForEof();
            switch (scanner.src[scanner.index]) {
                '>' => {
                    scanner.state = .check_next_tok_type;
                    scanner.index += 1;
                    return null;
                },
                else => {
                    scanner.state = .pi_data;
                    return "?";
                },
            }
        },

        .@"<!" => {
            scanner.state = .check_next_tok_type;
            return "<!";
        },

        .@"<!-" => {
            scanner.state = .check_next_tok_type;
            return "<!-";
        },
        .comment => {
            try scanner.checkForEof();
            const str_start = scanner.index;
            while (std.mem.indexOfScalarPos(u8, scanner.src, scanner.index, '-')) |idx| {
                scanner.state = .@"comment,-";
                scanner.index = idx + 1;
                if (str_start != idx) return scanner.src[str_start..idx];
                return scanner.nextString();
            }
            scanner.index = scanner.src.len;
            return scanner.src[str_start..scanner.index];
        },
        .@"comment,-" => {
            try scanner.checkForEof();
            switch (scanner.src[scanner.index]) {
                '-' => {
                    scanner.state = .@"comment,--";
                    scanner.index += 1;
                    // return @call(.always_tail, nextString, .{scanner});
                    return scanner.nextString();
                },
                else => {
                    scanner.state = .comment;
                    return "-";
                },
            }
        },
        .@"comment,--" => {
            try scanner.checkForEof();
            switch (scanner.src[scanner.index]) {
                '>' => {
                    scanner.state = .@"comment,-->";
                    scanner.index += 1;
                    return null;
                },
                '-' => {
                    scanner.state = .@"comment,---";
                    scanner.index += 1;
                    // return @call(.always_tail, nextString, .{scanner});
                    return scanner.nextString();
                },
                else => return null,
            }
        },
        .@"comment,-->" => unreachable,
        .@"comment,---" => {
            try scanner.checkForEof();
            switch (scanner.src[scanner.index]) {
                '>' => {
                    scanner.state = .@"comment,--->";
                    scanner.index += 1;
                    return null;
                },
                else => return null,
            }
        },
        .@"comment,--->" => return null,

        .@"<!D", .@"<!DO", .@"<!DOC", .@"<!DOCT", .@"<!DOCTY", .@"<!DOCTYP" => |tag| {
            scanner.state = .check_next_tok_type;
            return @tagName(tag);
        },
        // reached by the other `.dtd_*` branches when preparing to go back to `nextTokenType`,
        // while still needing to return a string.
        .dtd_decl => return null,
        .dtd_whitespace => {
            if (scanner.index == scanner.src.len) {
                if (!scanner.flags.eof_specified) return error.BufferUnderrun;
                scanner.state = .dtd_decl;
                return null;
            }
            const str_start = scanner.index;
            const str_end = std.mem.indexOfNonePos(u8, scanner.src, scanner.index, whitespace_set) orelse {
                scanner.index = scanner.src.len;
                return scanner.src[str_start..];
            };
            scanner.state = .dtd_decl;
            scanner.index = str_end;
            if (str_start == str_end) return null;
            return scanner.src[str_start..str_end];
        },
        .dtd_token => {
            if (scanner.index == scanner.src.len) {
                if (!scanner.flags.eof_specified) return error.BufferUnderrun;
                scanner.state = .dtd_decl;
                return null;
            }
            const str_start = scanner.index;
            const str_end = std.mem.indexOfAnyPos(u8, scanner.src, scanner.index, whitespace_set ++ &[_]u8{ '\'', '\"', '>', '[' }) orelse {
                scanner.index = scanner.src.len;
                return scanner.src[str_start..];
            };
            scanner.state = .dtd_decl;
            scanner.index = str_end;
            if (str_start == str_end) return null;
            return scanner.src[str_start..str_end];
        },
        .dtd_quote_single,
        .dtd_quote_double,
        => |tag| {
            if (scanner.index == scanner.src.len) {
                if (!scanner.flags.eof_specified) return error.BufferUnderrun;
                return null;
            }
            const matching_quote: u8 = switch (tag) {
                .dtd_quote_single => '\'',
                .dtd_quote_double => '\"',
                else => unreachable,
            };
            const str_start = scanner.index;
            const str_end = std.mem.indexOfScalarPos(u8, scanner.src, scanner.index, matching_quote) orelse {
                scanner.index = scanner.src.len;
                return scanner.src[str_start..];
            };
            scanner.index = str_end;
            if (str_start == str_end) return null;
            return scanner.src[str_start..str_end];
        },
        .dtd_int_subset => return null,
        .dtd_int_subset_whitespace => {
            try scanner.checkForEof();
            const str_start = scanner.index;
            const str_end = std.mem.indexOfNonePos(u8, scanner.src, scanner.index, whitespace_set) orelse {
                scanner.index = scanner.src.len;
                return scanner.src[str_start..];
            };
            scanner.state = .dtd_int_subset;
            scanner.index = str_end;
            if (str_start == str_end) return null;
            return scanner.src[str_start..str_end];
        },
        .dtd_int_subset_pe_ref => {
            try scanner.checkForEof();
            const str_start = scanner.index;
            const str_end = std.mem.indexOfScalarPos(u8, scanner.src, scanner.index, ';') orelse {
                scanner.index = scanner.src.len;
                return scanner.src[str_start..];
            };
            scanner.state = .dtd_int_subset;
            scanner.index = str_end + 1;
            if (str_start == str_end) return null;
            return scanner.src[str_start..str_end];
        },
        .dtd_int_subset_invalid => {
            if (scanner.index == scanner.src.len) {
                if (!scanner.flags.eof_specified) return error.BufferUnderrun;
                scanner.state = .dtd_int_subset;
                return null;
            }
            const str_start = scanner.index;
            const str_end = std.mem.indexOfAnyPos(u8, scanner.src, scanner.index, whitespace_set ++ &[_]u8{ '<', '%', ']' }) orelse {
                scanner.index = scanner.src.len;
                return scanner.src[str_start..];
            };
            scanner.state = .dtd_int_subset;
            scanner.index = str_end;
            if (str_start == str_end) return null;
            return scanner.src[str_start..str_end];
        },
        .@"dtd_int_subset:<",
        .@"dtd_int_subset:<!",
        .@"dtd_int_subset:<!E",
        .@"dtd_int_subset:<!EL",
        .@"dtd_int_subset:<!ELE",
        .@"dtd_int_subset:<!ELEM",
        .@"dtd_int_subset:<!ELEME",
        .@"dtd_int_subset:<!ELEMEN",
        .@"dtd_int_subset:<!EN",
        .@"dtd_int_subset:<!ENT",
        .@"dtd_int_subset:<!ENTI",
        .@"dtd_int_subset:<!ENTIT",
        .@"dtd_int_subset:<!A",
        .@"dtd_int_subset:<!AT",
        .@"dtd_int_subset:<!ATT",
        .@"dtd_int_subset:<!ATTL",
        .@"dtd_int_subset:<!ATTLI",
        .@"dtd_int_subset:<!ATTLIS",
        => |tag| {
            scanner.state = .dtd_int_subset_invalid;
            return @tagName(tag)["dtd_int_subset:".len..];
        },

        .dtd_int_subset_element => return null,
        .dtd_int_subset_element_whitespace => {
            if (scanner.index == scanner.src.len) {
                if (!scanner.flags.eof_specified) return error.BufferUnderrun;
                scanner.state = .check_next_tok_type;
                return null;
            }
            const str_start = scanner.index;
            const str_end = std.mem.indexOfNonePos(u8, scanner.src, scanner.index, whitespace_set) orelse {
                scanner.index = scanner.src.len;
                return scanner.src[str_start..];
            };
            scanner.state = .dtd_int_subset_element;
            scanner.index = str_end;
            if (str_start == str_end) return null;
            return scanner.src[str_start..str_end];
        },
        .dtd_int_subset_element_token => {
            if (scanner.index == scanner.src.len) {
                if (!scanner.flags.eof_specified) return error.BufferUnderrun;
                scanner.state = .check_next_tok_type;
                return null;
            }
            const str_start = scanner.index;
            const str_end = std.mem.indexOfAnyPos(u8, scanner.src, scanner.index, whitespace_set ++ &[_]u8{ '\'', '>', '(' }) orelse {
                scanner.index = scanner.src.len;
                return scanner.src[str_start..];
            };
            scanner.state = .dtd_int_subset_element;
            scanner.index = str_end;
            if (str_start == str_end) return null;
            return scanner.src[str_start..str_end];
        },

        .dtd_int_subset_entity => unreachable,
        .dtd_int_subset_entity_whitespace => unreachable,
        .dtd_int_subset_entity_token => unreachable,

        .dtd_int_subset_attlist => unreachable,
        .dtd_int_subset_attlist_whitespace => unreachable,
        .dtd_int_subset_attlist_token => unreachable,

        .@"<![", .@"<![C", .@"<![CD", .@"<![CDA", .@"<![CDAT", .@"<![CDATA" => |tag| {
            scanner.state = .check_next_tok_type;
            return @tagName(tag);
        },
        .cdata => {
            if (scanner.index == scanner.src.len) {
                if (!scanner.flags.eof_specified) return error.BufferUnderrun;
                scanner.state = .check_next_tok_type;
                return null;
            }
            const str_start = scanner.index;
            while (std.mem.indexOfScalarPos(u8, scanner.src, scanner.index, ']')) |idx| {
                scanner.state = .@"cdata,]";
                scanner.index = idx + 1;
                if (str_start != idx) return scanner.src[str_start..idx];
                return scanner.nextString();
            }
            scanner.index = scanner.src.len;
            return scanner.src[str_start..];
        },
        .@"cdata,]" => {
            try scanner.checkForEof();
            if (scanner.src[scanner.index] != ']') {
                scanner.state = .cdata;
                return "]";
            }
            scanner.state = .@"cdata,]]";
            scanner.index += 1;
            // return @call(.always_tail, nextString, .{scanner});
            return scanner.nextString();
        },
        .@"cdata,]]" => {
            try scanner.checkForEof();
            switch (scanner.src[scanner.index]) {
                '>' => {
                    scanner.state = .check_next_tok_type;
                    scanner.index += 1;
                    return null;
                },
                ']' => {
                    scanner.index += 1;
                    return "]";
                },
                else => {
                    scanner.state = .cdata;
                    return "]]";
                },
            }
        },

        .element_open, .attr_name => {
            try scanner.checkForEof();
            const str_start = scanner.index;
            while (scanner.index != scanner.src.len) : (scanner.index += 1) {
                const idx = scanner.index;
                const char = scanner.src[idx];

                // terminate on '=', '\'', and '\"' as well and let caller
                // handle the fact that it doesn't make sense.
                const terminators = whitespace_set ++ &[_]u8{
                    '/',  '>',  '=',
                    '\'', '\"',
                };
                if (std.mem.indexOfScalar(u8, terminators, char) == null) continue;
                scanner.state = .element_open_name_end;
                break;
            }
            if (str_start != scanner.index) return scanner.src[str_start..scanner.index];
            return null;
        },
        .element_open_name_end => return null,
        .element_open_name_ws => {
            assert(std.mem.indexOfScalar(u8, whitespace_set, scanner.src[scanner.index]) != null);
            const str_start = scanner.index;
            scanner.state = .element_open_name_end;
            scanner.index = std.mem.indexOfNonePos(u8, scanner.src, scanner.index, whitespace_set) orelse scanner.src.len;
            if (str_start == scanner.index) return null;
            return scanner.src[str_start..scanner.index];
        },

        .@"element_open,/" => {
            scanner.state = .check_next_tok_type;
            return "/";
        },

        .attr_value_sq,
        .attr_value_dq,
        => |tag| {
            try scanner.checkForEof();
            const matching_quote: u8, const attr_val_ref: State = switch (tag) {
                .attr_value_sq => .{ '\'', .attr_value_sq_ref },
                .attr_value_dq => .{ '\"', .attr_value_dq_ref },
                else => unreachable,
            };
            const str_start = scanner.index;
            while (std.mem.indexOfAnyPos(u8, scanner.src, scanner.index, &.{ '&', matching_quote })) |idx| {
                scanner.index = idx;
                if (scanner.src[idx] != matching_quote) {
                    assert(scanner.src[idx] == '&');
                    scanner.state = attr_val_ref;
                    scanner.index += 1;
                    if (str_start != idx) return scanner.src[str_start..idx];
                    // return @call(.always_tail, nextString, .{scanner});
                    return scanner.nextString();
                }
                scanner.state = .attr_value_end;
                scanner.index += 1;
                if (str_start != idx) return scanner.src[str_start..idx];
                // return @call(.always_tail, nextString, .{scanner});
                return scanner.nextString();
            }
            scanner.index = scanner.src.len;
            return scanner.src[str_start..scanner.index];
        },
        .attr_value_sq_ref,
        .attr_value_dq_ref,
        => return null,
        .attr_value_end => return null,

        .@"</" => {
            try scanner.checkForEof();
            const str_start = scanner.index;
            const idx = std.mem.indexOfScalarPos(u8, scanner.src, scanner.index, '>') orelse {
                scanner.index = scanner.src.len;
                return scanner.src[str_start..];
            };
            scanner.state = .element_close_end;
            scanner.index = idx + 1;
            if (str_start != idx) return scanner.src[str_start..idx];
            // return @call(.always_tail, nextString, .{scanner});
            return scanner.nextString();
        },
        .element_close_end => {
            scanner.state = .check_next_tok_type;
            return null;
        },
    }
}

inline fn checkForEof(scanner: *Scanner) BufferOrEofError!void {
    if (scanner.index != scanner.src.len) return;
    if (!scanner.flags.eof_specified) return error.BufferUnderrun;
    scanner.state = .check_next_tok_type;
    return error.PrematureEof;
}

const Flags = packed struct {
    eof_specified: bool,
};

const State = enum {
    check_next_tok_type,

    text_data,
    @"text_data,]",
    @"text_data,]]",
    @"text_data,]]>",
    text_data_end,
    text_data_end_eof,

    markup_tag,

    pi_target,
    pi_target_qm,
    pi_target_end,
    pi_data,
    pi_data_qm,

    @"<!",

    @"<!-",
    comment,
    @"comment,-",
    @"comment,--",
    @"comment,-->",
    @"comment,---",
    @"comment,--->",

    @"<!D",
    @"<!DO",
    @"<!DOC",
    @"<!DOCT",
    @"<!DOCTY",
    @"<!DOCTYP",
    dtd_decl,
    dtd_whitespace,
    dtd_token,
    dtd_quote_single,
    dtd_quote_double,
    dtd_int_subset,
    dtd_int_subset_whitespace,
    dtd_int_subset_pe_ref,
    dtd_int_subset_invalid,
    @"dtd_int_subset:<",
    @"dtd_int_subset:<!",
    @"dtd_int_subset:<!E",
    @"dtd_int_subset:<!EL",
    @"dtd_int_subset:<!ELE",
    @"dtd_int_subset:<!ELEM",
    @"dtd_int_subset:<!ELEME",
    @"dtd_int_subset:<!ELEMEN",
    dtd_int_subset_element,
    dtd_int_subset_element_whitespace,
    dtd_int_subset_element_token,
    @"dtd_int_subset:<!EN",
    @"dtd_int_subset:<!ENT",
    @"dtd_int_subset:<!ENTI",
    @"dtd_int_subset:<!ENTIT",
    dtd_int_subset_entity,
    dtd_int_subset_entity_whitespace,
    dtd_int_subset_entity_token,
    @"dtd_int_subset:<!A",
    @"dtd_int_subset:<!AT",
    @"dtd_int_subset:<!ATT",
    @"dtd_int_subset:<!ATTL",
    @"dtd_int_subset:<!ATTLI",
    @"dtd_int_subset:<!ATTLIS",
    dtd_int_subset_attlist,
    dtd_int_subset_attlist_whitespace,
    dtd_int_subset_attlist_token,

    @"<![",
    @"<![C",
    @"<![CD",
    @"<![CDA",
    @"<![CDAT",
    @"<![CDATA",
    cdata,
    @"cdata,]",
    @"cdata,]]",

    element_open,
    element_open_name_end,
    element_open_name_ws,
    @"element_open,/",

    attr_val_sq_ref_segment,
    attr_val_dq_ref_segment,
    ref_segment,

    attr_val_sq_ref_semicolon,
    attr_val_dq_ref_semicolon,
    ref_semicolon,

    attr_name,
    attr_value_sq,
    attr_value_dq,
    attr_value_sq_ref,
    attr_value_dq_ref,
    attr_value_end,

    @"</",
    element_close_end,
};

fn testingPrint(comptime fmt_str: []const u8, args: anytype) void {
    if (@inComptime()) {
        @compileError(std.fmt.comptimePrint(fmt_str, args));
    } else if (std.testing.backend_can_print) {
        std.debug.print(fmt_str, args);
    }
}

fn expectNextTokenType(scanner: *Scanner, expected: NextTokenTypeError!TokenType) !void {
    switch (scanner.state) {
        .text_data,
        .text_data_end,
        .text_data_end_eof,
        .ref_segment,
        .ref_semicolon,
        .pi_target,
        .pi_target_qm,
        .pi_data_qm,
        .comment,
        .cdata,
        .element_open,
        .element_open_name_ws,
        .attr_name,
        .attr_val_sq_ref_segment,
        .attr_val_dq_ref_segment,
        .attr_val_sq_ref_semicolon,
        .attr_val_dq_ref_semicolon,
        .attr_value_sq,
        .attr_value_dq,
        .element_close_end,
        => |tag| {
            testingPrint("expected to be able to obtain next token type, but state is .{}\n", .{std.zig.fmtId(@tagName(tag))});
            return error.TestExpectedEqual;
        },
        else => {},
    }

    const actual = scanner.nextTokenType();
    try std.testing.expectEqual(expected, actual);
}
fn expectNextString(scanner: *Scanner, expected: NextStringError!?[]const u8) !void {
    switch (scanner.state) {
        .@"comment,-->" => |tag| {
            testingPrint("expected to be able to obtain next string, but state is .{}\n", .{std.zig.fmtId(@tagName(tag))});
            return error.TestExpectedEqual;
        },
        else => {},
    }

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
fn expectNextStringSeq(scanner: *Scanner, expected_list: []const NextStringError!?[]const u8) !void {
    for (expected_list) |expected| try scanner.expectNextString(expected);
}
fn expectNextTokenStringSeq(scanner: *Scanner, expected_tt: TokenType, expected_list: []const NextStringError!?[]const u8) !void {
    try scanner.expectNextTokenType(expected_tt);
    try std.testing.expect(expected_tt.hasString());
    try scanner.expectNextStringSeq(expected_list);
}

test "Scanner Invalid Markup ('<'/'<![CDATA['/'<!DOCTYPE')" {
    var scanner: Scanner = undefined;

    scanner = Scanner.initComplete("<");
    try scanner.expectNextTokenType(.invalid_token);
    try scanner.expectNextString("<");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

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
        scanner = Scanner.initComplete(incomplete_mk_str);
        try scanner.expectNextTokenType(.invalid_token);
        try scanner.expectNextString(incomplete_mk_str);
        try scanner.expectNextString(null);
        try scanner.expectNextTokenType(.eof);

        scanner = Scanner.initStreaming();
        for (0..incomplete_mk_str.len) |i| {
            scanner.feedInput(incomplete_mk_str[i..][0..1]);
            try scanner.expectNextTokenType(error.BufferUnderrun);
        }

        var eof_copy = scanner;
        eof_copy.feedEof();
        try eof_copy.expectNextTokenType(.invalid_token);
        try eof_copy.expectNextString(incomplete_mk_str);
        try eof_copy.expectNextString(null);
        try eof_copy.expectNextTokenType(.eof);

        scanner.feedInput("a");
        scanner.feedEof();
        try scanner.expectNextTokenType(.invalid_token);
        try scanner.expectNextString(incomplete_mk_str);
        try scanner.expectNextString(null);
        try scanner.expectNextTokenType(.text_data);
        try scanner.expectNextString("a");
        try scanner.expectNextString(null);
        try scanner.expectNextTokenType(.eof);
    }

    scanner = Scanner.initComplete("<![CDATAR a");
    try scanner.expectNextTokenType(.invalid_token);
    try scanner.expectNextString("<![CDATA");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.text_data);
    try scanner.expectNextString("R a");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<!DOCTYPR a");
    try scanner.expectNextTokenType(.invalid_token);
    try scanner.expectNextString("<!DOCTYP");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.text_data);
    try scanner.expectNextString("R a");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);
}

test "Scanner Processing Instructions" {
    var scanner: Scanner = undefined;

    scanner = Scanner.initComplete("<??>");
    try scanner.expectNextTokenType(.pi_target);
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<? ?>");
    try scanner.expectNextTokenType(.pi_target);
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.pi_data);
    try scanner.expectNextStringSeq(&.{ " ", null });
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<?foo?>");
    try scanner.expectNextTokenType(.pi_target);
    try scanner.expectNextStringSeq(&.{ "foo", null });
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<?foo ?>");
    try scanner.expectNextTokenType(.pi_target);
    try scanner.expectNextStringSeq(&.{ "foo", null });
    try scanner.expectNextTokenType(.pi_data);
    try scanner.expectNextStringSeq(&.{ " ", null });
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<?foo bar?>");
    try scanner.expectNextTokenType(.pi_target);
    try scanner.expectNextStringSeq(&.{ "foo", null });
    try scanner.expectNextTokenType(.pi_data);
    try scanner.expectNextStringSeq(&.{ " bar", null });
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<?" ++ "???" ++ "?>");
    try scanner.expectNextTokenType(.pi_target);
    try scanner.expectNextStringSeq(&.{ "?", "?", "?", null });
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming();
    scanner.feedInput("<?foo");
    try scanner.expectNextTokenType(.pi_target);
    try scanner.expectNextStringSeq(&.{ "foo", error.BufferUnderrun });
    scanner.feedInput("?>");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming();
    scanner.feedInput("<?foo");
    try scanner.expectNextTokenType(.pi_target);
    try scanner.expectNextStringSeq(&.{ "foo", error.BufferUnderrun });
    scanner.feedInput("?");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(">");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming();
    scanner.feedInput("<?");
    try scanner.expectNextTokenType(.pi_target);
    scanner.feedInput("fizz?>");
    try scanner.expectNextStringSeq(&.{ "fizz", null });
    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming();
    scanner.feedInput("<?bar");
    try scanner.expectNextTokenType(.pi_target);
    try scanner.expectNextStringSeq(&.{ "bar", error.BufferUnderrun });
    scanner.feedInput("?");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("?");
    try scanner.expectNextStringSeq(&.{ "?", error.BufferUnderrun });
    scanner.feedInput("baz");
    try scanner.expectNextStringSeq(&.{ "?", "baz", error.BufferUnderrun });
    scanner.feedInput("?>");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming();
    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedInput("<");
    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedInput("?");
    try scanner.expectNextTokenType(.pi_target);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("fo");
    try scanner.expectNextStringSeq(&.{ "fo", error.BufferUnderrun });
    scanner.feedInput("o?");
    try scanner.expectNextStringSeq(&.{ "o", error.BufferUnderrun });
    scanner.feedInput("bar");
    try scanner.expectNextStringSeq(&.{ "?", "bar", error.BufferUnderrun });
    scanner.feedInput(" ");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.pi_data);
    try scanner.expectNextStringSeq(&.{ " ", error.BufferUnderrun });
    scanner.feedInput("?");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(" ");
    try scanner.expectNextStringSeq(&.{ "?", " ", error.BufferUnderrun });
    scanner.feedInput("?");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(">");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextTokenType(.eof);
}

test "Scanner Comments" {
    var scanner: Scanner = undefined;

    scanner = Scanner.initComplete("<!--" ++ "-->");
    try scanner.expectNextTokenType(.comment);
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.comment_end);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<!--" ++ "-" ++ "-->");
    try scanner.expectNextTokenType(.comment);
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.invalid_comment_end_triple_dash);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<!--" ++ "--" ++ "-->");
    try scanner.expectNextTokenType(.comment);
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.invalid_comment_dash_dash);
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.comment_end);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<!--" ++ "--" ++ "-" ++ "-->");
    try scanner.expectNextTokenType(.comment);
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.invalid_comment_dash_dash);
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.invalid_comment_end_triple_dash);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<!-- <foo bar> -->");
    try scanner.expectNextTokenType(.comment);
    try scanner.expectNextStringSeq(&.{ " <foo bar> ", null });
    try scanner.expectNextTokenType(.comment_end);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming();
    scanner.feedInput("<!--");
    try scanner.expectNextTokenType(.comment);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("--");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(">");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.comment_end);
    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming();
    scanner.feedInput("<");
    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedInput("!");
    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedInput("-");
    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedInput("-");
    try scanner.expectNextTokenType(.comment);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("-");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("-");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(">");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.comment_end);
    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming();
    scanner.feedInput("<!--");
    try scanner.expectNextTokenType(.comment);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("--a");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.invalid_comment_dash_dash);
    try scanner.expectNextStringSeq(&.{ "a", error.BufferUnderrun });
    scanner.feedInput("-->");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.comment_end);
    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextTokenType(.eof);
}

test "Scanner Document Type Definition" {
    var scanner: Scanner = undefined;

    scanner = Scanner.initComplete("<!DOCTYPE");
    try scanner.expectNextTokenType(.dtd);
    try scanner.expectNextTokenType(.eof);
    scanner = Scanner.initComplete("<!DOCTYPE>");
    try scanner.expectNextTokenType(.dtd);
    try scanner.expectNextTokenType(.dtd_end);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<!DOCTYPEfoo");
    try scanner.expectNextTokenType(.dtd);
    try scanner.expectNextTokenType(.dtd_token);
    try scanner.expectNextStringSeq(&.{ "foo", null });
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming();
    scanner.feedInput("<!DOCTYPEfoo");
    try scanner.expectNextTokenType(.dtd);
    try scanner.expectNextTokenType(.dtd_token);
    try scanner.expectNextStringSeq(&.{ "foo", error.BufferUnderrun });
    scanner.feedEof();
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<!DOCTYPE foo SYSTEM 'bar");
    try scanner.expectNextTokenType(.dtd);
    try scanner.expectNextTokenStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTokenStringSeq(.dtd_token, &.{ "foo", null });
    try scanner.expectNextTokenStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTokenStringSeq(.dtd_token, &.{ "SYSTEM", null });
    try scanner.expectNextTokenStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTokenType(.dtd_literal_quote_single);
    try scanner.expectNextStringSeq(&.{ "bar", null });
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<!DOCTYPE foo SYSTEM 'bar' >");
    try scanner.expectNextTokenType(.dtd);

    try scanner.expectNextTokenStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTokenStringSeq(.dtd_token, &.{ "foo", null });
    try scanner.expectNextTokenStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTokenStringSeq(.dtd_token, &.{ "SYSTEM", null });
    try scanner.expectNextTokenStringSeq(.tag_whitespace, &.{ " ", null });

    try scanner.expectNextTokenType(.dtd_literal_quote_single);
    try scanner.expectNextStringSeq(&.{ "bar", null });
    try scanner.expectNextTokenType(.dtd_literal_end);

    try scanner.expectNextTokenStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTokenType(.dtd_end);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<!DOCTYPE []>");
    try scanner.expectNextTokenType(.dtd);
    try scanner.expectNextTokenStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTokenType(.dtd_int_subset);
    try scanner.expectNextTokenType(.dtd_int_subset_end);
    try scanner.expectNextTokenType(.dtd_end);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<!DOCTYPE [ asdf ]>");
    try scanner.expectNextTokenType(.dtd);
    try scanner.expectNextTokenStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTokenType(.dtd_int_subset);
    try scanner.expectNextTokenStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTokenStringSeq(.invalid_token, &.{ "asdf", null });
    try scanner.expectNextTokenStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTokenType(.dtd_int_subset_end);
    try scanner.expectNextTokenType(.dtd_end);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<!DOCTYPE [ asdf%foo; ]>");
    try scanner.expectNextTokenType(.dtd);
    try scanner.expectNextTokenStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTokenType(.dtd_int_subset);
    try scanner.expectNextTokenStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTokenStringSeq(.invalid_token, &.{ "asdf", null });
    try scanner.expectNextTokenStringSeq(.dtd_int_subset_pe_ref, &.{ "foo", null });
    try scanner.expectNextTokenStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTokenType(.dtd_int_subset_end);
    try scanner.expectNextTokenType(.dtd_end);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<!DOCTYPE foo PUBLIC 'bar' [\n\n] >");
    try scanner.expectNextTokenType(.dtd);

    try scanner.expectNextTokenStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTokenStringSeq(.dtd_token, &.{ "foo", null });
    try scanner.expectNextTokenStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTokenStringSeq(.dtd_token, &.{ "PUBLIC", null });
    try scanner.expectNextTokenStringSeq(.tag_whitespace, &.{ " ", null });

    try scanner.expectNextTokenType(.dtd_literal_quote_single);
    try scanner.expectNextStringSeq(&.{ "bar", null });
    try scanner.expectNextTokenType(.dtd_literal_end);

    try scanner.expectNextTokenStringSeq(.tag_whitespace, &.{ " ", null });

    try scanner.expectNextTokenType(.dtd_int_subset);
    try scanner.expectNextTokenStringSeq(.tag_whitespace, &.{ "\n\n", null });
    try scanner.expectNextTokenType(.dtd_int_subset_end);

    try scanner.expectNextTokenStringSeq(.tag_whitespace, &.{ " ", null });

    try scanner.expectNextTokenType(.dtd_end);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete(
        \\<!DOCTYPE [
        \\  <!ELEMENT foo EMPTY>
        \\]>
    );
    try scanner.expectNextTokenType(.dtd);
    try scanner.expectNextTokenStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTokenType(.dtd_int_subset);
    try scanner.expectNextTokenStringSeq(.tag_whitespace, &.{ "\n  ", null });
    try scanner.expectNextTokenType(.dtd_int_subset_element);
    try scanner.expectNextTokenStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTokenStringSeq(.dtd_token, &.{ "foo", null });
    try scanner.expectNextTokenStringSeq(.tag_whitespace, &.{ " ", null });
    try scanner.expectNextTokenStringSeq(.dtd_token, &.{ "EMPTY", null });
    try scanner.expectNextTokenType(.dtd_int_subset_element_end);
    try scanner.expectNextTokenStringSeq(.tag_whitespace, &.{ "\n", null });
    try scanner.expectNextTokenType(.dtd_int_subset_end);
    try scanner.expectNextTokenType(.dtd_end);
    try scanner.expectNextTokenType(.eof);
}

test "Scanner CDATA Sections" {
    var scanner: Scanner = undefined;

    scanner = Scanner.initComplete("<![CDATA[" ++ "]]>");
    try scanner.expectNextTokenType(.cdata);
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<![CDATA[" ++ " foo " ++ "]]>");
    try scanner.expectNextTokenType(.cdata);
    try scanner.expectNextStringSeq(&.{ " foo ", null });
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<![CDATA[" ++ "]]" ++ "]]>");
    try scanner.expectNextTokenType(.cdata);
    try scanner.expectNextStringSeq(&.{ "]", "]", null });
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<![CDATA[" ++ "]]]" ++ "]]>");
    try scanner.expectNextTokenType(.cdata);
    try scanner.expectNextStringSeq(&.{ "]", "]", "]", null });
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<![CDATA[" ++ "]>" ++ "]]>");
    try scanner.expectNextTokenType(.cdata);
    try scanner.expectNextStringSeq(&.{ "]", ">", null });
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming();
    for ("<![CDATA[") |c| {
        try scanner.expectNextTokenType(error.BufferUnderrun);
        scanner.feedInput(&.{c});
    }
    try scanner.expectNextTokenType(.cdata);
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
    try scanner.expectNextTokenType(.eof);
}

test "Scanner Character/Entity References" {
    var scanner: Scanner = undefined;

    scanner = Scanner.initComplete("&abc;");
    try scanner.expectNextTokenType(.char_ent_ref);
    try scanner.expectNextStringSeq(&.{ "abc", null });
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("&;");
    try scanner.expectNextTokenType(.char_ent_ref);
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("&");
    try scanner.expectNextTokenType(.char_ent_ref);
    try scanner.expectNextString(error.PrematureEof);

    scanner = Scanner.initComplete("&foo");
    try scanner.expectNextTokenType(.char_ent_ref);
    try scanner.expectNextString("foo");
    try scanner.expectNextString(error.PrematureEof);

    scanner = Scanner.initStreaming();
    scanner.feedInput("&");
    try scanner.expectNextTokenType(.char_ent_ref);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(";");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming();
    scanner.feedInput("&");
    try scanner.expectNextTokenType(.char_ent_ref);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("foo");
    try scanner.expectNextString("foo");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("bar");
    try scanner.expectNextString("bar");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(";");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextTokenType(.eof);
}

test "Scanner Text Data" {
    var scanner: Scanner = undefined;

    scanner = Scanner.initComplete("");
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming();
    scanner.feedEof();
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("foo bar");
    try scanner.expectNextTokenType(.text_data);
    try scanner.expectNextString("foo bar");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("foo bar");
    try scanner.expectNextTokenType(.text_data);
    try scanner.expectNextString("foo bar");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming();
    scanner.feedInput("foo");
    try scanner.expectNextTokenType(.text_data);
    try scanner.expectNextString("foo");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(" bar");
    try scanner.expectNextString(" bar");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("]]>");
    try scanner.expectNextTokenType(.text_data);
    try scanner.expectNextString("]]>");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming();
    scanner.feedInput("]]>");
    try scanner.expectNextTokenType(.text_data);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("foo");
    try scanner.expectNextString("]]>");
    try scanner.expectNextString("foo");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("foo]]> bar");
    try scanner.expectNextTokenType(.text_data);
    try scanner.expectNextString("foo");
    try scanner.expectNextString("]]>");
    try scanner.expectNextString(" bar");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming();
    scanner.feedInput("]");
    try scanner.expectNextTokenType(.text_data);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextString("]");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming();
    scanner.feedInput("]]");
    try scanner.expectNextTokenType(.text_data);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextString("]]");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming();
    scanner.feedInput("]]>");
    try scanner.expectNextTokenType(.text_data);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextString("]]>");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming();
    scanner.feedInput("]]");
    try scanner.expectNextTokenType(.text_data);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextString("]]");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);
}

test "Scanner Element Closing Tags" {
    var scanner: Scanner = undefined;

    scanner = Scanner.initComplete("</foo>");
    try scanner.expectNextTokenType(.element_close);
    try scanner.expectNextString("foo");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("</foo >");
    try scanner.expectNextTokenType(.element_close);
    try scanner.expectNextString("foo ");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("</ >");
    try scanner.expectNextTokenType(.element_close);
    try scanner.expectNextString(" ");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("</>");
    try scanner.expectNextTokenType(.element_close);
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);
}

test "Scanner Element Opening Tags" {
    var scanner: Scanner = undefined;

    scanner = Scanner.initComplete("<foo  />");
    try scanner.expectNextTokenType(.element_open);
    try scanner.expectNextString("foo");
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.tag_whitespace);
    try scanner.expectNextString("  ");
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.element_open_end_close_inline);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<foo  >");
    try scanner.expectNextTokenType(.element_open);
    try scanner.expectNextString("foo");
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.tag_whitespace);
    try scanner.expectNextString("  ");
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.element_open_end);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<foo bar='fizz'>");
    try scanner.expectNextTokenType(.element_open);
    try scanner.expectNextString("foo");
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.tag_whitespace);
    try scanner.expectNextString(" ");
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.attr_name);
    try scanner.expectNextString("bar");
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.attr_eql);

    try scanner.expectNextTokenType(.attr_value_quote_single);
    try scanner.expectNextString("fizz");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.attr_value_end);

    try scanner.expectNextTokenType(.element_open_end);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<foo bar = 'fizz' >");
    try scanner.expectNextTokenType(.element_open);
    try scanner.expectNextString("foo");
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.tag_whitespace);
    try scanner.expectNextString(" ");
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.attr_name);
    try scanner.expectNextString("bar");
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.tag_whitespace);
    try scanner.expectNextString(" ");
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.attr_eql);

    try scanner.expectNextTokenType(.tag_whitespace);
    try scanner.expectNextString(" ");
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.attr_value_quote_single);
    try scanner.expectNextString("fizz");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.attr_value_end);

    try scanner.expectNextTokenType(.tag_whitespace);
    try scanner.expectNextString(" ");
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.element_open_end);

    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<foo bar=\"fizz\">");
    try scanner.expectNextTokenType(.element_open);
    try scanner.expectNextString("foo");
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.tag_whitespace);
    try scanner.expectNextString(" ");
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.attr_name);
    try scanner.expectNextString("bar");
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.attr_eql);

    try scanner.expectNextTokenType(.attr_value_quote_double);
    try scanner.expectNextString("fizz");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.attr_value_end);

    try scanner.expectNextTokenType(.element_open_end);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<foo bar='&baz;'>");
    try scanner.expectNextTokenType(.element_open);
    try scanner.expectNextString("foo");
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.tag_whitespace);
    try scanner.expectNextString(" ");
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.attr_name);
    try scanner.expectNextString("bar");
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.attr_eql);

    try scanner.expectNextTokenType(.attr_value_quote_single);
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.char_ent_ref);
    try scanner.expectNextString("baz");
    try scanner.expectNextString(null);

    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.attr_value_end);

    try scanner.expectNextTokenType(.element_open_end);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<foo  bar='fizz&baz;buzz'>");
    try scanner.expectNextTokenType(.element_open);
    try scanner.expectNextString("foo");
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.tag_whitespace);
    try scanner.expectNextString("  ");
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.attr_name);
    try scanner.expectNextString("bar");
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.attr_eql);

    try scanner.expectNextTokenType(.attr_value_quote_single);
    try scanner.expectNextString("fizz");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.char_ent_ref);
    try scanner.expectNextString("baz");
    try scanner.expectNextString(null);
    try scanner.expectNextString("buzz");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.attr_value_end);

    try scanner.expectNextTokenType(.element_open_end);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming();
    scanner.feedInput("<fo");
    try scanner.expectNextTokenType(.element_open);
    try scanner.expectNextString("fo");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("o  ba");
    try scanner.expectNextString("o");
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.tag_whitespace);
    try scanner.expectNextString("  ");
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.attr_name);
    try scanner.expectNextString("ba");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("r='fi");
    try scanner.expectNextString("r");
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.attr_eql);

    try scanner.expectNextTokenType(.attr_value_quote_single);
    try scanner.expectNextString("fi");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("zz&ba");
    try scanner.expectNextString("zz");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.char_ent_ref);
    try scanner.expectNextString("ba");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("z;bu");
    try scanner.expectNextString("z");
    try scanner.expectNextString(null);
    try scanner.expectNextString("bu");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("zz'");
    try scanner.expectNextString("zz");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.attr_value_end);

    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedInput(">");
    try scanner.expectNextTokenType(.element_open_end);
    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextTokenType(.eof);
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

    var scanner = Scanner.initStreaming();
    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextTokenType(.pi_target);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextString("xm");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextString("l");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.pi_data);
    try scanner.expectNextString(" ");
    try scanner.expectNextString(error.BufferUnderrun);
    for (comptime &[_]*const [2:0]u8{
        "ve", "rs", "io", "n=", "\"1", ".0", "\" ", "en", "co", "di", "ng", "=\"", "UT", "F-", "8\"",
    }) |expected| {
        try scanner.expectNextString(error.BufferUnderrun);
        scanner.feedInput(feeder.next().?);
        try scanner.expectNextString(expected);
    }
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextTokenType(.text_data);
    try scanner.expectNextString("\n\n");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.element_open);
    try scanner.expectNextString("f");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextString("oo");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.element_open_end);

    try scanner.expectNextTokenType(.text_data);
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

    try scanner.expectNextTokenType(.element_open);
    try scanner.expectNextString("b");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextString("ar");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.tag_whitespace);
    try scanner.expectNextString(" ");
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.attr_name);
    try scanner.expectNextString("f");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextString("iz");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextString("z");
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.attr_eql);

    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextTokenType(.attr_value_quote_single);
    try scanner.expectNextString("b");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextString("uz");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextString("z");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.attr_value_end);
    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextTokenType(.element_open_end);

    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextTokenType(.element_open);
    try scanner.expectNextString("ba");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextString("z");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextTokenType(.element_open_end_close_inline);

    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextTokenType(.element_close);
    try scanner.expectNextString("b");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextString("ar");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.text_data);
    try scanner.expectNextString("\n");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextString(null);

    try scanner.expectNextTokenType(.element_close);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextString("fo");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextString("o");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextTokenType(.text_data);
    try scanner.expectNextString("\n");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);
}
