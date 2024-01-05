const std = @import("std");
const assert = std.debug.assert;

const Scanner = @This();
src: []const u8,
index: usize,
state: State,
eof_specified: bool,

/// Initializes the `Scanner` with the full input.
/// Calling `feedInput` or `feedEof` is illegal.
pub inline fn initComplete(src: []const u8) Scanner {
    var scanner = Scanner.initStreaming(src);
    scanner.feedEof();
    return scanner;
}

pub inline fn initStreaming(src: []const u8) Scanner {
    return .{
        .src = src,
        .index = 0,
        .state = .check_next_tok_type,
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
pub fn feedInput(scanner: *Scanner, src: []const u8) void {
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

pub const TokenType = enum {
    /// The end of the XML source.
    eof,

    /// The '<' character was followed by a sequence of characters
    /// which do not form a valid markup tag. This is an error,
    /// but tokenization may continue and this error can be deferred
    /// to later.
    invalid_markup,

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
    pi,

    /// The opening tag '<!--'.
    /// Call `nextString` to get segments of the comment text.
    ///
    /// After `nextString` returns null, `nextTokenType` should be called next,
    /// which will return either `.comment_end` or `.invalid_comment_end_triple_dash`.
    comment,
    /// The invalid token '--'.
    /// This can be ignored, and resume calling `nextString` as described for `.comment`,
    /// hence the error may be reported at a later time when it can be discerned
    /// into possibly more useful information, or grouped with other errors.
    invalid_comment_dash_dash,
    /// Indicates '-->' after a comment.
    comment_end,
    /// Indicates '--->' after a comment. This is an error, but can be deferred
    /// to later, and tokenization may proceed.
    invalid_comment_end_triple_dash,

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
    element_close,

    /// Whether or not this token should be followed up by a call to `nextString`.
    /// `.eof` returns false.
    pub inline fn hasString(token_type: TokenType) bool {
        return switch (token_type) {
            .eof => false,

            .char_ent_ref,
            .text_data,
            .pi,
            .comment,
            .invalid_comment_dash_dash,
            .cdata,
            .element_open,
            .attr_name,
            .attr_value_quote_single,
            .attr_value_quote_double,
            .element_close,
            => true,

            .invalid_markup,
            .comment_end,
            .invalid_comment_end_triple_dash,
            .invalid_cdata_stray_end,
            .element_open_end,
            .element_open_end_close_inline,
            .attr_eql,
            .attr_value_end,
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
                if (!scanner.eof_specified) return error.BufferUnderrun;
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
            try scanner.checkForEof();
            switch (scanner.src[scanner.index]) {
                '?' => {
                    scanner.state = .pi;
                    scanner.index += 1;
                    return .pi;
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

        .pi => unreachable,
        .@"pi,?" => unreachable,

        .@"<!" => {
            try scanner.checkForEof();
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
                else => {
                    scanner.state = .check_next_tok_type;
                    scanner.index += 1;
                    return .invalid_markup;
                },
            }
        },

        .@"<!-" => {
            try scanner.checkForEof();
            switch (scanner.src[scanner.index]) {
                '-' => {
                    scanner.state = .comment;
                    scanner.index += 1;
                    return .comment;
                },
                else => {
                    scanner.state = .check_next_tok_type;
                    scanner.index += 1;
                    return .invalid_markup;
                },
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

        .@"<![" => {
            try scanner.checkForEof();
            switch (scanner.src[scanner.index]) {
                'C' => {
                    scanner.state = .@"<![C";
                    scanner.index += 1;
                    // return @call(.always_tail, nextTokenType, .{scanner});
                    return scanner.nextTokenType();
                },
                else => {
                    scanner.state = .check_next_tok_type;
                    scanner.index += 1;
                    return .invalid_markup;
                },
            }
        },
        .@"<![C" => {
            try scanner.checkForEof();
            switch (scanner.src[scanner.index]) {
                'D' => {
                    scanner.state = .@"<![CD";
                    scanner.index += 1;
                    // return @call(.always_tail, nextTokenType, .{scanner});
                    return scanner.nextTokenType();
                },
                else => {
                    scanner.state = .check_next_tok_type;
                    scanner.index += 1;
                    return .invalid_markup;
                },
            }
        },
        .@"<![CD" => {
            try scanner.checkForEof();
            switch (scanner.src[scanner.index]) {
                'A' => {
                    scanner.state = .@"<![CDA";
                    scanner.index += 1;
                    // return @call(.always_tail, nextTokenType, .{scanner});
                    return scanner.nextTokenType();
                },
                else => {
                    scanner.state = .check_next_tok_type;
                    scanner.index += 1;
                    return .invalid_markup;
                },
            }
        },
        .@"<![CDA" => {
            try scanner.checkForEof();
            switch (scanner.src[scanner.index]) {
                'T' => {
                    scanner.state = .@"<![CDAT";
                    scanner.index += 1;
                    // return @call(.always_tail, nextTokenType, .{scanner});
                    return scanner.nextTokenType();
                },
                else => {
                    scanner.state = .check_next_tok_type;
                    scanner.index += 1;
                    return .invalid_markup;
                },
            }
        },
        .@"<![CDAT" => {
            try scanner.checkForEof();
            switch (scanner.src[scanner.index]) {
                'A' => {
                    scanner.state = .@"<![CDATA";
                    scanner.index += 1;
                    // return @call(.always_tail, nextTokenType, .{scanner});
                    return scanner.nextTokenType();
                },
                else => {
                    scanner.state = .check_next_tok_type;
                    scanner.index += 1;
                    return .invalid_markup;
                },
            }
        },
        .@"<![CDATA" => {
            try scanner.checkForEof();
            switch (scanner.src[scanner.index]) {
                '[' => {
                    scanner.state = .cdata;
                    scanner.index += 1;
                    return .cdata;
                },
                else => {
                    scanner.state = .check_next_tok_type;
                    scanner.index += 1;
                    return .invalid_markup;
                },
            }
        },
        .cdata => unreachable,
        .@"cdata,]" => unreachable,
        .@"cdata,]]" => unreachable,

        .element_open => unreachable,
        .element_open_name_end => {
            scanner.index = std.mem.indexOfNonePos(u8, scanner.src, scanner.index, whitespace_set) orelse scanner.src.len;
            try scanner.checkForEof();
            switch (scanner.src[scanner.index]) {
                else => {
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
                else => {
                    scanner.state = .check_next_tok_type;
                    return .invalid_markup;
                },
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
        .check_next_tok_type => unreachable,

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
                    if (scanner.eof_specified) {
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
                    if (scanner.eof_specified) {
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
                if (!scanner.eof_specified) return error.BufferUnderrun;
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
                if (!scanner.eof_specified) return error.BufferUnderrun;
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
            if (!scanner.eof_specified and scanner.index == scanner.src.len) return error.BufferUnderrun;
            scanner.state = if (scanner.eof_specified and scanner.index == scanner.src.len) .text_data_end else .text_data;
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
            if (scanner.eof_specified) {
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
            const segment: State, const semicolon: State = switch (itag) {
                .ref_segment => .{ .ref_segment, .ref_semicolon },
                .attr_val_sq_ref_segment => .{ .attr_val_sq_ref_segment, .attr_val_sq_ref_semicolon },
                .attr_val_dq_ref_segment => .{ .attr_val_dq_ref_segment, .attr_val_dq_ref_semicolon },
                else => unreachable,
            };

            try scanner.checkForEof();
            const str_start = scanner.index;
            const semicolon_idx = std.mem.indexOfScalarPos(u8, scanner.src, scanner.index, ';');

            scanner.state = if (semicolon_idx == null) segment else semicolon;
            scanner.index = if (semicolon_idx) |idx| idx else scanner.src.len;
            return scanner.src[str_start..scanner.index];
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

        .markup_tag => unreachable,

        .pi => {
            try scanner.checkForEof();
            const str_start = scanner.index;
            while (std.mem.indexOfScalarPos(u8, scanner.src, scanner.index, '?')) |idx| {
                scanner.state = .@"pi,?";
                scanner.index = idx + 1;
                if (str_start != idx) return scanner.src[str_start..idx];
                return scanner.nextString();
            }
            scanner.index = scanner.src.len;
            return scanner.src[str_start..scanner.index];
        },
        .@"pi,?" => {
            try scanner.checkForEof();
            if (scanner.src[scanner.index] != '>') {
                scanner.state = .pi;
                return "?";
            }
            scanner.state = .check_next_tok_type;
            scanner.index += 1;
            return null;
        },

        .@"<!" => unreachable,

        .@"<!-" => unreachable,
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

        .@"<![" => unreachable,
        .@"<![C" => unreachable,
        .@"<![CD" => unreachable,
        .@"<![CDA" => unreachable,
        .@"<![CDAT" => unreachable,
        .@"<![CDATA" => unreachable,
        .cdata => {
            try scanner.checkForEof();
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

        .@"element_open,/" => unreachable,

        inline //
        .attr_value_sq,
        .attr_value_dq,
        => |itag| {
            try scanner.checkForEof();
            const matching_quote: u8, const attr_val_ref: State = comptime switch (itag) {
                .attr_value_sq => .{ '\'', .attr_value_sq_ref },
                .attr_value_dq => .{ '\"', .attr_value_dq_ref },
                else => unreachable,
            };
            const str_start = scanner.index;
            while (std.mem.indexOfAnyPos(u8, scanner.src, scanner.index, &.{ '&', matching_quote })) |idx| {
                scanner.index = idx;
                switch (scanner.src[idx]) {
                    '&' => {
                        scanner.state = attr_val_ref;
                        scanner.index += 1;
                        if (str_start != idx) return scanner.src[str_start..idx];
                        // return @call(.always_tail, nextString, .{scanner});
                        return scanner.nextString();
                    },
                    matching_quote => {
                        scanner.state = .attr_value_end;
                        scanner.index += 1;
                        if (str_start != idx) return scanner.src[str_start..idx];
                        // return @call(.always_tail, nextString, .{scanner});
                        return scanner.nextString();
                    },
                    else => unreachable,
                }
            } else {
                scanner.index = scanner.src.len;
            }
            return scanner.src[str_start..scanner.index];
        },
        inline //
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
    if (!scanner.eof_specified) return error.BufferUnderrun;
    return error.PrematureEof;
}

const State = enum {
    check_next_tok_type,

    text_data,
    @"text_data,]",
    @"text_data,]]",
    @"text_data,]]>",
    text_data_end,
    text_data_end_eof,

    markup_tag,

    pi,
    @"pi,?",

    @"<!",

    @"<!-",
    comment,
    @"comment,-",
    @"comment,--",
    @"comment,-->",
    @"comment,---",
    @"comment,--->",

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

const whitespace_set: []const u8 = &[_]u8{
    '\u{20}',
    '\u{09}',
    '\u{0D}',
    '\u{0A}',
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
        else => |tag| {
            testingPrint("expected to be able to obtain next token type, but state is .{}\n", .{std.zig.fmtId(@tagName(tag))});
            return error.TestExpectedEqual;
        },
        .check_next_tok_type,
        .markup_tag,

        .element_open_name_end,
        .@"element_open,/",
        .attr_value_sq,
        .attr_value_dq,
        .attr_value_sq_ref,
        .attr_value_dq_ref,
        .attr_value_end,

        .@"<!",
        .@"<!-",

        .@"<![",
        .@"<![C",
        .@"<![CD",
        .@"<![CDA",
        .@"<![CDAT",
        .@"<![CDATA",

        .@"comment,--",
        .@"comment,-->",
        .@"comment,---",
        .@"comment,--->",
        => {},
    }

    const actual = scanner.nextTokenType();
    try std.testing.expectEqual(expected, actual);
}

fn expectNextString(scanner: *Scanner, expected: NextStringError!?[]const u8) !void {
    switch (scanner.state) {
        .check_next_tok_type,
        .markup_tag,
        => |tag| {
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

test "Scanner Processing Instructions" {
    var scanner: Scanner = undefined;

    scanner = Scanner.initComplete("<??>");
    try scanner.expectNextTokenType(.pi);
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<? ?>");
    try scanner.expectNextTokenType(.pi);
    try scanner.expectNextString(" ");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<?foo?>");
    try scanner.expectNextTokenType(.pi);
    try scanner.expectNextString("foo");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<?foo ?>");
    try scanner.expectNextTokenType(.pi);
    try scanner.expectNextString("foo ");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<?foo bar?>");
    try scanner.expectNextTokenType(.pi);
    try scanner.expectNextString("foo bar");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<?" ++ "???" ++ "?>");
    try scanner.expectNextTokenType(.pi);
    try scanner.expectNextString("?");
    try scanner.expectNextString("?");
    try scanner.expectNextString("?");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming("<?foo");
    try scanner.expectNextTokenType(.pi);
    try scanner.expectNextString("foo");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("?>");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming("<?foo");
    try scanner.expectNextTokenType(.pi);
    try scanner.expectNextString("foo");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("?");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(">");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming("<?");
    try scanner.expectNextTokenType(.pi);
    scanner.feedInput("fizz?>");
    try scanner.expectNextString("fizz");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming("<?bar");
    try scanner.expectNextTokenType(.pi);
    try scanner.expectNextString("bar");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("?");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("?");
    try scanner.expectNextString("?");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("baz");
    try scanner.expectNextString("?");
    try scanner.expectNextString("baz");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("?>");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming("");
    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedInput("<");
    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedInput("?");
    try scanner.expectNextTokenType(.pi);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("fo");
    try scanner.expectNextString("fo");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("o?");
    try scanner.expectNextString("o");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("bar");
    try scanner.expectNextString("?");
    try scanner.expectNextString("bar");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(" ");
    try scanner.expectNextString(" ");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("?");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(" ");
    try scanner.expectNextString("?");
    try scanner.expectNextString(" ");
    try scanner.expectNextString(error.BufferUnderrun);
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
    try scanner.expectNextString(" <foo bar> ");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.comment_end);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming("<!--");
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

    scanner = Scanner.initStreaming("");
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

    scanner = Scanner.initStreaming("<!--");
    try scanner.expectNextTokenType(.comment);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("--a");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.invalid_comment_dash_dash);
    try scanner.expectNextString("a");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("-->");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.comment_end);
    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedEof();
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
    try scanner.expectNextString(" foo ");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<![CDATA[" ++ "]]" ++ "]]>");
    try scanner.expectNextTokenType(.cdata);
    try scanner.expectNextString("]");
    try scanner.expectNextString("]");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<![CDATA[" ++ "]]]" ++ "]]>");
    try scanner.expectNextTokenType(.cdata);
    try scanner.expectNextString("]");
    try scanner.expectNextString("]");
    try scanner.expectNextString("]");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<![CDATA[" ++ "]>" ++ "]]>");
    try scanner.expectNextTokenType(.cdata);
    try scanner.expectNextString("]");
    try scanner.expectNextString(">");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming("");
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
    try scanner.expectNextString("]");
    try scanner.expectNextString("]");
    try scanner.expectNextString("]");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(">");
    try scanner.expectNextString(null);
    scanner.feedEof();
    try scanner.expectNextTokenType(.eof);
}

test "Scanner Character/Entity References" {
    var scanner: Scanner = undefined;

    scanner = Scanner.initComplete("&abc;");
    try scanner.expectNextTokenType(.char_ent_ref);
    try scanner.expectNextString("abc");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("&;");
    try scanner.expectNextTokenType(.char_ent_ref);
    try scanner.expectNextString("");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming("&");
    try scanner.expectNextTokenType(.char_ent_ref);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(";");
    try scanner.expectNextString("");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming("&");
    try scanner.expectNextTokenType(.char_ent_ref);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("foo");
    try scanner.expectNextString("foo");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("bar");
    try scanner.expectNextString("bar");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(";");
    try scanner.expectNextString("");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextTokenType(.eof);
}

test "Scanner Text Data" {
    var scanner: Scanner = undefined;

    scanner = Scanner.initComplete("");
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming("");
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

    scanner = Scanner.initStreaming("foo");
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

    scanner = Scanner.initStreaming("]]>");
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

    scanner = Scanner.initStreaming("]");
    try scanner.expectNextTokenType(.text_data);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextString("]");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming("]]");
    try scanner.expectNextTokenType(.text_data);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextString("]]");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming("]]>");
    try scanner.expectNextTokenType(.text_data);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedEof();
    try scanner.expectNextString("]]>");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initStreaming("]]");
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
    try scanner.expectNextTokenType(.element_open_end_close_inline);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<foo  >");
    try scanner.expectNextTokenType(.element_open);
    try scanner.expectNextString("foo");
    try scanner.expectNextString(null);
    try scanner.expectNextTokenType(.element_open_end);
    try scanner.expectNextTokenType(.eof);

    scanner = Scanner.initComplete("<foo bar='fizz'>");
    try scanner.expectNextTokenType(.element_open);
    try scanner.expectNextString("foo");
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

    scanner = Scanner.initComplete("<foo bar=\"fizz\">");
    try scanner.expectNextTokenType(.element_open);
    try scanner.expectNextString("foo");
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

    scanner = Scanner.initStreaming("<fo");
    try scanner.expectNextTokenType(.element_open);
    try scanner.expectNextString("fo");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput("o  ba");
    try scanner.expectNextString("o");
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

    var scanner = Scanner.initStreaming("");
    try scanner.expectNextTokenType(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    try scanner.expectNextTokenType(.pi);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feedInput(feeder.next().?);
    for (comptime &[_]*const [2:0]u8{
        "xm", "l ", "ve", "rs", "io",  "n=", "\"1", ".0",  "\" ", //
        "en", "co", "di", "ng", "=\"", "UT", "F-",  "8\"",
    }) |expected| {
        try scanner.expectNextString(expected);
        try scanner.expectNextString(error.BufferUnderrun);
        const fed = feeder.next().?;
        scanner.feedInput(fed);
    }
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
