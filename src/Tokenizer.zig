const std = @import("std");
const assert = std.debug.assert;

const Scanner = @This();
src: []const u8,
index: usize,
state: State,

pub inline fn init(src: []const u8) Scanner {
    return .{
        .src = src,
        .index = 0,
        .state = .text_or_close_or_start,
    };
}

/// Should be called to feed more input after encountering `error.BufferUnderrun`.
pub fn feed(scanner: *Scanner, src: []const u8) void {
    scanner.src = src;
    scanner.index = 0;
}

pub const DataType = enum {
    /// The end of the XML source - if more input is feed into the Scanner,
    /// this is overriden.
    eof,
    /// The opening of a markup tag '<'.
    /// Call `nextMarkupTag` to find out the type of the markup tag.
    markup_tag,
};

pub const MarkupTag = enum {
    /// The '<' character was followed by a sequence of characters
    /// which do not form a valid markup tag. This is an error,
    /// but tokenization may continue and this error can be deferred
    /// to later.
    invalid,

    /// The markup tag is a PI (Processing Instructions) tag ('<?').
    /// Call `nextString` to get segments of the PI target name until
    /// it returns null.
    ///
    /// After it returns null, it should once again be called repeatedly
    /// to obtain segments of the PI instructions string. If it returns
    /// null immediately, there are no PI instructions after the target.
    ///
    /// After it returns null again, `nextDataType` should be called next.
    pi,

    /// The markup tag is a comment tag ('<!--').
    /// Call `nextString` to get segments of the comment text.
    ///
    /// The invalid token '--' will be tokenized as normal, and in addition
    /// is guaranteed to be returned as a singular string, such that the error
    /// can be easily flagged and deferred to later code.
    ///
    /// After `nextString` returns null, `nextMarkupTag` should be called next,
    /// which will return either `.comment_end` or `.comment_end_triple_dash`.
    comment,

    /// Indicates '-->' after a comment.
    comment_end,
    /// Indicates '--->' after a comment. This is an error, but can be deferred
    /// to later, and tokenization may proceed.
    comment_end_triple_dash,

    /// The markup tag is a CDATA Section ('<![CDATA[').
    /// Call `nextString` to get the CDATA text segments until it returns `null`.
    cdata,
};

pub const BufferError = error{
    /// Must feed more input.
    BufferUnderrun,
};

pub const NextDataTypeError = BufferError;
/// See the comments on the data type tag for more information.
pub fn nextDataType(scanner: *Scanner) NextDataTypeError!DataType {
    switch (scanner.state) {
        .text_or_close_or_start => {
            if (scanner.index == scanner.src.len) return .eof;
            switch (scanner.src[scanner.index]) {
                '<' => {
                    scanner.state = .markup_tag;
                    scanner.index += 1;
                    return .markup_tag;
                },
                '&' => @panic("TODO"),
                else => @panic("TODO"),
            }
        },
        .markup_tag => unreachable,

        .pi_target_name => unreachable,
        .pi_target_name_qm => unreachable,
        .pi_target_name_qm_rab => unreachable,
        .pi_target_name_end => unreachable,

        .pi_data => unreachable,
        .pi_data_qm => unreachable,
        .pi_data_qm_rab => unreachable,

        .@"<!" => unreachable,

        .@"<!-" => unreachable,
        .@"<!--" => unreachable,
        .comment_dash => unreachable,
        .comment_dash_dash => unreachable,
        .comment_dash_dash_dash => unreachable,
        .@"--->" => unreachable,
        .@"-->" => unreachable,

        .@"<![" => unreachable,
        .@"<![C" => unreachable,
        .@"<![CD" => unreachable,
        .@"<![CDA" => unreachable,
        .@"<![CDAT" => unreachable,
        .@"<![CDATA" => unreachable,
        .@"<![CDATA[" => unreachable,
        .@"]" => unreachable,
        .@"]]" => unreachable,
    }
}

pub const NextMarkupTagError = BufferError;
/// See the comments on the markup tag for more information.
pub fn nextMarkupTag(scanner: *Scanner) NextMarkupTagError!MarkupTag {
    switch (scanner.state) {
        .text_or_close_or_start => unreachable,
        .markup_tag => {
            if (scanner.index == scanner.src.len) return error.BufferUnderrun;
            switch (scanner.src[scanner.index]) {
                '?' => {
                    scanner.state = .pi_target_name;
                    scanner.index += 1;
                    return .pi;
                },
                '!' => {
                    scanner.state = .@"<!";
                    scanner.index += 1;
                    return @call(.always_tail, nextMarkupTag, .{scanner});
                },
                else => @panic("TODO"),
            }
        },

        .pi_target_name => unreachable,
        .pi_target_name_qm => unreachable,
        .pi_target_name_qm_rab => unreachable,
        .pi_target_name_end => unreachable,

        .pi_data => unreachable,
        .pi_data_qm => unreachable,
        .pi_data_qm_rab => unreachable,

        .@"<!" => {
            if (scanner.index == scanner.src.len) return error.BufferUnderrun;
            switch (scanner.src[scanner.index]) {
                '-' => {
                    scanner.state = .@"<!-";
                    scanner.index += 1;
                    return @call(.always_tail, nextMarkupTag, .{scanner});
                },
                '[' => {
                    scanner.state = .@"<![";
                    scanner.index += 1;
                    return @call(.always_tail, nextMarkupTag, .{scanner});
                },
                else => {
                    scanner.state = .text_or_close_or_start;
                    scanner.index += 1;
                    return .invalid;
                },
            }
        },

        .@"<!-" => {
            if (scanner.index == scanner.src.len) return error.BufferUnderrun;
            switch (scanner.src[scanner.index]) {
                '-' => {
                    scanner.state = .@"<!--";
                    scanner.index += 1;
                    return .comment;
                },
                else => {
                    scanner.state = .text_or_close_or_start;
                    scanner.index += 1;
                    return .invalid;
                },
            }
        },
        .@"<!--" => unreachable,
        .comment_dash => unreachable,
        .comment_dash_dash => unreachable,
        .comment_dash_dash_dash => unreachable,
        .@"--->" => {
            scanner.state = .text_or_close_or_start;
            return .comment_end_triple_dash;
        },
        .@"-->" => {
            scanner.state = .text_or_close_or_start;
            return .comment_end;
        },

        .@"<![" => {
            if (scanner.index == scanner.src.len) return error.BufferUnderrun;
            switch (scanner.src[scanner.index]) {
                'C' => {
                    scanner.state = .@"<![C";
                    scanner.index += 1;
                    return @call(.always_tail, nextMarkupTag, .{scanner});
                },
                else => {
                    scanner.state = .text_or_close_or_start;
                    scanner.index += 1;
                    return .invalid;
                },
            }
        },
        .@"<![C" => {
            if (scanner.index == scanner.src.len) return error.BufferUnderrun;
            switch (scanner.src[scanner.index]) {
                'D' => {
                    scanner.state = .@"<![CD";
                    scanner.index += 1;
                    return @call(.always_tail, nextMarkupTag, .{scanner});
                },
                else => {
                    scanner.state = .text_or_close_or_start;
                    scanner.index += 1;
                    return .invalid;
                },
            }
        },
        .@"<![CD" => {
            if (scanner.index == scanner.src.len) return error.BufferUnderrun;
            switch (scanner.src[scanner.index]) {
                'A' => {
                    scanner.state = .@"<![CDA";
                    scanner.index += 1;
                    return @call(.always_tail, nextMarkupTag, .{scanner});
                },
                else => {
                    scanner.state = .text_or_close_or_start;
                    scanner.index += 1;
                    return .invalid;
                },
            }
        },
        .@"<![CDA" => {
            if (scanner.index == scanner.src.len) return error.BufferUnderrun;
            switch (scanner.src[scanner.index]) {
                'T' => {
                    scanner.state = .@"<![CDAT";
                    scanner.index += 1;
                    return @call(.always_tail, nextMarkupTag, .{scanner});
                },
                else => {
                    scanner.state = .text_or_close_or_start;
                    scanner.index += 1;
                    return .invalid;
                },
            }
        },
        .@"<![CDAT" => {
            if (scanner.index == scanner.src.len) return error.BufferUnderrun;
            switch (scanner.src[scanner.index]) {
                'A' => {
                    scanner.state = .@"<![CDATA";
                    scanner.index += 1;
                    return @call(.always_tail, nextMarkupTag, .{scanner});
                },
                else => {
                    scanner.state = .text_or_close_or_start;
                    scanner.index += 1;
                    return .invalid;
                },
            }
        },
        .@"<![CDATA" => {
            if (scanner.index == scanner.src.len) return error.BufferUnderrun;
            switch (scanner.src[scanner.index]) {
                '[' => {
                    scanner.state = .@"<![CDATA[";
                    scanner.index += 1;
                    return .cdata;
                },
                else => {
                    scanner.state = .text_or_close_or_start;
                    scanner.index += 1;
                    return .invalid;
                },
            }
        },
        .@"<![CDATA[" => unreachable,
        .@"]" => unreachable,
        .@"]]" => unreachable,
    }
}

pub const NextStringError = BufferError;
pub fn nextString(scanner: *Scanner) NextStringError!?[]const u8 {
    switch (scanner.state) {
        .text_or_close_or_start => unreachable,
        .markup_tag => unreachable,
        .pi_target_name => {
            if (scanner.index == scanner.src.len) return error.BufferUnderrun;
            const str_start = scanner.index;
            for (scanner.src[scanner.index..], scanner.index..) |char, idx| {
                if (isWhitespace(char)) {
                    scanner.state = .pi_target_name_end;
                    scanner.index = idx;
                    return scanner.src[str_start..scanner.index];
                }
                if (char != '?') continue;
                scanner.index = idx + 1;
                if (scanner.index == scanner.src.len) {
                    scanner.state = .pi_target_name_qm;
                    return scanner.src[str_start..idx];
                }
                if (scanner.src[scanner.index] == '>') {
                    scanner.state = .pi_target_name_qm_rab;
                    return scanner.src[str_start..idx];
                }
            }
            scanner.index = scanner.src.len;
            return scanner.src[str_start..];
        },
        .pi_target_name_qm => {
            if (scanner.index == scanner.src.len) return error.BufferUnderrun;
            assert(scanner.index == 0);
            if (scanner.src[scanner.index] == '>') {
                scanner.state = .pi_data_qm_rab;
                return null;
            }
            scanner.state = .pi_target_name;
            return "?";
        },
        .pi_target_name_qm_rab => {
            assert(scanner.src[scanner.index] == '>');
            scanner.state = .pi_data_qm_rab;
            return null;
        },
        .pi_target_name_end => {
            assert(isWhitespace(scanner.src[scanner.index]));
            scanner.state = .pi_data;
            return null;
        },

        .pi_data => {
            if (scanner.index == scanner.src.len) return error.BufferUnderrun;
            const str_start = scanner.index;
            while (std.mem.indexOfScalarPos(u8, scanner.src, scanner.index, '?')) |idx| {
                scanner.index = idx + 1;
                if (scanner.index == scanner.src.len) {
                    scanner.state = .pi_data_qm;
                    return scanner.src[str_start..idx];
                }
                if (scanner.src[scanner.index] == '>') {
                    scanner.state = .pi_data_qm_rab;
                    return scanner.src[str_start..idx];
                }
            }
            scanner.index = scanner.src.len;
            return scanner.src[str_start..];
        },
        .pi_data_qm => {
            if (scanner.index == scanner.src.len) return error.BufferUnderrun;
            assert(scanner.index == 0);
            if (scanner.src[scanner.index] == '>') {
                scanner.state = .text_or_close_or_start;
                scanner.index += 1;
                return null;
            }
            scanner.state = .pi_data;
            return "?";
        },
        .pi_data_qm_rab => {
            assert(scanner.src[scanner.index] == '>');
            scanner.state = .text_or_close_or_start;
            scanner.index += 1;
            return null;
        },

        .@"<!" => unreachable,

        .@"<!-" => unreachable,
        .@"<!--" => {
            if (scanner.index == scanner.src.len) return error.BufferUnderrun;
            const str_start = scanner.index;
            while (std.mem.indexOfScalarPos(u8, scanner.src, scanner.index, '-')) |idx| {
                scanner.index = idx + 1;
                if (scanner.index == scanner.src.len) {
                    scanner.state = .comment_dash;
                    return scanner.src[str_start..idx];
                }
                if (scanner.src[scanner.index] == '-') {
                    scanner.state = .comment_dash_dash;
                    scanner.index += 1;
                    return scanner.src[str_start..idx];
                }
            }
            scanner.index = scanner.src.len;
            return scanner.src[str_start..];
        },
        .comment_dash => {
            if (scanner.index == scanner.src.len) return error.BufferUnderrun;
            if (scanner.src[scanner.index] != '-') {
                scanner.state = .@"<!--";
                return "-";
            }
            scanner.state = .comment_dash_dash;
            scanner.index += 1;
            return @call(.always_tail, nextString, .{scanner});
        },
        .comment_dash_dash => {
            if (scanner.index == scanner.src.len) return error.BufferUnderrun;
            switch (scanner.src[scanner.index]) {
                '>' => {
                    scanner.state = .@"-->";
                    scanner.index += 1;
                    return null;
                },
                '-' => {
                    scanner.state = .comment_dash_dash_dash;
                    scanner.index += 1;
                    return @call(.always_tail, nextString, .{scanner});
                },
                else => {
                    scanner.state = .@"<!--";
                    return "--";
                },
            }
        },
        .comment_dash_dash_dash => {
            if (scanner.index == scanner.src.len) return error.BufferUnderrun;
            if (scanner.src[scanner.index] != '>') {
                scanner.state = .comment_dash;
                return "--";
            }
            scanner.state = .@"--->";
            scanner.index += 1;
            return null;
        },
        .@"-->" => unreachable,
        .@"--->" => unreachable,

        .@"<![" => unreachable,
        .@"<![C" => unreachable,
        .@"<![CD" => unreachable,
        .@"<![CDA" => unreachable,
        .@"<![CDAT" => unreachable,
        .@"<![CDATA" => unreachable,
        .@"<![CDATA[" => {
            const str_start = scanner.index;
            while (std.mem.indexOfScalarPos(u8, scanner.src, scanner.index, ']')) |idx| {
                scanner.index = idx + 1;
                if (scanner.index == scanner.src.len) {
                    scanner.state = .@"]";
                    return scanner.src[str_start..idx];
                }
                if (scanner.src[scanner.index] == ']') {
                    scanner.state = .@"]]";
                    scanner.index += 1;
                    return scanner.src[str_start..idx];
                }
            }
            scanner.index = scanner.src.len;
            return scanner.src[str_start..];
        },
        .@"]" => {
            if (scanner.index == scanner.src.len) return error.BufferUnderrun;
            if (scanner.src[scanner.index] != ']') {
                scanner.state = .@"<![CDATA[";
                return "]";
            }
            scanner.state = .@"]]";
            scanner.index += 1;
            return @call(.always_tail, nextString, .{scanner});
        },
        .@"]]" => {
            if (scanner.index == scanner.src.len) return error.BufferUnderrun;
            if (scanner.src[scanner.index] != '>') {
                scanner.state = .comment_dash;
                return "]]";
            }
            scanner.state = .text_or_close_or_start;
            scanner.index += 1;
            return null;
        },
    }
}

const State = enum {
    text_or_close_or_start,
    markup_tag,

    pi_target_name,
    pi_target_name_qm,
    pi_target_name_qm_rab,
    pi_target_name_end,

    pi_data,
    pi_data_qm,
    pi_data_qm_rab,

    @"<!",

    @"<!-",
    @"<!--",
    comment_dash,
    comment_dash_dash,
    comment_dash_dash_dash,
    @"--->",
    @"-->",

    @"<![",
    @"<![C",
    @"<![CD",
    @"<![CDA",
    @"<![CDAT",
    @"<![CDATA",
    @"<![CDATA[",
    @"]",
    @"]]",
};

const whitespace_set = &[_]u8{
    '\u{20}',
    '\u{09}',
    '\u{0D}',
    '\u{0A}',
};
inline fn isWhitespace(cp_first_byte: u8) bool {
    return switch (cp_first_byte) {
        '\u{20}',
        '\u{09}',
        '\u{0D}',
        '\u{0A}',
        => true,
        else => false,
    };
}

inline fn isNameStartChar(codepoint: u21) bool {
    return switch (codepoint) {
        ':',
        'A'...'Z',
        '_',
        'a'...'z',
        '\u{C0}'...'\u{D6}',
        '\u{D8}'...'\u{F6}',
        '\u{F8}'...'\u{2FF}',
        '\u{370}'...'\u{37D}',
        '\u{37F}'...'\u{1FFF}',
        '\u{200C}'...'\u{200D}',
        '\u{2070}'...'\u{218F}',
        '\u{2C00}'...'\u{2FEF}',
        '\u{3001}'...'\u{D7FF}',
        '\u{F900}'...'\u{FDCF}',
        '\u{FDF0}'...'\u{FFFD}',
        '\u{10000}'...'\u{EFFFF}',
        => true,
        else => false,
    };
}

inline fn isNameChar(codepoint: u21) bool {
    return isNameStartChar(codepoint) or switch (codepoint) {
        '-',
        '.',
        '0'...'9',
        '\u{B7}',
        '\u{0300}'...'\u{036F}',
        '\u{203F}'...'\u{2040}',
        => true,
        else => false,
    };
}

pub usingnamespace if (!@import("builtin").is_test) struct {} else struct {
    fn expectNextDataType(scanner: *Scanner, expected: NextDataTypeError!DataType) !void {
        const actual = scanner.nextDataType();
        try std.testing.expectEqual(expected, actual);
    }
    fn expectNextMarkupTag(scanner: *Scanner, expected: NextMarkupTagError!MarkupTag) !void {
        const actual = scanner.nextMarkupTag();
        try std.testing.expectEqual(expected, actual);
    }
    fn expectNextString(scanner: *Scanner, expected: NextStringError!?[]const u8) !void {
        const actual = scanner.nextString();
        try std.testing.expectEqualDeep(expected, actual);
    }
};

test "Scanner Processing Instructions" {
    var scanner: Scanner = undefined;

    scanner = Scanner.init("<??>");
    try scanner.expectNextDataType(.markup_tag);
    try scanner.expectNextMarkupTag(.pi);

    try scanner.expectNextString("");
    try scanner.expectNextString(null);

    try scanner.expectNextString(null);

    try scanner.expectNextDataType(.eof);

    scanner = Scanner.init("<? ?>");
    try scanner.expectNextDataType(.markup_tag);
    try scanner.expectNextMarkupTag(.pi);

    try scanner.expectNextString("");
    try scanner.expectNextString(null);

    try scanner.expectNextString(" ");
    try scanner.expectNextString(null);

    try scanner.expectNextDataType(.eof);

    scanner = Scanner.init("<?foo?>");
    try scanner.expectNextDataType(.markup_tag);
    try scanner.expectNextMarkupTag(.pi);

    try scanner.expectNextString("foo");
    try scanner.expectNextString(null);

    try scanner.expectNextString(null);

    try scanner.expectNextDataType(.eof);

    scanner = Scanner.init("<?foo ?>");
    try scanner.expectNextDataType(.markup_tag);
    try scanner.expectNextMarkupTag(.pi);

    try scanner.expectNextString("foo");
    try scanner.expectNextString(null);

    try scanner.expectNextString(" ");
    try scanner.expectNextString(null);

    try scanner.expectNextDataType(.eof);

    scanner = Scanner.init("<?foo bar?>");
    try scanner.expectNextDataType(.markup_tag);
    try scanner.expectNextMarkupTag(.pi);

    try scanner.expectNextString("foo");
    try scanner.expectNextString(null);

    try scanner.expectNextString(" bar");
    try scanner.expectNextString(null);

    try scanner.expectNextDataType(.eof);

    scanner = Scanner.init("<?foo");
    try scanner.expectNextDataType(.markup_tag);
    try scanner.expectNextMarkupTag(.pi);
    try scanner.expectNextString("foo");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feed("?>");
    try scanner.expectNextString("");
    try scanner.expectNextString(null);
    try scanner.expectNextString(null);
    try scanner.expectNextDataType(.eof);

    scanner = Scanner.init("<?foo");
    try scanner.expectNextDataType(.markup_tag);
    try scanner.expectNextMarkupTag(.pi);
    try scanner.expectNextString("foo");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feed("?");
    try scanner.expectNextString("");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feed(">");
    try scanner.expectNextString(null);
    try scanner.expectNextString(null);
    try scanner.expectNextDataType(.eof);

    scanner = Scanner.init("<?");
    try scanner.expectNextDataType(.markup_tag);
    try scanner.expectNextMarkupTag(.pi);
    scanner.feed("fizz?>");

    try scanner.expectNextString("fizz");
    try scanner.expectNextString(null);

    try scanner.expectNextString(null);

    try scanner.expectNextDataType(.eof);

    scanner = Scanner.init("<?bar");
    try scanner.expectNextDataType(.markup_tag);
    try scanner.expectNextMarkupTag(.pi);
    try scanner.expectNextString("bar");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feed("?");
    try scanner.expectNextString("");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feed("?");
    try scanner.expectNextString("?");
    try scanner.expectNextString("");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feed("baz");
    try scanner.expectNextString("?");
    try scanner.expectNextString("baz");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feed("?>");
    try scanner.expectNextString("");
    try scanner.expectNextString(null);
    try scanner.expectNextString(null);
    try scanner.expectNextDataType(.eof);

    scanner = Scanner.init("");
    try scanner.expectNextDataType(.eof);
    scanner.feed("<");
    try scanner.expectNextDataType(.markup_tag);
    try scanner.expectNextMarkupTag(error.BufferUnderrun);
    scanner.feed("?");
    try scanner.expectNextMarkupTag(.pi);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feed("fo");
    try scanner.expectNextString("fo");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feed("o?");
    try scanner.expectNextString("o");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feed("bar");
    try scanner.expectNextString("?");
    try scanner.expectNextString("bar");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feed(" ");
    try scanner.expectNextString("");
    try scanner.expectNextString(null);
    try scanner.expectNextString(" ");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feed("?");
    try scanner.expectNextString("");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feed(" ");
    try scanner.expectNextString("?");
    try scanner.expectNextString(" ");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feed("?");
    try scanner.expectNextString("");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feed(">");
    try scanner.expectNextString(null);
    try scanner.expectNextDataType(.eof);
}

test "Scanner Comments" {
    var scanner: Scanner = undefined;

    scanner = Scanner.init("<!--" ++ "-->");
    try scanner.expectNextDataType(.markup_tag);
    try scanner.expectNextMarkupTag(.comment);
    try scanner.expectNextString("");
    try scanner.expectNextString(null);
    try scanner.expectNextMarkupTag(.comment_end);

    scanner = Scanner.init("<!--" ++ "-" ++ "-->");
    try scanner.expectNextDataType(.markup_tag);
    try scanner.expectNextMarkupTag(.comment);
    try scanner.expectNextString("");
    try scanner.expectNextString(null);
    try scanner.expectNextMarkupTag(.comment_end_triple_dash);
    try scanner.expectNextDataType(.eof);

    scanner = Scanner.init("<!--" ++ "--" ++ "-->");
    try scanner.expectNextDataType(.markup_tag);
    try scanner.expectNextMarkupTag(.comment);
    try scanner.expectNextString("");
    try scanner.expectNextString("--");
    try scanner.expectNextString(null);
    try scanner.expectNextMarkupTag(.comment_end);
    try scanner.expectNextDataType(.eof);

    scanner = Scanner.init("<!--" ++ "--" ++ "-" ++ "-->");
    try scanner.expectNextDataType(.markup_tag);
    try scanner.expectNextMarkupTag(.comment);
    try scanner.expectNextString("");
    try scanner.expectNextString("--");
    try scanner.expectNextString(null);
    try scanner.expectNextMarkupTag(.comment_end_triple_dash);
    try scanner.expectNextDataType(.eof);

    scanner = Scanner.init("<!-- <foo bar> -->");
    try scanner.expectNextDataType(.markup_tag);
    try scanner.expectNextMarkupTag(.comment);
    try scanner.expectNextString(" <foo bar> ");
    try scanner.expectNextString(null);
    try scanner.expectNextMarkupTag(.comment_end);
    try scanner.expectNextDataType(.eof);

    scanner = Scanner.init("<!--");
    try scanner.expectNextDataType(.markup_tag);
    try scanner.expectNextMarkupTag(.comment);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feed("--");
    try scanner.expectNextString("");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feed(">");
    try scanner.expectNextString(null);
    try scanner.expectNextMarkupTag(.comment_end);
    try scanner.expectNextDataType(.eof);

    scanner = Scanner.init("<!--");
    try scanner.expectNextDataType(.markup_tag);
    try scanner.expectNextMarkupTag(.comment);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feed("--a");
    try scanner.expectNextString("");
    try scanner.expectNextString("--");
    try scanner.expectNextString("a");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feed("-->");
    try scanner.expectNextString("");
    try scanner.expectNextString(null);
    try scanner.expectNextMarkupTag(.comment_end);
    try scanner.expectNextDataType(.eof);

    scanner = Scanner.init("<!--");
    try scanner.expectNextDataType(.markup_tag);
    try scanner.expectNextMarkupTag(.comment);
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feed("-");
    try scanner.expectNextString("");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feed("-");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feed("-");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feed("-");
    try scanner.expectNextString("--");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feed("-");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feed(" ");
    try scanner.expectNextString("--");
    try scanner.expectNextString("-");
    try scanner.expectNextString(" ");
    try scanner.expectNextString(error.BufferUnderrun);
    scanner.feed("-->");
    try scanner.expectNextString("");
    try scanner.expectNextString(null);
    try scanner.expectNextMarkupTag(.comment_end);
    try scanner.expectNextDataType(.eof);
}

test "Scanner CDATA Sections" {
    // TODO: actually do testing
    return error.SkipZigTest;
}
