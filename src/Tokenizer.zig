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

/// Replaces the current src with the given `src` parameter,
/// with subsequent calls to `scanner.next()` behaving as though
/// the given src parameter has been appended to all the text
/// that's already been tokenized.
///
/// NOTE: This invalidates the loc fields of any previously returned tokens.
/// Those must somehow continue to refer to the previous buffer, or
/// must instead be stored as `TokenWithString` or equivalent.
///
/// Useful for streaming data.
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
    }
}

pub const NextMarkupTagError = BufferError;
/// See the comments on the markup tag for more information.
pub fn nextMarkupTag(scanner: *Scanner) NextMarkupTagError!MarkupTag {
    if (scanner.index == scanner.src.len) return error.BufferUnderrun;
    switch (scanner.state) {
        .text_or_close_or_start => unreachable,
        .markup_tag => switch (scanner.src[scanner.index]) {
            '?' => {
                scanner.state = .pi_target_name;
                scanner.index += 1;
                return .pi;
            },
            '!' => @panic("TODO"),
            else => @panic("TODO"),
        },

        .pi_target_name => unreachable,
        .pi_target_name_qm => unreachable,
        .pi_target_name_qm_rab => unreachable,
        .pi_target_name_end => unreachable,

        .pi_data => unreachable,
        .pi_data_qm => unreachable,
        .pi_data_qm_rab => unreachable,
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
                scanner.state = .pi_target_name_qm_rab;
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
            for (scanner.src[scanner.index..], scanner.index..) |char, idx| {
                if (char != '?') continue;
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
    const scanner_test_log = std.log.scoped(.scanner_test);

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
    fn expectNextStringFull(scanner: *Scanner, expected: ?[]const u8) !void {
        var actual = std.ArrayList(u8).init(std.testing.allocator);
        defer actual.deinit();

        {
            const first_segment = (scanner.nextString() catch unreachable) orelse {
                try std.testing.expectEqual(expected, null);
                return;
            };
            try actual.appendSlice(first_segment);
        }

        while (scanner.nextString() catch unreachable) |segment| {
            try actual.appendSlice(segment);
        }

        if (expected == null or !std.mem.eql(u8, actual.items, expected.?)) {
            scanner_test_log.err("Expected {?s}, got {s}", .{ expected, actual.items });
            return error.TestExpectedEqual;
        }
    }
};

test "Scanner PI" {
    var scanner: Scanner = undefined;

    scanner = Scanner.init("<??>");
    try scanner.expectNextDataType(.markup_tag);
    try scanner.expectNextMarkupTag(.pi);
    try scanner.expectNextStringFull("");
    try scanner.expectNextStringFull(null);
    try scanner.expectNextDataType(.eof);

    scanner = Scanner.init("<?foo?>");
    try scanner.expectNextDataType(.markup_tag);
    try scanner.expectNextMarkupTag(.pi);
    try scanner.expectNextStringFull("foo");
    try scanner.expectNextStringFull(null);
    try scanner.expectNextDataType(.eof);

    scanner = Scanner.init("<?foo bar?>");
    try scanner.expectNextDataType(.markup_tag);
    try scanner.expectNextMarkupTag(.pi);
    try scanner.expectNextStringFull("foo");
    try scanner.expectNextStringFull(" bar");
    try scanner.expectNextDataType(.eof);

    scanner = Scanner.init("<");
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
