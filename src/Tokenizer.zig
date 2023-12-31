const std = @import("std");
const assert = std.debug.assert;

const Tokenizer = @This();
src: []const u8,
index: usize,
state: State,

pub inline fn init(src: []const u8) Tokenizer {
    return .{
        .src = src,
        .index = 0,
        .state = .text_or_close_or_start,
    };
}

/// Replaces the current src with the given `src` parameter,
/// with subsequent calls to `lexer.next()` behaving as though
/// the given src parameter has been appended to all the text
/// that's already been tokenized.
/// Useful for streaming data.
pub fn feed(lexer: *Tokenizer, src: []const u8) void {
    lexer.src = src;
    lexer.index = 0;
}

pub const Token = struct {
    id: TokId,
    loc: Loc,
};

pub const Loc = struct {
    start: usize,
    end: usize,
};

pub const TokId = enum {
    /// End of the document.
    eof,

    /// The character '<' followed by a sequence of characters that don't form a valid tag.
    invalid_tag_start,

    /// '<?' followed by the PI target name
    pi_target,
    @"?>",

    @"<!--",
    /// The text in between two '--' tokens in a comment.
    comment_text,
    /// An invalid '--' in a comment which isn't followed by a '>'.
    /// This is an error, however tokenization may proceed.
    comment_invalid_double_dash,
    /// This token is invalid, but can be treated the same as '-->' for the purposes
    /// of deferring the error to a later stage.
    @"--->",
    @"-->",

    @"<![CDATA[",
    cdata_text,
    @"]]>",

    @"<!DOCTYPE",

    @"<!ELEMENT",
    @"<!ATTLIST",
    @"<!ENTITY",

    /// Denotes the closing bracket of a variety of opening tags.
    @">",
    /// Denotes the closing bracket of an element tag.
    @"/>",
};

pub fn next(lexer: *Tokenizer) Token {
    if (lexer.index == lexer.src.len) return makeEofToken(lexer.src.len);
    switch (lexer.state) {
        .text_or_close_or_start => switch (lexer.src[lexer.index]) {
            '<' => {
                const loc_start = lexer.index;
                lexer.index += 1;
                if (lexer.index == lexer.src.len) return .{
                    .id = .invalid_tag_start,
                    .loc = .{
                        .start = loc_start,
                        .end = lexer.index,
                    },
                };
                switch (lexer.src[lexer.index]) {
                    '?' => {
                        lexer.index += 1;
                        const pi_target_name_start = lexer.index;

                        while (lexer.index < lexer.src.len) { // loop ends immediately if we're already at the end of the src
                            if (lexer.src[lexer.index] == '?' and
                                lexer.index + 1 != lexer.src.len and
                                lexer.src[lexer.index] == '>' //
                            ) break; // '?>' terminates the PI

                            valid_pi_target_name: {
                                const cp_start = lexer.index;
                                const cp_len = std.unicode.utf8ByteSequenceLength(lexer.src[lexer.index]) catch |err| switch (err) {
                                    error.Utf8InvalidStartByte => {
                                        lexer.index += 1;
                                        break :valid_pi_target_name;
                                    },
                                };
                                lexer.index += cp_len;
                                if (lexer.index > lexer.src.len) {
                                    lexer.index = lexer.src.len;
                                    break :valid_pi_target_name;
                                }

                                const cp_bytes = lexer.src[cp_start..][0..cp_len];
                                const cp_value = std.unicode.utf8Decode(cp_bytes) catch |e| switch (e) {
                                    error.Utf8ExpectedContinuation,
                                    error.Utf8OverlongEncoding,
                                    error.Utf8EncodesSurrogateHalf,
                                    error.Utf8CodepointTooLarge,
                                    => @panic("TODO"),
                                };
                                if (!isNameChar(cp_value) or (cp_start == pi_target_name_start and !isNameStartChar(cp_value))) {
                                    break :valid_pi_target_name;
                                }

                                continue;
                            }

                            // not a valid PI target name codepoint
                            return .{
                                .id = .invalid_tag_start,
                                .loc = .{
                                    .start = loc_start,
                                    .end = lexer.index,
                                },
                            };
                        }

                        return .{
                            .id = .pi_target,
                            .loc = .{
                                .start = loc_start,
                                .end = lexer.index,
                            },
                        };
                    },

                    '!' => {
                        lexer.index += 1;

                        inline for (
                            comptime [_]struct { []const u8, State, TokId }{
                                .{ "--", .comment_start, .@"<!--" },
                                .{ "[CDATA[", .cdata_start, .@"<![CDATA[" },
                                .{ "DOCTYPE", .doctype_def, .@"<!DOCTYPE" },
                                .{ "ELEMENT", .element_type_decl, .@"<!ELEMENT" },
                                .{ "ATTLIST", .attlist_decl, .@"<!ATTLIST" },
                                .{ "ENTITY", .entity_decl, .@"<!ENTITY" },
                            },
                        ) |vals| {
                            const match, const new_state, const tok_id = vals;
                            if (std.mem.startsWith(u8, lexer.src[lexer.index..], match)) {
                                lexer.index += match.len;
                                lexer.state = new_state;
                                return .{
                                    .id = tok_id,
                                    .loc = .{
                                        .start = loc_start,
                                        .end = lexer.index,
                                    },
                                };
                            }
                        }

                        return .{
                            .id = .invalid_tag_start,
                            .loc = .{
                                .start = loc_start,
                                .end = lexer.index,
                            },
                        };
                    },

                    else => @panic("TODO"),
                }
            },
            else => @panic("TODO"),
        },

        // -- Processing Instructions tokenizing --

        // -- comment tokenizing --

        .comment_start,
        // this means the parsing of the document will end up in an error state,
        // however it'd be more useful to defer such an error to a later stage
        // than tokenization.
        .comment_invalid_double_dash,
        => {
            const loc_start = lexer.index;
            const double_dash = std.mem.indexOfPos(u8, lexer.src, lexer.index, "--") orelse lexer.src.len;
            lexer.state = .comment_text;
            lexer.index = double_dash;
            return .{
                .id = .comment_text,
                .loc = .{
                    .start = loc_start,
                    .end = lexer.index,
                },
            };
        },

        .comment_text => {
            const loc_start = lexer.index;
            lexer.index += "--".len;

            if (lexer.index < lexer.src.len) specific_case: {
                switch (lexer.src[lexer.index]) {
                    '>' => {
                        lexer.index += 1;
                        lexer.state = .text_or_close_or_start;
                        return .{
                            .id = .@"-->",
                            .loc = .{
                                .start = loc_start,
                                .end = lexer.index,
                            },
                        };
                    },
                    '-' => {
                        if (lexer.index + 1 == lexer.src.len) break :specific_case;
                        if (lexer.src[lexer.index + 1] != '>') break :specific_case;
                        lexer.index += 2;
                        lexer.state = .text_or_close_or_start;
                        return .{
                            .id = .@"--->",
                            .loc = .{
                                .start = loc_start,
                                .end = lexer.index,
                            },
                        };
                    },
                    else => {},
                }
            }

            lexer.state = .comment_invalid_double_dash;
            return .{
                .id = .comment_invalid_double_dash,
                .loc = .{
                    .start = loc_start,
                    .end = lexer.index,
                },
            };
        },

        // -- CDATA tokenizing --

        .cdata_start => {
            const loc_start = lexer.index;

            const cdata_end = std.mem.indexOfPos(u8, lexer.src, lexer.index, "]]>") orelse lexer.src.len;
            lexer.state = .cdata_text;
            lexer.index = cdata_end;
            return .{
                .id = .cdata_text,
                .loc = .{
                    .start = loc_start,
                    .end = lexer.index,
                },
            };
        },

        .cdata_text => {
            const loc_start = lexer.index;
            lexer.state = .text_or_close_or_start;
            lexer.index += "]]>".len;
            return .{
                .id = .@"]]>",
                .loc = .{
                    .start = loc_start,
                    .end = lexer.index,
                },
            };
        },

        .doctype_def => @panic("TODO"),

        .element_type_decl => @panic("TODO"),
        .attlist_decl => @panic("TODO"),
        .entity_decl => @panic("TODO"),
    }
}

const State = enum {
    text_or_close_or_start,

    comment_start,
    comment_text,
    comment_invalid_double_dash,

    cdata_start,
    cdata_text,

    doctype_def,

    element_type_decl,
    attlist_decl,
    entity_decl,
};

inline fn makeEofToken(src_len: usize) Token {
    return .{
        .id = .eof,
        .loc = .{
            .start = src_len,
            .end = src_len,
        },
    };
}

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

fn testTokenizer(src: []const u8, expected: []const Token) !void {
    var tk = Tokenizer.init(src);

    var actual = std.ArrayList(Token).init(std.testing.allocator);
    defer actual.deinit();

    while (true) {
        const tok = tk.next();
        try actual.append(tok);
        switch (tok.id) {
            .eof => break,
            else => {},
        }
    }

    try std.testing.expectEqualDeep(expected, actual.items);
}

test Tokenizer {
    try testTokenizer("<!---->", &[_]Token{
        .{
            .id = .@"<!--",
            .loc = .{ .start = 0, .end = 4 },
        },
        .{
            .id = .comment_text,
            .loc = .{ .start = 4, .end = 4 },
        },
        .{
            .id = .@"-->",
            .loc = .{ .start = 4, .end = 7 },
        },
        .{
            .id = .eof,
            .loc = .{ .start = 7, .end = 7 },
        },
    });
}
