pub const SkipError = error{
    PrematureEof,
    EmptyPI,
    NonWhitespaceCharacters,
    CommentTipleDashEnd,
    CommentDoubleDash,
    CommentSingleDashStart,
    InvalidMarkup,
    UnexpectedDoctypeDecl,
};

/// Simple helper for skipping the Prolog of a document, up until
/// encountering the first '<' token. Returns an error when encountering
/// a DTD, invalid markup, or EOF.
pub fn skipFull(tokenizer: *Tokenizer.Full) SkipError!void {
    return skipImpl(null, tokenizer, .{});
}

/// Same as `skipFull`, but over a streaming tokenizer, using a reader
/// and a read buffer to feed the tokenizer as needed.
pub fn skipStream(
    tokenizer: *Tokenizer.Stream,
    reader: anytype,
    read_buffer: []u8,
) (SkipError || @TypeOf(reader).Error)!void {
    return skipImpl(@TypeOf(reader), tokenizer, .{
        .reader = reader,
        .read_buffer = read_buffer,
    });
}

fn skipImpl(
    comptime MaybeReader: ?type,
    tokenizer: *if (MaybeReader != null) Tokenizer.Stream else Tokenizer.Full,
    mbr: parse_helper.MaybeBufferedReader(MaybeReader),
) !void {
    while (true) switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .non_markup, MaybeReader, mbr)) {
        .eof => return SkipError.PrematureEof,
        .text_data => switch (try parse_helper.skipWhitespaceTokenSrc(tokenizer.asTokenizer(), .non_markup, MaybeReader, mbr)) {
            .all_whitespace => {},
            .non_whitespace => return SkipError.NonWhitespaceCharacters,
        },
        .angle_bracket_left => return,
        .ampersand => return SkipError.NonWhitespaceCharacters,
        .pi_start => {
            switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .pi, MaybeReader, mbr)) {
                .eof => return SkipError.PrematureEof,
                .pi_end => return SkipError.EmptyPI,
                .text_data => {},
            }
            // TODO: validate that the PI data begins with a valid target name?
            try parse_helper.skipTokenStr(tokenizer.asTokenizer(), .pi, MaybeReader, mbr);
            switch (try parse_helper.nextTokenTypeNarrow(tokenizer.asTokenizer(), .pi, MaybeReader, mbr)) {
                .eof => return SkipError.PrematureEof,
                .text_data => unreachable, // text can't be followed immediately by text, they'd be interpreted as part of the same string of characters
                .pi_end => {},
            }
        },
        .cdata_start => return SkipError.NonWhitespaceCharacters,
        .invalid_cdata_start => return SkipError.NonWhitespaceCharacters,
        .cdata_end => return SkipError.NonWhitespaceCharacters,
        .comment_start => switch (try parse_helper.handleCommentSkip(tokenizer.asTokenizer(), MaybeReader, mbr)) {
            .normal_end => {},
            .invalid_end_triple_dash => return SkipError.CommentTipleDashEnd,
            .invalid_dash_dash => return SkipError.CommentDoubleDash,
            .eof => return SkipError.PrematureEof,
        },
        .invalid_comment_start_single_dash => return SkipError.CommentSingleDashStart,
        .angle_bracket_left_bang => return SkipError.InvalidMarkup,
        .dtd_start => return SkipError.UnexpectedDoctypeDecl,
        .invalid_dtd_start => return SkipError.InvalidMarkup,
    };
}

test skipFull {
    var tokenizer = Tokenizer.initFull(
        \\<?xml version="1.0" encoding="UTF-8"?>
        \\
        \\ <!-- random comment -->
        \\
        \\<foo/>
        \\
    );
    try std.testing.expectEqual({}, skipFull(&tokenizer.full));
}

test skipStream {
    var fbs = std.io.fixedBufferStream(
        \\<?xml version="1.0" encoding="UTF-8"?>
        \\
        \\ <!-- random comment -->
        \\
        \\<foo/>
        \\
    );
    var read_buffer: [8]u8 = undefined;
    var tokenizer = Tokenizer.initStream();
    try std.testing.expectEqual({}, skipStream(&tokenizer.stream, fbs.reader(), &read_buffer));
}

const std = @import("std");
const assert = std.debug.assert;

const builtin = @import("builtin");

const xml = @import("iksemel.zig");
const Tokenizer = xml.Tokenizer;

const parse_helper = @import("parse_helper.zig");
