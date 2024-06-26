pub fn MaybeBufferedReader(comptime MaybeReader: ?type) type {
    const Reader = MaybeReader orelse return struct {
        reader: void = {},
        read_buffer: void = {},
    };
    return struct {
        reader: Reader,
        read_buffer: []u8,
    };
}

pub fn nextTokenTypeNarrow(
    tokenizer: *Tokenizer,
    comptime context: Tokenizer.Context,
    comptime MaybeReader: ?type,
    mbr: MaybeBufferedReader(MaybeReader),
) !Tokenizer.TokenType.Subset(context) {
    const token_type = try nextTokenType(tokenizer, context, MaybeReader, mbr);
    return token_type.intoNarrow(context).?;
}

pub fn nextTokenType(
    tokenizer: *Tokenizer,
    context: Tokenizer.Context,
    comptime MaybeReader: ?type,
    mbr: MaybeBufferedReader(MaybeReader),
) !Tokenizer.TokenType {
    const reader = mbr.reader;
    const read_buffer = mbr.read_buffer;

    if (MaybeReader == null) {
        return tokenizer.full.nextType(context);
    }
    return tokenizer.stream.nextType(context) catch while (true) {
        const bytes_read = try reader.read(read_buffer);
        if (bytes_read != 0) {
            tokenizer.stream.feedInput(read_buffer[0..bytes_read]);
        } else {
            tokenizer.stream.feedEof();
        }
        break tokenizer.stream.nextType(context) catch continue;
    };
}

pub fn nextTokenSegmentStr(
    tokenizer: *Tokenizer,
    context: Tokenizer.Context,
    comptime MaybeReader: ?type,
    mbr: MaybeBufferedReader(MaybeReader),
) !?[]const u8 {
    const maybe_segment = try nextTokenSegment(tokenizer, context, MaybeReader, mbr);
    const segment = maybe_segment orelse return null;
    return if (MaybeReader != null) segment else segment.toStr(tokenizer.src);
}

pub fn nextTokenSegment(
    tokenizer: *Tokenizer,
    context: Tokenizer.Context,
    comptime MaybeReader: ?type,
    mbr: MaybeBufferedReader(MaybeReader),
) !?if (MaybeReader != null) []const u8 else Tokenizer.Range {
    if (MaybeReader == null) return tokenizer.full.nextSrcRange(context);
    const reader = mbr.reader;
    const read_buffer = mbr.read_buffer;
    return tokenizer.stream.nextSrc(context) catch while (true) {
        const bytes_read = try reader.read(read_buffer);
        if (bytes_read != 0) {
            tokenizer.stream.feedInput(read_buffer[0..bytes_read]);
        } else {
            tokenizer.stream.feedEof();
        }
        break tokenizer.stream.nextSrc(context) catch continue;
    };
}

/// Consumes the token source, and attempts to conver the string
/// into an enum of the given type with a matching name.
/// Returns null if the source does not match any member
/// of the enumeration.
/// If the token source is too long to match any of the enum values,
/// it will skip the rest of the source.
pub fn nextTokenSrcAsEnum(
    tokenizer: *Tokenizer,
    context: Tokenizer.Context,
    comptime MaybeReader: ?type,
    mbr: MaybeBufferedReader(MaybeReader),
    comptime E: type,
) !?E {
    var str: std.BoundedArray(u8, blk: {
        var longest = 0;
        const fields = @typeInfo(E).Enum.fields;
        @setEvalBranchQuota(fields.len);
        for (fields) |field| longest = @max(longest, field.name.len);
        break :blk longest;
    }) = .{};

    if (MaybeReader != null) {
        while (try nextTokenSegment(tokenizer, context, MaybeReader, mbr)) |segment| {
            str.appendSlice(segment) catch {
                try skipTokenStr(tokenizer, context, MaybeReader, mbr);
                return null;
            };
        }
    } else {
        const range = tokenizer.full.nextSrcComplete(context);
        str.appendSlice(range.toStr(tokenizer.src)) catch return null;
    }

    return std.meta.stringToEnum(E, str.constSlice());
}

/// Consumes the token source, returns whether or not it contained non-whitespace.
pub fn skipWhitespaceTokenSrc(
    tokenizer: *Tokenizer,
    context: Tokenizer.Context,
    comptime MaybeReader: ?type,
    mbr: MaybeBufferedReader(MaybeReader),
) !enum { all_whitespace, non_whitespace } {
    if (MaybeReader != null) {
        while (try nextTokenSegment(tokenizer, context, MaybeReader, mbr)) |str| {
            if (std.mem.indexOfNone(u8, str, xml.prod.whitespace_set) == null) continue;
            return .non_whitespace;
        }
        return .all_whitespace;
    } else {
        const range = tokenizer.full.nextSrcComplete(context);
        const all_whitespace = std.mem.indexOfNone(u8, range.toStr(tokenizer.src), xml.prod.whitespace_set) == null;
        return if (all_whitespace) .all_whitespace else .non_whitespace;
    }
}

/// This function reads the source and asserts it's all whitespace
/// in safe modes, and simply skips it in unsafe modes.
pub fn skipWhitespaceSrcUnchecked(
    tokenizer: *Tokenizer,
    context: Tokenizer.Context,
    comptime MaybeReader: ?type,
    mbr: MaybeBufferedReader(MaybeReader),
) !void {
    if (std.debug.runtime_safety) {
        switch (try skipWhitespaceTokenSrc(tokenizer, context, MaybeReader, mbr)) {
            .all_whitespace => {},
            .non_whitespace => unreachable,
        }
    } else {
        try skipTokenStr(tokenizer, context, MaybeReader, mbr);
    }
}

/// Gets the immediate next token type; if it's `.tag_whitespace`, it skips
/// the whitespace token source, and then returns the next token type,
/// asserting it is not of `.tag_whitespace` (two can't be returned consecutively).
pub fn nextTokenTypeIgnoreTagWhitespace(
    tokenizer: *Tokenizer,
    comptime context: Tokenizer.Context,
    comptime MaybeReader: ?type,
    mbr: MaybeBufferedReader(MaybeReader),
) (if (MaybeReader) |Reader| Reader.Error else error{})!Tokenizer.TokenType.Subset(context) {
    switch (try nextTokenTypeNarrow(tokenizer, context, MaybeReader, mbr)) {
        else => |tag| return tag,
        .tag_whitespace => {},
    }
    try skipWhitespaceSrcUnchecked(tokenizer, context, MaybeReader, mbr);
    return switch (try nextTokenTypeNarrow(tokenizer, context, MaybeReader, mbr)) {
        else => |tag| tag,
        .tag_whitespace => unreachable,
    };
}

/// Expects the next token to be `.tag_whitespace`, and if it is,
/// it skips the whitespace source, and returns null. Otherwise,
/// it returns the actual token type.
pub fn skipIfTagWhitespaceOrGetNextTokType(
    tokenizer: *Tokenizer,
    comptime context: Tokenizer.Context,
    comptime MaybeReader: ?type,
    mbr: MaybeBufferedReader(MaybeReader),
) (if (MaybeReader) |Reader| Reader.Error else error{})!?Tokenizer.TokenType.Subset(context) {
    switch (try nextTokenTypeNarrow(tokenizer, context, MaybeReader, mbr)) {
        else => |tag| return tag,
        .tag_whitespace => {},
    }
    switch (try skipWhitespaceTokenSrc(tokenizer, context, MaybeReader, mbr)) {
        .all_whitespace => {},
        .non_whitespace => unreachable,
    }
    return null;
}

pub fn skipTokenStr(
    tokenizer: *Tokenizer,
    context: Tokenizer.Context,
    comptime MaybeReader: ?type,
    mbr: MaybeBufferedReader(MaybeReader),
) (if (MaybeReader) |Reader| Reader.Error else error{})!void {
    while (try nextTokenSegment(tokenizer, context, MaybeReader, mbr)) |_| {}
}

pub const CommentSkipResult = enum {
    normal_end,
    invalid_end_triple_dash,
    invalid_dash_dash,
    eof,
};
pub fn handleCommentSkip(
    tokenizer: *Tokenizer,
    comptime MaybeReader: ?type,
    mbr: MaybeBufferedReader(MaybeReader),
) (if (MaybeReader) |Reader| Reader.Error else error{})!CommentSkipResult {
    return switch (try nextTokenType(tokenizer, .comment, MaybeReader, mbr)) {
        .text_data => blk: {
            try skipTokenStr(tokenizer, .comment, MaybeReader, mbr);
            break :blk switch (try nextTokenType(tokenizer, .comment, MaybeReader, mbr)) {
                .text_data => unreachable,
                .invalid_comment_dash_dash => .invalid_dash_dash,
                .invalid_comment_end_triple_dash => .invalid_end_triple_dash,
                .comment_end => .normal_end,
                .eof => .eof,
                else => unreachable,
            };
        },
        .invalid_comment_dash_dash => .invalid_dash_dash,
        .invalid_comment_end_triple_dash => .invalid_end_triple_dash,
        .comment_end => .normal_end,
        .eof => .eof,
        else => unreachable,
    };
}

const std = @import("std");
const assert = std.debug.assert;

const xml = @import("iksemel.zig");
const Tokenizer = xml.Tokenizer;
