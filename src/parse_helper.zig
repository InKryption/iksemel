const std = @import("std");
const assert = std.debug.assert;

const iksemel = @import("iksemel.zig");
const Tokenizer = iksemel.Tokenizer;

pub inline fn checkSrcType(comptime optional: enum { is_optional, not_optional }, comptime T: type) void {
    const ExpectedSlice: type, //
    const ExpectedRange: type //
    = switch (optional) {
        .is_optional => .{ ?[]const u8, ?Tokenizer.Range },
        .not_optional => .{ []const u8, Tokenizer.Range },
    };
    switch (T) {
        ExpectedSlice => {},
        ExpectedRange => {},
        else => @compileError("Expected " ++ @typeName(ExpectedSlice) ++ " or " ++ @typeName(ExpectedRange) ++ ", instead got " ++ @typeName(T)),
    }
    comptime return; // this function must be run at `comptime`
}

pub fn MaybeBufferedReader(comptime MaybeReader: ?type) type {
    return struct {
        reader: (MaybeReader orelse void),
        read_buffer: if (MaybeReader != null) []u8 else void,
    };
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
        return tokenizer.nextTypeNoUnderrun(context);
    }
    return tokenizer.nextTypeStream(context) catch while (true) {
        const bytes_read = try reader.read(read_buffer);
        if (bytes_read != 0) {
            tokenizer.feedInput(read_buffer[0..bytes_read]);
        } else {
            tokenizer.feedEof();
        }
        break tokenizer.nextTypeStream(context) catch continue;
    };
}

pub fn nextTokenSegment(
    tokenizer: *Tokenizer,
    context: Tokenizer.Context,
    reader: anytype,
    read_buffer: []u8,
) !?[]const u8 {
    return tokenizer.nextSrcStream(context) catch while (true) {
        const bytes_read = try reader.read(read_buffer);
        if (bytes_read != 0) {
            tokenizer.feedInput(read_buffer[0..bytes_read]);
        } else {
            tokenizer.feedEof();
        }
        break tokenizer.nextSrcStream(context) catch continue;
    };
}

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
        while (try nextTokenSegment(tokenizer, context, mbr.reader, mbr.read_buffer)) |segment| {
            str.appendSlice(segment) catch {
                try skipTokenStr(tokenizer, context, MaybeReader, mbr);
                return null;
            };
        }
    } else {
        const range = tokenizer.nextSrcNoUnderrun(context);
        str.appendSlice(range.toStr(tokenizer.src)) catch return null;
    }

    return std.meta.stringToEnum(E, str.constSlice());
}

pub fn TokenSrcIter(comptime MaybeReader: ?type) type {
    return struct {
        iterated_once: if (MaybeReader == null) bool else void = if (MaybeReader == null) false,
        const Self = @This();

        pub fn next(
            iter: *Self,
            tokenizer: *Tokenizer,
            context: Tokenizer.Context,
            mbr: MaybeBufferedReader(MaybeReader),
        ) !?if (MaybeReader != null) []const u8 else Tokenizer.Range {
            if (MaybeReader != null) {
                return nextTokenSegment(tokenizer, context, mbr.reader, mbr.read_buffer);
            } else {
                defer iter.iterated_once = true;
                return if (iter.iterated_once) null else tokenizer.nextSrcNoUnderrun(context);
            }
        }
    };
}

pub fn skipWhitespaceTokenSrc(
    tokenizer: *Tokenizer,
    context: Tokenizer.Context,
    comptime MaybeReader: ?type,
    mbr: MaybeBufferedReader(MaybeReader),
) !enum { all_whitespace, non_whitespace } {
    var any_non_whitespace = false;
    if (MaybeReader != null) {
        while (try nextTokenSegment(tokenizer, context, mbr.reader, mbr.read_buffer)) |str| {
            if (std.mem.indexOfNone(u8, str, Tokenizer.whitespace_set) == null) continue;
            any_non_whitespace = false;
        }
    } else {
        const range = tokenizer.nextSrcNoUnderrun(context);
        any_non_whitespace = std.mem.indexOfNone(u8, range.toStr(tokenizer.src), Tokenizer.whitespace_set) != null;
    }
    return if (any_non_whitespace) .non_whitespace else .all_whitespace;
}

pub fn nextTokenTypeIgnoreTagWhitespace(
    tokenizer: *Tokenizer,
    context: Tokenizer.Context,
    comptime MaybeReader: ?type,
    mbr: MaybeBufferedReader(MaybeReader),
) (if (MaybeReader) |Reader| Reader.Error else error{})!Tokenizer.TokenType {
    switch (try nextTokenType(tokenizer, context, MaybeReader, mbr)) {
        else => |tag| return tag,
        .tag_whitespace => {},
    }
    switch (try skipWhitespaceTokenSrc(tokenizer, context, MaybeReader, mbr)) {
        .all_whitespace => {},
        .non_whitespace => unreachable,
    }
    return switch (try nextTokenType(tokenizer, context, MaybeReader, mbr)) {
        else => |tag| tag,
        .tag_whitespace => unreachable,
    };
}

/// Expects the next token to be `.tag_whitespace`, and if it is,
/// it skips the whitespace source, and returnsn ull. Otherwise,
/// it returns the actual token type.
pub fn expectAndSkipIfTagWhitespace(
    tokenizer: *Tokenizer,
    context: Tokenizer.Context,
    comptime MaybeReader: ?type,
    mbr: MaybeBufferedReader(MaybeReader),
) (if (MaybeReader) |Reader| Reader.Error else error{})!?Tokenizer.TokenType {
    switch (try nextTokenType(tokenizer, context, MaybeReader, mbr)) {
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
    if (MaybeReader == null) {
        _ = tokenizer.nextSrcNoUnderrun(context);
        return;
    }
    while (try nextTokenSegment(tokenizer, context, mbr.reader, mbr.read_buffer)) |_| {}
}

pub const CommentSkipResult = enum {
    normal_end,
    invalid_end_triple_dash,
    invalid_dash_dash,
};
pub fn handleCommentSkip(
    tokenizer: *Tokenizer,
    comptime MaybeReader: ?type,
    mbr: MaybeBufferedReader(MaybeReader),
) (if (MaybeReader) |Reader| Reader.Error else error{})!CommentSkipResult {
    return while (true) switch (try nextTokenType(tokenizer, .comment, MaybeReader, mbr)) {
        .text_data => try skipTokenStr(tokenizer, .comment, MaybeReader, mbr),
        .invalid_comment_dash_dash => break .invalid_dash_dash,
        .invalid_comment_end_triple_dash => break .invalid_end_triple_dash,
        .comment_end => break .normal_end,
        else => unreachable,
    };
}
