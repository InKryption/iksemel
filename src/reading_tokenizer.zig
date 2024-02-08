const std = @import("std");
const assert = std.debug.assert;

const iksemel = @import("iksemel.zig");

pub inline fn readingTokenizer(src_reader: anytype, read_buffer: []u8) ReadingTokenizer(@TypeOf(src_reader)) {
    const Rt = ReadingTokenizer(@TypeOf(src_reader));
    return Rt.init(src_reader, read_buffer);
}

pub fn ReadingTokenizer(comptime SrcReader: type) type {
    return struct {
        tokenizer: iksemel.Tokenizer,
        buffer: []u8,
        src: Src,
        const Self = @This();

        pub const Src = SrcReader;
        pub const Error = Src.Error;

        pub inline fn init(src: SrcReader, read_buffer: []u8) Self {
            assert(read_buffer.len != 0);
            return .{
                .tokenizer = iksemel.Tokenizer.initStreaming(),
                .src = src,
                .buffer = read_buffer,
            };
        }

        pub const NextTypeError = Error;
        pub fn nextType(rs: *Self) NextTypeError!iksemel.Tokenizer.TokenType {
            assert(rs.buffer.len != 0);
            return rs.tokenizer.nextType() catch |err_1| return switch (err_1) {
                error.BufferUnderrun => while (true) {
                    try rs.feed();
                    break rs.tokenizer.nextType() catch |err_2| return switch (err_2) {
                        error.BufferUnderrun => continue,
                    };
                },
            };
        }

        pub const NextStringError = Error;
        /// The returned string is valid until the next call to `nextType`, `nextSrc`, or `nextString`.
        pub fn nextString(rs: *Self) NextStringError!?[]const u8 {
            assert(rs.buffer.len != 0);
            return rs.tokenizer.nextString() catch |first_underrun_err| switch (first_underrun_err) {
                error.BufferUnderrun => while (true) {
                    try rs.feed();
                    break rs.tokenizer.nextString() catch |err| switch (err) {
                        error.BufferUnderrun => continue,
                    };
                },
            };
        }

        /// Writes the string to the stream.
        /// Returns true if there was any non-null component of the string, false otherwise.
        pub fn nextStringStream(rs: *Self, out_writer: anytype) (NextStringError || @TypeOf(out_writer).Error)!void {
            while (try rs.nextString()) |str| {
                try out_writer.writeAll(str);
            }
        }

        inline fn feed(rs: *Self) Src.Error!void {
            const bytes_read = try rs.src.read(rs.buffer);
            if (bytes_read == 0) {
                rs.tokenizer.feedEof();
            } else {
                rs.tokenizer.feedInput(rs.buffer[0..bytes_read]);
            }
        }
    };
}

test ReadingTokenizer {
    const TokTypeOrStr = union(enum) {
        tt: iksemel.Tokenizer.TokenType,
        str: ?[]const u8,

        pub fn format(
            tt_or_sr: @This(),
            comptime fmt_str: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = fmt_str;
            _ = options;
            switch (tt_or_sr) {
                .tt => |tt| try writer.print(".{s}", .{std.zig.fmtId(@tagName(tt))}),
                .str => |maybe_str| {
                    const str = maybe_str orelse return try writer.writeAll("null");
                    try writer.print("\"{}\"", .{std.zig.fmtEscapes(str)});
                },
            }
        }
    };

    const expected_tt_or_str_list: []const TokTypeOrStr = &.{
        .{ .tt = .pi_start },
        .{ .tt = .text_data },
        .{ .str = "xml version=\"1.0\" encoding=\"UTF-8\"" },
        .{ .tt = .pi_end },
        .{ .tt = .text_data },
        .{ .str = "\n\n" },
        .{ .tt = .angle_bracket_left },
        .{ .tt = .tag_token },
        .{ .str = "foo" },
        .{ .tt = .angle_bracket_right },
        .{ .tt = .text_data },
        .{ .str = "\n  Lorem ipsum\n  " },
        .{ .tt = .angle_bracket_left },
        .{ .tt = .tag_token },
        .{ .str = "bar" },
        .{ .tt = .tag_whitespace },
        .{ .str = " " },
        .{ .tt = .tag_token },
        .{ .str = "fizz" },
        .{ .tt = .equals },
        .{ .tt = .quote_single },
        .{ .tt = .text_data },
        .{ .str = "buzz" },
        .{ .tt = .quote_single },
        .{ .tt = .angle_bracket_right },
        .{ .tt = .angle_bracket_left },
        .{ .tt = .tag_token },
        .{ .str = "baz" },
        .{ .tt = .slash },
        .{ .tt = .angle_bracket_right },
        .{ .tt = .angle_bracket_left },
        .{ .tt = .slash },
        .{ .tt = .tag_token },
        .{ .str = "bar" },
        .{ .tt = .angle_bracket_right },
        .{ .tt = .text_data },
        .{ .str = "\n" },
        .{ .tt = .angle_bracket_left },
        .{ .tt = .slash },
        .{ .tt = .tag_token },
        .{ .str = "foo" },
        .{ .tt = .angle_bracket_right },
        .{ .tt = .text_data },
        .{ .str = "\n" },
        .{ .tt = .eof },
    };
    var fbs = std.io.fixedBufferStream(
        \\<?xml version="1.0" encoding="UTF-8"?>
        \\
        \\<foo>
        \\  Lorem ipsum
        \\  <bar fizz='buzz'><baz/></bar>
        \\</foo>
        \\
    );

    var rs_buffer: [2]u8 = undefined;
    var rs = readingTokenizer(fbs.reader(), &rs_buffer);

    var tt_or_str_list = std.ArrayList(TokTypeOrStr).init(std.testing.allocator);
    defer tt_or_str_list.deinit();
    defer for (tt_or_str_list.items) |tt_or_str| switch (tt_or_str) {
        .tt => {},
        .str => |str| std.testing.allocator.free(str orelse ""),
    };

    var str_buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer str_buffer.deinit();

    while (true) {
        const tt = try rs.nextType();
        try tt_or_str_list.append(.{ .tt = tt });
        if (tt == .eof) break;

        if (!tt.hasString()) continue;
        _ = try rs.nextStringStream(str_buffer.writer());
        const duped = try std.testing.allocator.dupe(u8, str_buffer.items);
        errdefer std.testing.allocator.free(duped);
        str_buffer.clearRetainingCapacity();
        try tt_or_str_list.append(.{ .str = duped });
    }

    try std.testing.expectEqualDeep(expected_tt_or_str_list, tt_or_str_list.items);
}
