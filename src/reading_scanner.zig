const std = @import("std");
const assert = std.debug.assert;

const xml = @import("xml.zig");

pub inline fn readingScanner(src_reader: anytype, read_buffer: []u8) ReadingScanner(@TypeOf(src_reader)) {
    const Rs = ReadingScanner(@TypeOf(src_reader));
    return Rs.init(src_reader, read_buffer);
}

pub fn ReadingScanner(comptime SrcReader: type) type {
    return struct {
        scanner: xml.Scanner,
        src: SrcReader,
        buffer: []u8,
        const Self = @This();

        pub fn init(src_reader: SrcReader, read_buffer: []u8) Self {
            assert(read_buffer.len != 0);
            return .{
                .scanner = xml.Scanner.initStreaming(""),
                .src = src_reader,
                .buffer = read_buffer,
            };
        }

        pub const Error = xml.Scanner.EofError || SrcReader.Error;
        pub fn nextTokenType(rs: *Self) Error!xml.Scanner.TokenType {
            assert(rs.buffer.len != 0);
            return rs.scanner.nextTokenType() catch |err_1| return switch (err_1) {
                error.PrematureEof => |e| e,
                error.BufferUnderrun => while (true) {
                    try rs.feed();
                    break rs.scanner.nextTokenType() catch |err_2| return switch (err_2) {
                        error.PrematureEof => |e| e,
                        error.BufferUnderrun => continue,
                    };
                },
            };
        }

        /// The returned string is valid until the next call to `nextString` or `nextTokenType`.
        pub fn nextString(rs: *Self) Error!?[]const u8 {
            assert(rs.buffer.len != 0);
            return rs.scanner.nextString() catch |err_1| return switch (err_1) {
                error.PrematureEof => |e| e,
                error.BufferUnderrun => while (true) {
                    try rs.feed();
                    break rs.scanner.nextString() catch |err_2| return switch (err_2) {
                        error.PrematureEof => |e| e,
                        error.BufferUnderrun => continue,
                    };
                },
            };
        }

        /// Writes the string to the stream.
        /// Returns true if there was any non-null component of the string, false otherwise.
        pub fn nextStringStream(rs: *Self, out_writer: anytype) (Error || @TypeOf(out_writer).Error)!bool {
            const first_str = (try rs.nextString()) orelse return false;
            try out_writer.writeAll(first_str);

            while (try rs.nextString()) |str| {
                try out_writer.writeAll(str);
            }
            return true;
        }

        inline fn feed(rs: *Self) SrcReader.Error!void {
            const bytes_read = try rs.src.read(rs.buffer);
            if (bytes_read == 0) {
                rs.scanner.feedEof();
            } else {
                rs.scanner.feedInput(rs.buffer[0..bytes_read]);
            }
        }
    };
}

test ReadingScanner {
    const TokTypeOrStr = union(enum) {
        tt: xml.Scanner.TokenType,
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
        .{ .tt = .pi_target },
        .{ .str = "xml" },
        .{ .tt = .pi_data },
        .{ .str = " version=\"1.0\" encoding=\"UTF-8\"" },
        .{ .tt = .text_data },
        .{ .str = "\n\n" },
        .{ .tt = .element_open },
        .{ .str = "foo" },
        .{ .tt = .element_open_end },
        .{ .tt = .text_data },
        .{ .str = "\n  Lorem ipsum\n  " },
        .{ .tt = .element_open },
        .{ .str = "bar" },
        .{ .tt = .attr_name },
        .{ .str = "fizz" },
        .{ .tt = .attr_eql },
        .{ .tt = .attr_value_quote_single },
        .{ .str = "buzz" },
        .{ .tt = .attr_value_end },
        .{ .tt = .element_open_end },
        .{ .tt = .element_open },
        .{ .str = "baz" },
        .{ .tt = .element_open_end_close_inline },
        .{ .tt = .element_close },
        .{ .str = "bar" },
        .{ .tt = .text_data },
        .{ .str = "\n" },
        .{ .tt = .element_close },
        .{ .str = "foo" },
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
    var rs = readingScanner(fbs.reader(), &rs_buffer);

    var tt_or_str_list = std.ArrayList(TokTypeOrStr).init(std.testing.allocator);
    defer tt_or_str_list.deinit();
    defer for (tt_or_str_list.items) |tt_or_str| switch (tt_or_str) {
        .tt => {},
        .str => |str| std.testing.allocator.free(str orelse ""),
    };

    var str_buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer str_buffer.deinit();

    while (true) {
        const tt = try rs.nextTokenType();
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
