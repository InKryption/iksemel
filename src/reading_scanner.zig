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
        src_reader: Src,
        src: []u8,
        const Self = @This();

        pub const Src = SrcReader;
        pub const Error = Src.Error;

        pub inline fn init(src_reader: SrcReader, read_buffer: []u8) Self {
            assert(read_buffer.len != 0);
            return .{
                .scanner = xml.Scanner.initStreaming(),
                .src_reader = src_reader,
                .src = read_buffer,
            };
        }

        pub const NextTypeError = Error;
        pub fn nextType(rs: *Self) NextTypeError!xml.Scanner.TokenType {
            assert(rs.src.len != 0);
            return rs.scanner.nextType() catch |err_1| return switch (err_1) {
                error.BufferUnderrun => while (true) {
                    try rs.feed();
                    break rs.scanner.nextType() catch |err_2| return switch (err_2) {
                        error.BufferUnderrun => continue,
                    };
                },
            };
        }

        pub const NextSrcError = Error;
        /// For a literal, the source is ephemeral (`.toRange` is illegal).
        /// For a range, the source can be obtained with `.getStr(rs.src)`
        /// until the next call to `nextType`, `nextSrc`, or `nextString`.
        pub fn nextSrc(rs: *Self) NextSrcError!?xml.Scanner.TokenSrc {
            assert(rs.src.len != 0);
            return rs.scanner.nextSrc() catch |first_underrun_err| switch (first_underrun_err) {
                error.BufferUnderrun => while (true) {
                    try rs.feed();
                    break rs.scanner.nextSrc() catch |err| switch (err) {
                        error.BufferUnderrun => continue,
                    };
                },
            };
        }

        pub const NextStringError = Error;
        /// The returned string is valid until the next call to `nextType`, `nextSrc`, or `nextString`.
        pub fn nextString(rs: *Self) NextStringError!?[]const u8 {
            const maybe_tok_src = try rs.nextSrc();
            const tok_src = maybe_tok_src orelse return null;
            return switch (tok_src) {
                .range => |range| range.getStr(rs.src),
                .literal => |literal| literal.toStr(),
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
            const bytes_read = try rs.src_reader.read(rs.src);
            if (bytes_read == 0) {
                rs.scanner.feedEof();
            } else {
                rs.scanner.feedInput(rs.src[0..bytes_read]);
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
