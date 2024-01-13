//! Compact representation of the raw structure of an XML document.
const std = @import("std");
const assert = std.debug.assert;
const xml = @import("xml.zig");

const Ast = @This();
str_buf: StrBuf,

pub fn deinit(tree: *Ast, allocator: std.mem.Allocator) void {
    tree.str_buf.deinit(allocator);
}

const StrBuf = union(enum) {
    owned: std.ArrayListUnmanaged(u8),
    referenced: []const u8,

    pub fn deinit(str_buf: *StrBuf, allocator: std.mem.Allocator) void {
        switch (str_buf.*) {
            .owned => |*owned| owned.deinit(allocator),
            .referenced => {},
        }
    }
};

pub const ParseFromReaderOptions = struct {
    allocator: std.mem.Allocator,
    read_buffer: []u8,
};
pub fn parseFromReader(
    /// `std.io.Reader(...)`
    src_reader: anytype,
    options: ParseFromReaderOptions,
) (std.mem.Allocator.Error || @TypeOf(src_reader).Error)!Ast {
    const allocator = options.allocator;
    var rs = xml.readingScanner(src_reader, options.read_buffer);
    _ = &rs;

    var tree: Ast = .{
        .str_buf = .{},
        .comment_buf = .{},
        .pi_buf = .{},
        .buf_ranges = .{},
        .attr_val_buf = .{},
        .attributes_buf = .{},
        .children_buf = .{},
        .elements = .{},
    };
    errdefer tree.deinit(allocator);

    try tree.elements.append(allocator, .{
        .name = .{ .start = 0, .end = 0 },
        .attributes = .{ .start = 0, .end = 0 },
        .children = std.math.maxInt(usize),
    });
    var current_element: usize = 0;
    _ = &current_element;

    while (true) {}

    return tree;
}

test Ast {
    if (true) return error.SkipZigTest;
    var fbs = std.io.fixedBufferStream(
        \\<?xml version="1.0" encoding="UTF-8"?>
        \\
        \\<foo>
        \\  Lorem ipsum
        \\  <bar fizz='buzz'><baz/></bar>
        \\</foo>
        \\
    );

    var read_buffer: [8]u8 = undefined;
    var tree = try Ast.parseFromReader(fbs.reader(), .{
        .allocator = std.testing.allocator,
        .read_buffer = &read_buffer,
    });
    defer tree.deinit(std.testing.allocator);
}
