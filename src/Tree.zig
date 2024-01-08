const std = @import("std");
const assert = std.debug.assert;
const xml = @import("xml.zig");

const Tree = @This();
str_data: std.ArrayListUnmanaged(u8),
comment_data: std.MultiArrayList(CommentData),
pi_data: std.ArrayListUnmanaged(ProcessingInstructionData),
data_ranges: std.ArrayListUnmanaged(DataRange),
attributes_data: std.MultiArrayList(AttributeData),
children_data: std.MultiArrayList(ChildData),
elements: std.MultiArrayList(Element),

pub fn deinit(tree: *Tree, allocator: std.mem.Allocator) void {
    tree.str_data.deinit(allocator);
    tree.comment_data.deinit(allocator);
    tree.pi_data.deinit(allocator);
    tree.data_ranges.deinit(allocator);
    tree.attributes_data.deinit(allocator);
    tree.children_data.deinit(allocator);
    tree.elements.deinit(allocator);
}

pub const DataRange = struct { start: usize, end: usize };
pub const CommentData = union(enum) {
    data: DataRange,
    invalid_dash_dash,
    invalid_end_triple_dash,
    end,
};
pub const ProcessingInstructionData = struct {
    /// Indexes `str_data`.
    target: DataRange,
    /// Indexes `str_data`.
    data: DataRange,
};
pub const AttributeData = union(enum) {
    /// Range in `str_data`.
    name: DataRange,
    eql,
    value_quote_single,
    value_quote_double,
    /// Range in `str_data`.
    val_str: DataRange,
    /// Range in `str_data`.
    val_char_ent_ref: DataRange,
};
pub const ChildData = struct {
    type: Type,
    /// Meaning depends on `type`.
    data_index: usize,
    /// `0` means this is the last node in the list.
    next_index: usize,

    pub const Type = enum {
        /// `data_index` indexes `data_ranges`, wherein the range indexes `str_data`.
        invalid_markup,
        /// `data_index` indexes `data_ranges`, wherein the range indexes `str_data`.
        char_ent_ref,
        /// `data_index` indexes `data_ranges`, wherein the range indexes `str_data`.
        text_data,
        /// `data_index` indexes `data_ranges`, wherein the range indexes `str_data`.
        cdata,
        /// `data_index` indexes `pi_data`.
        pi,
        /// `data_index` indexes `data_ranges`, wherein the range indexes `comment_data`.
        comment,
        /// `data_index` is undefined.
        invalid_cdata_stray_end,
        /// `data_index` indexes `data_ranges`, wherein the range indexes `str_data`.
        /// The value `std.math.maxInt(usize)` is used as the null value.
        element_close,
        /// `data_index` indexes `elements`.
        element,
    };
};
pub const Element = struct {
    /// Range in `str_data`.
    name: DataRange,
    /// The range indexes `attributes_data`.
    attributes: DataRange,
    /// `std.math.maxInt(usize)` is used as null.
    /// Indicates the first node in the list in `children_data`.
    children: usize,

    pub fn getName(element: Element, tree: *const Tree) ?[]const u8 {
        if (element.name.start == 0 and element.name.end == 0) return null;
        return tree.str_data.items[element.name.start..element.name.end];
    }

    pub inline fn attributeDataIterator(element: Element, tree: *const Tree) AttributeDataIterator {
        return .{
            .attributes = element.attributes,
            .current = element.attributes.start,
            .tree = tree,
        };
    }

    pub inline fn childIterator(element: Element, tree: *const Tree) ChildIterator {
        return ChildIterator.init(tree, element.children);
    }

    pub const AttributeDataIterator = struct {
        attributes: DataRange,
        current: usize,
        tree: *const Tree,

        pub inline fn reset(iter: *AttributeDataIterator) void {
            iter.current = iter.attributes.start;
        }

        pub inline fn next(iter: *AttributeDataIterator) ?AttributeData {
            if (iter.current == iter.attributes.end) return null;
            defer iter.current += 1;
            const slice = iter.tree.attributes_data.slice();
            return slice.get(iter.current);
        }
    };
};

pub const ChildIterator = struct {
    start: usize,
    current: ?usize,
    child_data: struct {
        type: []const ChildData.Type,
        data_index: []const usize,
        next_index: []const usize,
    },

    pub inline fn init(tree: *const Tree, start: usize) ChildIterator {
        const slice = tree.children_data.slice();
        return .{
            .start = start,
            .current = if (start == std.math.maxInt(usize)) null else start,
            .child_data = .{
                .type = slice.items(.type),
                .data_index = slice.items(.data_index),
                .next_index = slice.items(.next_index),
            },
        };
    }

    pub inline fn reset(iter: *ChildIterator) void {
        iter.current = iter.start;
    }

    pub fn next(iter: *ChildIterator) ?ChildData {
        const current = iter.current orelse return null;
        const next_index = iter.child_data.next_index[current];
        iter.current = if (next_index == 0) null else next_index;
        return .{
            .type = iter.child_data.type[current],
            .data_index = iter.child_data.data_index[current],
            .next_index = next_index,
        };
    }
};

pub const ParseOptions = struct {
    allocator: std.mem.Allocator,
    read_buffer: []u8,
};
pub fn parse(src_reader: anytype, options: ParseOptions) !Tree {
    const allocator = options.allocator;
    var rs = xml.readingScanner(src_reader, options.read_buffer);

    var tree: Tree = .{
        .str_data = .{},
        .comment_data = .{},
        .pi_data = .{},
        .data_ranges = .{},
        .attributes_data = .{},
        .children_data = .{},
        .elements = .{},
    };
    errdefer tree.deinit(allocator);

    try tree.elements.append(allocator, .{
        .name = .{ .start = 0, .end = 0 },
        .attributes = .{ .start = 0, .end = 0 },
        .children = std.math.maxInt(usize),
    });
    var current_element: usize = 0;

    while (true) {
        switch (try rs.nextTokenType()) {
            .eof => break,

            .invalid_markup,
            .char_ent_ref,
            .text_data,
            .cdata,
            => |tag| {
                const child_type: ChildData.Type = switch (tag) {
                    .invalid_markup => .invalid_markup,
                    .char_ent_ref => .char_ent_ref,
                    .text_data => .text_data,
                    .cdata => .cdata,
                    else => unreachable,
                };

                const new_range_idx = tree.data_ranges.items.len;
                const new_range = try tree.data_ranges.addOne(allocator);
                errdefer _ = tree.data_ranges.pop();

                const start = tree.str_data.items.len;
                assert(try rs.nextStringStream(tree.str_data.writer(allocator))); // there should be no possible way for invalid markup to not return a string.
                const end = tree.str_data.items.len;
                new_range.* = .{ .start = start, .end = end };
                try tree.appendChild(allocator, current_element, .{
                    .type = child_type,
                    .data_index = new_range_idx,
                });
            },
            .pi_target => {
                const new_pi_index = tree.pi_data.items.len;
                const new_pi = try tree.pi_data.addOne(allocator);

                const target_start = tree.str_data.items.len;
                new_pi.* = .{
                    .target = .{ .start = target_start, .end = target_start },
                    .data = .{ .start = 0, .end = 0 },
                };
                if (try rs.nextStringStream(tree.str_data.writer(allocator))) {
                    const target_end = tree.str_data.items.len;
                    new_pi.target.end = target_end;
                }

                try tree.appendChild(allocator, current_element, .{
                    .type = .pi,
                    .data_index = new_pi_index,
                });
            },
            .pi_data => {
                const child_index = tree.elements.items(.children)[current_element];

                const slice = tree.children_data.slice();
                const children_next_indices = slice.items(.next_index);
                var next_idx: usize = children_next_indices[child_index];
                while (children_next_indices[next_idx] != 0) next_idx = children_next_indices[next_idx];

                const data_start = tree.str_data.items.len;
                if (try rs.nextStringStream(tree.str_data.writer(allocator))) {
                    const data_end = tree.str_data.items.len;
                    tree.pi_data.items[slice.items(.data_index)[next_idx]].data = .{
                        .start = data_start,
                        .end = data_end,
                    };
                }
            },

            .comment => {
                const new_range_idx = tree.data_ranges.items.len;
                const new_range = try tree.data_ranges.addOne(allocator);
                errdefer _ = tree.data_ranges.pop();

                const start = tree.comment_data.len;

                while (true) {
                    const data_start = tree.str_data.items.len;
                    if (try rs.nextStringStream(tree.str_data.writer(allocator))) {
                        const data_end = tree.str_data.items.len;
                        try tree.comment_data.append(allocator, .{ .data = .{
                            .start = data_start,
                            .end = data_end,
                        } });
                    }

                    switch (try rs.nextTokenType()) {
                        .invalid_comment_dash_dash => try tree.comment_data.append(allocator, .invalid_dash_dash),
                        .invalid_comment_end_triple_dash => {
                            try tree.comment_data.append(allocator, .invalid_end_triple_dash);
                            break;
                        },
                        .comment_end => {
                            try tree.comment_data.append(allocator, .end);
                            break;
                        },
                        else => unreachable,
                    }
                }
                const end = tree.comment_data.len;
                new_range.* = .{
                    .start = start,
                    .end = end,
                };
                try tree.appendChild(allocator, current_element, .{
                    .type = .comment,
                    .data_index = new_range_idx,
                });
            },
            .invalid_comment_dash_dash => unreachable,
            .invalid_comment_end_triple_dash => unreachable,
            .comment_end => unreachable,

            .invalid_cdata_stray_end => try tree.appendChild(allocator, current_element, .{
                .type = .invalid_cdata_stray_end,
                .data_index = undefined,
            }),

            .element_open => {
                const new_element_idx = try tree.elements.addOne(allocator);
                errdefer _ = tree.elements.pop();
                var elements_slice = tree.elements.slice();

                const name_range: DataRange = blk: {
                    const name_start = tree.str_data.items.len;
                    if (!try rs.nextStringStream(tree.str_data.writer(allocator))) {
                        break :blk .{ .start = 0, .end = 0 };
                    }
                    const name_end = tree.str_data.items.len;
                    break :blk .{ .start = name_start, .end = name_end };
                };
                elements_slice.set(new_element_idx, .{
                    .name = name_range,
                    .attributes = .{ .start = 0, .end = 0 },
                    .children = std.math.maxInt(usize),
                });

                try tree.appendChild(allocator, current_element, .{
                    .type = .element,
                    .data_index = new_element_idx,
                });

                current_element += 1;
                assert(current_element == new_element_idx);
            },
            .element_open_end => {},
            .element_open_end_close_inline => current_element -= 1,

            .element_tag_whitespace => assert(try rs.nextStringStream(std.io.null_writer)),
            .attr_name => {
                const attributes: *DataRange = &tree.elements.items(.attributes)[current_element];
                if (attributes.start == 0 and attributes.end == 0) {
                    attributes.* = .{
                        .start = tree.attributes_data.len,
                        .end = tree.attributes_data.len,
                    };
                }

                const start = tree.str_data.items.len;
                assert(try rs.nextStringStream(tree.str_data.writer(allocator))); // there should be no possible way for attribute name to not return a string.
                const end = tree.str_data.items.len;

                try tree.attributes_data.append(allocator, .{ .name = .{
                    .start = start,
                    .end = end,
                } });

                attributes.end += 1;
            },
            .attr_eql => {
                const attributes: *DataRange = &tree.elements.items(.attributes)[current_element];
                if (attributes.start == 0 and attributes.end == 0) {
                    attributes.* = .{
                        .start = tree.attributes_data.len,
                        .end = tree.attributes_data.len,
                    };
                }

                try tree.attributes_data.append(allocator, .eql);
                attributes.end += 1;
            },
            .attr_value_quote_single,
            .attr_value_quote_double,
            => |tag| {
                const attr_data: AttributeData = switch (tag) {
                    .attr_value_quote_single => .value_quote_single,
                    .attr_value_quote_double => .value_quote_double,
                    else => unreachable,
                };

                const attributes: *DataRange = &tree.elements.items(.attributes)[current_element];
                if (attributes.start == 0 and attributes.end == 0) {
                    attributes.* = .{
                        .start = tree.attributes_data.len,
                        .end = tree.attributes_data.len,
                    };
                }

                try tree.attributes_data.append(allocator, attr_data);
                attributes.end += 1;

                while (true) {
                    const data_start = tree.str_data.items.len;
                    if (try rs.nextStringStream(tree.str_data.writer(allocator))) {
                        const data_end = tree.str_data.items.len;
                        try tree.attributes_data.append(allocator, .{ .val_str = .{
                            .start = data_start,
                            .end = data_end,
                        } });
                        attributes.end += 1;
                    }
                    switch (try rs.nextTokenType()) {
                        .attr_value_end => break,
                        .char_ent_ref => {
                            const char_ent_ref_start = tree.str_data.items.len;
                            if (try rs.nextStringStream(tree.str_data.writer(allocator))) {
                                const char_ent_ref_end = tree.str_data.items.len;
                                try tree.attributes_data.append(allocator, .{ .val_char_ent_ref = .{
                                    .start = char_ent_ref_start,
                                    .end = char_ent_ref_end,
                                } });
                                attributes.end += 1;
                            }
                        },
                        else => unreachable,
                    }
                }
            },
            .attr_value_end => unreachable,

            .element_close => {
                defer current_element -= 1;
                const curr_element_name: DataRange = tree.elements.items(.name)[current_element];

                const maybe_name_start = tree.str_data.items.len;
                if (try rs.nextStringStream(tree.str_data.writer(allocator))) {
                    const maybe_name_end = tree.str_data.items.len;
                    const name_range_value: DataRange = if (std.mem.eql(
                        u8,
                        tree.str_data.items[curr_element_name.start..curr_element_name.end],
                        tree.str_data.items[maybe_name_start..maybe_name_end],
                    )) blk: {
                        tree.str_data.shrinkRetainingCapacity(maybe_name_start);
                        break :blk curr_element_name;
                    } else .{
                        .start = maybe_name_start,
                        .end = maybe_name_end,
                    };
                    const new_index = tree.data_ranges.items.len;
                    try tree.data_ranges.append(allocator, name_range_value);
                    try tree.appendChild(allocator, current_element, .{
                        .type = .element_close,
                        .data_index = new_index,
                    });
                }
            },
        }
    }

    return tree;
}

fn appendChild(
    tree: *Tree,
    allocator: std.mem.Allocator,
    element_index: usize,
    child: struct {
        type: ChildData.Type,
        data_index: usize,
    },
) std.mem.Allocator.Error!void {
    const new_idx = try tree.children_data.addOne(allocator);
    tree.children_data.set(new_idx, .{
        .type = child.type,
        .data_index = child.data_index,
        .next_index = 0,
    });
    const child_index_ptr = &tree.elements.items(.children)[element_index];
    if (child_index_ptr.* == std.math.maxInt(usize)) {
        child_index_ptr.* = new_idx;
        return;
    }

    const children_next_indices = tree.children_data.items(.next_index);
    var next_idx_ptr: *usize = &children_next_indices[child_index_ptr.*];
    while (next_idx_ptr.* != 0) next_idx_ptr = &children_next_indices[next_idx_ptr.*];
    next_idx_ptr.* = new_idx;
}

test Tree {
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
    var tree = try Tree.parse(fbs.reader(), .{
        .allocator = std.testing.allocator,
        .read_buffer = &read_buffer,
    });
    defer tree.deinit(std.testing.allocator);

    const root = tree.elements.get(0);
    try std.testing.expectEqual(Tree.DataRange{ .start = 0, .end = 0 }, root.attributes);
    var root_child_iter = root.childIterator(&tree);
    {
        const child = root_child_iter.next() orelse return error.TestExpectedNotNull;
        try std.testing.expectEqual(Tree.ChildData.Type.pi, child.type);
        const pi_data = tree.pi_data.items[child.data_index];
        try std.testing.expectEqualStrings("xml", tree.str_data.items[pi_data.target.start..pi_data.target.end]);
        try std.testing.expectEqualStrings(" version=\"1.0\" encoding=\"UTF-8\"", tree.str_data.items[pi_data.data.start..pi_data.data.end]);
    }

    {
        const child = root_child_iter.next() orelse return error.TestExpectedNotNull;
        try std.testing.expectEqual(Tree.ChildData.Type.text_data, child.type);
        const range = tree.data_ranges.items[child.data_index];
        try std.testing.expectEqualStrings("\n\n", tree.str_data.items[range.start..range.end]);
    }

    {
        const foo = blk: {
            const child = root_child_iter.next() orelse return error.TestExpectedNotNull;
            try std.testing.expectEqual(Tree.ChildData.Type.element, child.type);
            const foo = tree.elements.get(child.data_index);
            try std.testing.expectEqualStrings("foo", foo.getName(&tree) orelse return error.TextExpectedEqual);
            try std.testing.expectEqual(Tree.DataRange{ .start = 0, .end = 0 }, foo.attributes);
            break :blk foo;
        };

        var foo_child_iter: Tree.ChildIterator = foo.childIterator(&tree);
        {
            const child = foo_child_iter.next() orelse return error.TextExpectedNotNull;
            try std.testing.expectEqual(Tree.ChildData.Type.text_data, child.type);
            const range = tree.data_ranges.items[child.data_index];
            try std.testing.expectEqualStrings("\n  Lorem ipsum\n  ", tree.str_data.items[range.start..range.end]);
        }
        {
            const bar = blk: {
                const child = foo_child_iter.next() orelse return error.TextExpectedNotNull;
                try std.testing.expectEqual(Tree.ChildData.Type.element, child.type);
                const bar = tree.elements.get(child.data_index);
                try std.testing.expectEqualStrings("bar", bar.getName(&tree) orelse return error.TextExpectedEqual);
                break :blk bar;
            };
            {
                var attr_iter = bar.attributeDataIterator(&tree);

                const name = attr_iter.next() orelse return error.TextExpectedNotNull;
                try std.testing.expectEqual(AttributeData.name, name);
                try std.testing.expectEqualStrings("fizz", tree.str_data.items[name.name.start..name.name.end]);

                try std.testing.expectEqual(@as(?AttributeData, .eql), attr_iter.next());

                try std.testing.expectEqual(@as(?AttributeData, .value_quote_single), attr_iter.next());

                const val_str = attr_iter.next() orelse return error.TextExpectedNotNull;
                try std.testing.expectEqual(AttributeData.val_str, val_str);
                try std.testing.expectEqualStrings("buzz", tree.str_data.items[val_str.val_str.start..val_str.val_str.end]);

                try std.testing.expectEqual(@as(?AttributeData, null), attr_iter.next());
            }
            var bar_child_iter = bar.childIterator(&tree);

            {
                const baz: Tree.Element = blk: {
                    const child = bar_child_iter.next() orelse return error.TestExpectedNotNull;

                    try std.testing.expectEqual(Tree.ChildData.Type.element, child.type);
                    const element = tree.elements.get(child.data_index);
                    try std.testing.expectEqualStrings("baz", element.getName(&tree) orelse return error.TestExpectedEqual);

                    break :blk element;
                };
                try std.testing.expectEqual(Tree.DataRange{ .start = 0, .end = 0 }, foo.attributes);
                try std.testing.expectEqual(@as(usize, std.math.maxInt(usize)), baz.children);

                var baz_child_iter = baz.childIterator(&tree);
                try std.testing.expectEqual(@as(?ChildData, null), baz_child_iter.next());
            }

            {
                const child = bar_child_iter.next() orelse return error.TestExpectedNotNull;
                try std.testing.expectEqual(Tree.ChildData.Type.element_close, child.type);
                const range = tree.data_ranges.items[child.data_index];
                try std.testing.expectEqualStrings("bar", tree.str_data.items[range.start..range.end]);
            }

            try std.testing.expectEqual(@as(?ChildData, null), bar_child_iter.next());
        }
        {
            const child = foo_child_iter.next() orelse return error.TextExpectedNotNull;
            try std.testing.expectEqual(ChildData.Type.text_data, child.type);
            const range = tree.data_ranges.items[child.data_index];
            try std.testing.expectEqualStrings("\n", tree.str_data.items[range.start..range.end]);
        }
        {
            const child = foo_child_iter.next() orelse return error.TextExpectedNotNull;
            try std.testing.expectEqual(ChildData.Type.element_close, child.type);
            const range = tree.data_ranges.items[child.data_index];
            try std.testing.expectEqualStrings("foo", tree.str_data.items[range.start..range.end]);
        }
        try std.testing.expectEqual(@as(?ChildData, null), foo_child_iter.next());
    }

    {
        const child = root_child_iter.next() orelse return error.TestExpectedNotNull;
        try std.testing.expectEqual(Tree.ChildData.Type.text_data, child.type);
        const range = tree.data_ranges.items[child.data_index];
        try std.testing.expectEqualStrings("\n", tree.str_data.items[range.start..range.end]);
    }

    try std.testing.expectEqual(@as(?Tree.ChildData, null), root_child_iter.next());
}
