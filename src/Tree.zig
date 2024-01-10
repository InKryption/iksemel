const std = @import("std");
const assert = std.debug.assert;
const xml = @import("xml.zig");

const Tree = @This();
str_buf: std.ArrayListUnmanaged(u8),
comment_buf: std.MultiArrayList(CommentData),
pi_buf: std.ArrayListUnmanaged(ProcessingInstructionData),
buf_ranges: std.ArrayListUnmanaged(DataRange),
attr_val_buf: std.MultiArrayList(AttributeValueData),
attributes_buf: std.MultiArrayList(AttributeData),
children_buf: std.MultiArrayList(ChildData),
elements: std.MultiArrayList(Element),

pub fn deinit(tree: *Tree, allocator: std.mem.Allocator) void {
    tree.str_buf.deinit(allocator);
    tree.comment_buf.deinit(allocator);
    tree.pi_buf.deinit(allocator);
    tree.buf_ranges.deinit(allocator);
    tree.attr_val_buf.deinit(allocator);
    tree.attributes_buf.deinit(allocator);
    tree.children_buf.deinit(allocator);
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
    /// Indexes `str_buf`.
    target: DataRange,
    /// Indexes `str_buf`.
    data: DataRange,
};

/// `... = '...'`:   `.{ .name = .{ .start = n, .end = m }, .eql = true,  .value = .{ .start = n, .end = m }  }`
/// `    = '...'`:   `.{ .name = .{ .start = 0, .end = 0 }, .eql = true,  .value = .{ .start = n, .end = m }  }`
/// `...   '...'`:   `.{ .name = .{ .start = n, .end = m }, .eql = false, .value = .{ .start = n, .end = m } }`
/// `... =      `:   `.{ .name = .{ .start = n, .end = m }, .eql = true,  .value = .{ .start = 0, .end = 0 }  }`
/// `      '...'`:   `.{ .name = .{ .start = 0, .end = 0 }, .eql = false, .value = .{ .start = n, .end = m }  }`
/// `    =      `:   `.{ .name = .{ .start = 0, .end = 0 }, .eql = true,  .value = .{ .start = 0, .end = 0 }  }`
/// `...        `:   `.{ .name = .{ .start = n, .end = m }, .eql = false, .value = .{ .start = 0, .end = 0 }  }`
pub const AttributeData = struct {
    /// Indexes `str_buf`.
    /// `.{ .start = 0, .end = 0 }` represents no name - this is an error.
    name: DataRange,
    /// Indicates whether or not there is actually an equals sign in between the value and the string.
    /// False is an error.
    eql: bool,
    /// Indexes `attr_val_buf`.
    /// `.{ .start = 0, .end = 0 }` represents no value - this is an error.
    value: DataRange,

    pub inline fn typed(attr: AttributeData, tree: *const Tree) Typed {
        const name_is_null = attr.name.start == 0 and attr.name.end == 0;
        const value_is_null = attr.value.start == 0 and attr.value.end == 0;
        return .{
            .name = if (name_is_null) null else tree.str_buf.items[attr.name.start..attr.name.end],

            .eql = attr.eql,
            .value = if (value_is_null) null else blk: {
                const slice = tree.attr_val_buf.slice();
                break :blk .{
                    .len = slice.len,
                    .str = slice.items(.str).ptr,
                    .kind = slice.items(.kind).ptr,
                };
            },
        };
    }
    pub const Typed = struct {
        name: ?[]const u8,
        eql: bool,
        value: ?Value,

        /// SoA version of `AttributeValueData`.
        pub const Value = struct {
            len: usize,
            /// Indexes `str_buf`.
            str: [*]const DataRange,
            kind: [*]const AttributeValueData.Kind,

            pub inline fn getStrings(value: Value) []const DataRange {
                return value.str[0..value.len];
            }
            pub inline fn getKinds(value: Value) []const AttributeValueData.Kind {
                return value.kind[0..value.len];
            }
        };
    };
};

pub const AttributeValueData = struct {
    /// Indexes `str_buf`.
    str: DataRange,
    kind: Kind,

    pub const Kind = enum {
        /// Regular string data
        str,
        /// A character or entity reference.
        ref,
    };
};

pub const ChildData = struct {
    type: Type,
    /// Meaning depends on `type`.
    data_index: usize,
    /// `0` means this is the last node in the list.
    next_index: usize,

    pub const Type = enum {
        /// `data_index` indexes `data_ranges`, wherein the range indexes `str_buf`.
        invalid_markup,
        /// `data_index` indexes `data_ranges`, wherein the range indexes `str_buf`.
        char_ent_ref,
        /// `data_index` indexes `data_ranges`, wherein the range indexes `str_buf`.
        text_data,
        /// `data_index` indexes `data_ranges`, wherein the range indexes `str_buf`.
        cdata,
        /// `data_index` indexes `pi_buf`.
        pi,
        /// `data_index` indexes `data_ranges`, wherein the range indexes `comment_buf`.
        comment,
        /// `data_index` is undefined.
        invalid_cdata_stray_end,
        /// `data_index` indexes `data_ranges`, wherein the range indexes `str_buf`.
        /// The value `std.math.maxInt(usize)` is used as the null value.
        element_close,
        /// `data_index` indexes `elements`.
        element,
    };

    pub inline fn typed(child_data: ChildData, tree: *const Tree) Typed {
        return switch (child_data.type) {
            inline //
            .invalid_markup,
            .char_ent_ref,
            .text_data,
            .cdata,
            => |tag| blk: {
                const range = tree.buf_ranges.items[child_data.data_index];
                break :blk @unionInit(Typed, @tagName(tag), tree.str_buf.items[range.start..range.end]);
            },
            .pi => blk: {
                const pi = tree.pi_buf.items[child_data.data_index];
                if (pi.target.start == 0 and pi.target.end == 0) break :blk .{ .pi = null };
                const target = tree.str_buf.items[pi.target.start..pi.target.end];
                if (pi.data.start == 0 and pi.data.end == 0) break :blk .{ .pi = .{
                    .target = target,
                    .data = null,
                } };
                const data = tree.str_buf.items[pi.data.start..pi.data.end];
                break :blk .{ .pi = .{
                    .target = target,
                    .data = data,
                } };
            },
            .comment => .{ .comment = tree.buf_ranges.items[child_data.data_index] },
            .invalid_cdata_stray_end => .invalid_cdata_stray_end,
            .element_close => blk: {
                if (child_data.data_index == std.math.maxInt(usize)) break :blk .{ .element_close = null };
                const range = tree.buf_ranges.items[child_data.data_index];
                break :blk .{ .element_close = tree.str_buf.items[range.start..range.end] };
            },
            .element => .{ .element = tree.elements.get(child_data.data_index) },
        };
    }
    pub const Typed = union(Type) {
        invalid_markup: []const u8,
        char_ent_ref: []const u8,
        text_data: []const u8,
        cdata: []const u8,
        pi: ?ProcessingInstructions,
        /// Indexes `tree.comment_buf`
        comment: DataRange,
        invalid_cdata_stray_end,
        element_close: ?[]const u8,
        element: Element,

        pub const ProcessingInstructions = struct {
            target: []const u8,
            data: ?[]const u8,
        };
    };
};

pub const Element = struct {
    /// Range in `str_buf`.
    name: DataRange,
    /// The range indexes `attributes_buf`.
    attributes: DataRange,
    /// `std.math.maxInt(usize)` is used as null.
    /// Indicates the first node in the list in `children_data`.
    children: usize,

    pub fn getName(element: Element, tree: *const Tree) ?[]const u8 {
        if (element.name.start == 0 and element.name.end == 0) return null;
        return tree.str_buf.items[element.name.start..element.name.end];
    }

    pub inline fn attributeDataIterator(element: Element, tree: *const Tree) AttributeDataIterator {
        return .{
            .attributes = element.attributes,
            .current = element.attributes.start,
            .slice = tree.attributes_buf.slice(),
        };
    }

    pub const AttributeDataIterator = struct {
        attributes: DataRange,
        current: usize,
        slice: std.MultiArrayList(AttributeData).Slice,

        pub inline fn reset(iter: *AttributeDataIterator) void {
            iter.current = iter.attributes.start;
        }

        pub inline fn next(iter: *AttributeDataIterator) ?AttributeData {
            if (iter.current == iter.attributes.end) return null;
            defer iter.current += 1;
            return iter.slice.get(iter.current);
        }
    };

    pub inline fn childIterator(element: Element, tree: *const Tree) ChildIterator {
        return ChildIterator.init(tree, element.children);
    }
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
        const slice = tree.children_buf.slice();
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

                const new_range_idx = tree.buf_ranges.items.len;
                const new_range = try tree.buf_ranges.addOne(allocator);
                errdefer _ = tree.buf_ranges.pop();

                const start = tree.str_buf.items.len;
                assert(try rs.nextStringStream(tree.str_buf.writer(allocator))); // there should be no possible way for invalid markup to not return a string.
                const end = tree.str_buf.items.len;
                new_range.* = .{ .start = start, .end = end };
                try tree.appendChild(allocator, current_element, .{
                    .type = child_type,
                    .data_index = new_range_idx,
                });
            },
            .pi_target => {
                const new_pi_index = tree.pi_buf.items.len;
                const new_pi = try tree.pi_buf.addOne(allocator);

                const target_start = tree.str_buf.items.len;
                new_pi.* = .{
                    .target = .{ .start = target_start, .end = target_start },
                    .data = .{ .start = 0, .end = 0 },
                };
                if (try rs.nextStringStream(tree.str_buf.writer(allocator))) {
                    const target_end = tree.str_buf.items.len;
                    new_pi.target.end = target_end;
                }

                try tree.appendChild(allocator, current_element, .{
                    .type = .pi,
                    .data_index = new_pi_index,
                });
            },
            .pi_data => {
                const child_index = tree.elements.items(.children)[current_element];

                const slice = tree.children_buf.slice();
                const children_next_indices = slice.items(.next_index);
                var next_idx: usize = children_next_indices[child_index];
                while (children_next_indices[next_idx] != 0) next_idx = children_next_indices[next_idx];

                const data_start = tree.str_buf.items.len;
                if (try rs.nextStringStream(tree.str_buf.writer(allocator))) {
                    const data_end = tree.str_buf.items.len;
                    tree.pi_buf.items[slice.items(.data_index)[next_idx]].data = .{
                        .start = data_start,
                        .end = data_end,
                    };
                }
            },

            .doctype_decl => @panic("TODO"),
            .doctype_decl_whitespace => unreachable,
            .doctype_name => unreachable,

            .comment => {
                const new_range_idx = tree.buf_ranges.items.len;
                const new_range = try tree.buf_ranges.addOne(allocator);
                errdefer _ = tree.buf_ranges.pop();

                const start = tree.comment_buf.len;

                while (true) {
                    const data_start = tree.str_buf.items.len;
                    if (try rs.nextStringStream(tree.str_buf.writer(allocator))) {
                        const data_end = tree.str_buf.items.len;
                        try tree.comment_buf.append(allocator, .{ .data = .{
                            .start = data_start,
                            .end = data_end,
                        } });
                    }

                    switch (try rs.nextTokenType()) {
                        .invalid_comment_dash_dash => try tree.comment_buf.append(allocator, .invalid_dash_dash),
                        .invalid_comment_end_triple_dash => {
                            try tree.comment_buf.append(allocator, .invalid_end_triple_dash);
                            break;
                        },
                        .comment_end => {
                            try tree.comment_buf.append(allocator, .end);
                            break;
                        },
                        else => unreachable,
                    }
                }
                const end = tree.comment_buf.len;
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
                    const name_start = tree.str_buf.items.len;
                    if (!try rs.nextStringStream(tree.str_buf.writer(allocator))) {
                        break :blk .{ .start = 0, .end = 0 };
                    }
                    const name_end = tree.str_buf.items.len;
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
                        .start = tree.attributes_buf.len,
                        .end = tree.attributes_buf.len,
                    };
                }

                const start = tree.str_buf.items.len;
                assert(try rs.nextStringStream(tree.str_buf.writer(allocator))); // there should be no possible way for attribute name to not return a string.
                const end = tree.str_buf.items.len;
                const range: DataRange = .{
                    .start = start,
                    .end = end,
                };
                try tree.attributes_buf.append(allocator, .{
                    .name = range,
                    .eql = false,
                    .value = .{ .start = 0, .end = 0 },
                });
                attributes.end += 1;
            },
            .attr_eql => blk: {
                const attributes: *DataRange = &tree.elements.items(.attributes)[current_element];
                if (attributes.start == 0 and attributes.end == 0) {
                    attributes.* = .{
                        .start = tree.attributes_buf.len,
                        .end = tree.attributes_buf.len,
                    };
                }
                {
                    const attrs = tree.attributes_buf.items(.eql);
                    if (!attrs[attrs.len - 1]) {
                        attrs[attrs.len - 1] = true;
                        break :blk;
                    }
                }
                try tree.attributes_buf.append(allocator, .{
                    .name = .{ .start = 0, .end = 0 },
                    .eql = true,
                    .value = .{ .start = 0, .end = 0 },
                });
                attributes.end += 1;
            },
            .attr_value_quote_single,
            .attr_value_quote_double,
            => {
                const attributes: *DataRange = &tree.elements.items(.attributes)[current_element];
                if (attributes.start == 0 and attributes.end == 0) {
                    attributes.* = .{
                        .start = tree.attributes_buf.len,
                        .end = tree.attributes_buf.len,
                    };
                }

                const attr_val_range: *DataRange = blk: {
                    try tree.attributes_buf.ensureUnusedCapacity(allocator, 1);
                    const attr_vals = tree.attributes_buf.items(.value);

                    if (attr_vals[attr_vals.len - 1].start == 0 and
                        attr_vals[attr_vals.len - 1].end == 0 //
                    ) {
                        attr_vals[attr_vals.len - 1] = .{
                            .start = tree.attr_val_buf.len,
                            .end = tree.attr_val_buf.len,
                        };
                        break :blk &attr_vals[attr_vals.len - 1];
                    } else {
                        tree.attributes_buf.appendAssumeCapacity(.{
                            .name = .{ .start = 0, .end = 0 },
                            .eql = false,
                            .value = .{
                                .start = tree.attr_val_buf.len,
                                .end = tree.attr_val_buf.len,
                            },
                        });
                        attributes.end += 1;
                        break :blk &attr_vals[attr_vals.len - 1];
                    }
                };

                while (true) {
                    const data_start = tree.str_buf.items.len;
                    if (try rs.nextStringStream(tree.str_buf.writer(allocator))) {
                        const data_end = tree.str_buf.items.len;
                        try tree.attr_val_buf.append(allocator, .{
                            .str = .{ .start = data_start, .end = data_end },
                            .kind = .str,
                        });
                        attr_val_range.end += 1;
                    }
                    switch (try rs.nextTokenType()) {
                        .attr_value_end => break,
                        .char_ent_ref => {
                            const char_ent_ref_start = tree.str_buf.items.len;
                            if (try rs.nextStringStream(tree.str_buf.writer(allocator))) {
                                const ref_end = tree.str_buf.items.len;
                                try tree.attr_val_buf.append(allocator, .{
                                    .str = .{ .start = char_ent_ref_start, .end = ref_end },
                                    .kind = .ref,
                                });
                                attr_val_range.end += 1;
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

                const maybe_name_start = tree.str_buf.items.len;
                if (try rs.nextStringStream(tree.str_buf.writer(allocator))) {
                    const maybe_name_end = tree.str_buf.items.len;
                    const name_range_value: DataRange = if (std.mem.eql(
                        u8,
                        tree.str_buf.items[curr_element_name.start..curr_element_name.end],
                        tree.str_buf.items[maybe_name_start..maybe_name_end],
                    )) blk: {
                        tree.str_buf.shrinkRetainingCapacity(maybe_name_start);
                        break :blk curr_element_name;
                    } else .{
                        .start = maybe_name_start,
                        .end = maybe_name_end,
                    };
                    const new_index = tree.buf_ranges.items.len;
                    try tree.buf_ranges.append(allocator, name_range_value);
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
    const new_idx = try tree.children_buf.addOne(allocator);
    tree.children_buf.set(new_idx, .{
        .type = child.type,
        .data_index = child.data_index,
        .next_index = 0,
    });
    const child_index_ptr = &tree.elements.items(.children)[element_index];
    if (child_index_ptr.* == std.math.maxInt(usize)) {
        child_index_ptr.* = new_idx;
        return;
    }

    const children_next_indices = tree.children_buf.items(.next_index);
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
        const child = (root_child_iter.next() orelse return error.TestExpectedNotNull).typed(&tree);
        try std.testing.expectEqual(Tree.ChildData.Type.pi, child);
        const pi = child.pi orelse return error.TestExpectedNotNull;
        try std.testing.expectEqualStrings("xml", pi.target);
        try std.testing.expectEqualStrings(" version=\"1.0\" encoding=\"UTF-8\"", pi.data orelse return error.TestExpectedNotNull);
    }

    try std.testing.expectEqualDeep(
        ChildData.Typed{ .text_data = "\n\n" },
        (root_child_iter.next() orelse return error.TestExpectedNotNull).typed(&tree),
    );

    {
        const foo = blk: {
            const child = (root_child_iter.next() orelse return error.TestExpectedNotNull).typed(&tree);
            try std.testing.expectEqual(Tree.ChildData.Type.element, child);
            const foo = child.element;
            try std.testing.expectEqualStrings("foo", foo.getName(&tree) orelse return error.TextExpectedEqual);
            try std.testing.expectEqual(Tree.DataRange{ .start = 0, .end = 0 }, foo.attributes);
            break :blk foo;
        };

        var foo_child_iter: Tree.ChildIterator = foo.childIterator(&tree);
        {
            const child = (foo_child_iter.next() orelse return error.TestExpectedNotNull).typed(&tree);
            try std.testing.expectEqual(Tree.ChildData.Type.text_data, child);
            try std.testing.expectEqualStrings("\n  Lorem ipsum\n  ", child.text_data);
        }
        {
            const bar = blk: {
                const child = (foo_child_iter.next() orelse return error.TestExpectedNotNull).typed(&tree);
                try std.testing.expectEqual(Tree.ChildData.Type.element, child);
                const bar = child.element;
                try std.testing.expectEqualStrings("bar", bar.getName(&tree) orelse return error.TextExpectedEqual);
                break :blk bar;
            };
            {
                var attr_iter = bar.attributeDataIterator(&tree);

                const attr = (attr_iter.next() orelse return error.TestExpectedNotNull).typed(&tree);
                try std.testing.expectEqualStrings("fizz", attr.name orelse return error.TestExpectedNotNull);
                try std.testing.expect(attr.eql);

                const value = attr.value orelse return error.TestExpectedNotNull;
                try std.testing.expectEqualSlices(AttributeValueData.Kind, &.{.str}, value.getKinds());

                var actual_strings = std.ArrayList([]const u8).init(std.testing.allocator);
                defer actual_strings.deinit();
                for (value.getStrings(), value.getKinds()) |range, _| {
                    const string = tree.str_buf.items[range.start..range.end];
                    try actual_strings.append(string);
                }
                try std.testing.expectEqualDeep(@as([]const []const u8, &.{"buzz"}), actual_strings.items);

                try std.testing.expectEqual(@as(?AttributeData, null), attr_iter.next());
            }

            var bar_child_iter = bar.childIterator(&tree);

            {
                const baz: Tree.Element = blk: {
                    const child = (bar_child_iter.next() orelse return error.TestExpectedNotNull).typed(&tree);

                    try std.testing.expectEqual(Tree.ChildData.Type.element, child);
                    const element = child.element;
                    try std.testing.expectEqualStrings("baz", element.getName(&tree) orelse return error.TestExpectedEqual);

                    break :blk element;
                };
                try std.testing.expectEqual(Tree.DataRange{ .start = 0, .end = 0 }, foo.attributes);
                try std.testing.expectEqual(@as(usize, std.math.maxInt(usize)), baz.children);

                var baz_child_iter = baz.childIterator(&tree);
                try std.testing.expectEqual(@as(?ChildData, null), baz_child_iter.next());
            }

            {
                const child = (bar_child_iter.next() orelse return error.TestExpectedNotNull).typed(&tree);
                try std.testing.expectEqual(Tree.ChildData.Type.element_close, child);
                try std.testing.expectEqualStrings("bar", child.element_close orelse return error.TestExpectedNotNull);
            }

            try std.testing.expectEqual(@as(?ChildData, null), bar_child_iter.next());
        }
        {
            const child = (foo_child_iter.next() orelse return error.TestExpectedNotNull).typed(&tree);
            try std.testing.expectEqual(ChildData.Type.text_data, child);
            try std.testing.expectEqualStrings("\n", child.text_data);
        }
        {
            const child = (foo_child_iter.next() orelse return error.TestExpectedNotNull).typed(&tree);
            try std.testing.expectEqual(ChildData.Type.element_close, child);
            try std.testing.expectEqualStrings("foo", child.element_close orelse return error.TestExpectedNotNull);
        }
        try std.testing.expectEqual(@as(?ChildData, null), foo_child_iter.next());
    }

    {
        const child = root_child_iter.next() orelse return error.TestExpectedNotNull;
        try std.testing.expectEqual(Tree.ChildData.Type.text_data, child.type);
        const range = tree.buf_ranges.items[child.data_index];
        try std.testing.expectEqualStrings("\n", tree.str_buf.items[range.start..range.end]);
    }

    try std.testing.expectEqual(@as(?Tree.ChildData, null), root_child_iter.next());
}
