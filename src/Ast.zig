//! Compact representation of the raw structure of an XML document.
const std = @import("std");
const assert = std.debug.assert;
const xml = @import("xml.zig");

const Ast = @This();
str_buf: std.ArrayListUnmanaged(u8),
/// Is either an alias to `str_buf.items`, or a copy of the source slice from which this was parsed.
str: []const u8,

invalid_data: std.MultiArrayList(InvalidData),
text_datas: std.MultiArrayList(TextData),
index_pairs: std.ArrayListUnmanaged(IndexPair),
/// The first element is the root node, and will be represented with `.kind = .element`.
nodes: std.MultiArrayList(Node),

pub fn deinit(tree: *Ast, allocator: std.mem.Allocator) void {
    tree.str_buf.deinit(allocator);
    tree.invalid_data.deinit(allocator);
    tree.text_datas.deinit(allocator);
    tree.index_pairs.deinit(allocator);
    tree.nodes.deinit(allocator);
}

pub const helpers = @import("ast/helpers.zig");

pub const Node = struct {
    kind: Kind,
    idx_a: usize,
    idx_b: usize,
    /// The next sibling node in `nodes`.
    /// The end of the linked list is represented with the value 0.
    next: usize,

    pub const Kind = enum {
        /// `index_pairs[idx_a]` -> `str[lhs..rhs]` for the name. `idx_a == std.math.maxInt(usize)` means no name.
        /// `index_pairs[idx_b]` -> `nodes[lhs..rhs]` for the attribute values.
        /// If this is the direct child of something other than a node with `.kind = .element`, this represents '<.../>'.
        element_empty,
        /// `nodes[idx_a]` -> `.kind = .element_empty` for the name and attributes. This will be 0 to represent '<>' (no name, no attributes).
        /// `nodes[idx_b]` for the first child. The value '0' is used to represent no children.
        element,
        /// `index_pairs[idx_a]` -> `str[lhs..rhs]` for the name. `idx_a == std.math.maxInt(usize)` means no name.
        /// `index_pairs[idx_b]` -> `invalid_data[lhs..rhs]` if `idx_b != std.math.maxInt(usize)`, for the data after the name.
        /// This is generally the terminating child of an element. Matching is done by depth, and not by name.
        element_close,

        /// `index_pairs[idx_a]` -> `str[lhs..rhs]` for the name. `idx_a == std.math.maxInt(usize)` means no name (' = value').
        /// `index_pairs[idx_b]` -> `text_datas[lhs..rhs]` for the value. `idx_b == std.math.maxInt(usize)` means no value ('name = ')
        attribute,
        /// The same as `.attribute`, but missing the delimiting equals sign ('name value' instead of 'name = value'),
        /// and either the name and/or the value are guaranteed to not be `std.math.maxInt(usize)`.
        attribute_no_eql,

        /// `str[idx_a..idx_b]` for the PI data.
        pi,
        /// `str[idx_a..idx_b]` for the text data.
        text_data,
        /// `str[idx_a..idx_b]` for the character or entity reference.
        reference,
        /// Same as `.reference`, but doesn't end with ';'.
        reference_invalid_end,
        /// `str[idx_a..idx_b]` for the cdata.
        cdata,
        /// `text_data[idx_a..idx_b]` for the comment data.
        comment,
    };
};

pub const IndexPair = struct {
    lhs: usize,
    rhs: usize,
};

pub const InvalidData = struct {
    tag: xml.Scanner.TokenType,
    /// Indexes `str_buf`.
    range: IndexPair,
};

pub const TextData = struct {
    kind: Kind,
    /// Indexes `str_buf`.
    range: IndexPair,

    pub const Kind = enum {
        str,
        ref,
        /// Same as `.ref`, but doesn't end in ';'.
        ref_invalid_end,
        /// '--'. The `range` field isn't meaningful.
        invalid_comment_dash_dash,
        /// '<'. The `range` field isn't meaningful.
        invlaid_left_angle_bracket,
    };
};

pub const ParseConfig = struct {
    allow_element_open_name_leading_whitespace: bool,
    allow_element_close_name_leading_whitespace: bool,

    const default: ParseConfig = .{
        .allow_element_open_name_leading_whitespace = false,
        .allow_element_close_name_leading_whitespace = false,
    };
};

pub const ParseFromSliceOptions = struct {
    allocator: std.mem.Allocator,
    config: ParseConfig = ParseConfig.default,
};

pub inline fn parseFromSlice(
    slice: []const u8,
    options: ParseFromSliceOptions,
) std.mem.Allocator.Error!Ast {
    return parseFromSource(null, slice, {}, options.allocator, options.config);
}

pub const ParseFromReaderOptions = struct {
    allocator: std.mem.Allocator,
    read_buffer: []u8,
    config: ParseConfig = ParseConfig.default,
};

pub inline fn parseFromReader(
    reader: anytype,
    options: ParseFromReaderOptions,
) (std.mem.Allocator.Error || @TypeOf(reader).Error)!Ast {
    return parseFromSource(@TypeOf(reader), reader, options.read_buffer, options.allocator, options.config);
}

fn parseFromSource(
    comptime SrcReader: ?type,
    src_reader_or_slice: (SrcReader orelse []const u8),
    read_buffer: if (SrcReader != null) []u8 else void,
    allocator: std.mem.Allocator,
    config: ParseConfig,
) !Ast {
    var scanner = if (SrcReader == null)
        xml.Scanner.initComplete(src_reader_or_slice)
    else
        xml.readingScanner(src_reader_or_slice, read_buffer);
    var ast: Ast = .{
        .str_buf = .{},
        .str = if (SrcReader == null) src_reader_or_slice else undefined,

        .invalid_data = .{},
        .text_datas = .{},
        .index_pairs = .{},
        .nodes = .{},
    };
    errdefer ast.deinit(allocator);

    // stack of indexes to `nodes`, where the pointed-to node is `.kind = .element`.
    var element_stack = std.ArrayList(usize).init(allocator);
    defer element_stack.deinit();

    try ast.nodes.append(allocator, .{
        .kind = .element,
        .idx_a = 0,
        .idx_b = 0,
        .next = 0,
    });

    mainloop: while (true) {
        const current_element = element_stack.getLastOrNull() orelse 0;
        switch (try getNextNextTokenType(&scanner)) {
            .text_data => {
                try ast.nodes.ensureUnusedCapacity(allocator, 1);
                const new_range = try ast.nextStringRange(allocator, &scanner);

                const new_node_idx = ast.nodes.addOneAssumeCapacity();
                ast.nodes.set(new_node_idx, .{
                    .kind = .text_data,
                    .idx_a = new_range.lhs,
                    .idx_b = new_range.rhs,
                    .next = 0,
                });
                ast.appendChildNodeToElement(current_element, new_node_idx);
            },
            .tag_whitespace => unreachable,
            .tag_token => unreachable,
            .invalid_token => @panic("TODO"),

            .eof => break,

            .equals => unreachable,

            .lparen => unreachable,
            .rparen => unreachable,

            .pipe => unreachable,
            .comma => unreachable,
            .qmark => unreachable,
            .asterisk => unreachable,
            .plus => unreachable,

            .quote_single => unreachable,
            .quote_double => unreachable,

            .angle_bracket_left => {
                var name: ?IndexPair = null;
                var attributes: ?IndexPair = null;

                var queued_tok_type: ?xml.Scanner.TokenType = null;
                const elem_terminating_tok_type: xml.Scanner.TokenType = while (true) switch (blk: {
                    defer queued_tok_type = null;
                    break :blk queued_tok_type orelse try getNextNextTokenType(&scanner);
                }) {
                    .tag_whitespace => {
                        if (name == null and !config.allow_element_open_name_leading_whitespace) {
                            name = .{ .lhs = 0, .rhs = 0 };
                        }
                        while (true) {
                            const seg = scanner.nextString() catch |err| {
                                if (@TypeOf(scanner) != xml.Scanner) return err;
                                unreachable;
                            };
                            if (seg == null) break;
                        }
                    },
                    .tag_token => {
                        const str_range = try ast.nextStringRange(allocator, &scanner);
                        if (name == null) {
                            name = str_range;
                            continue;
                        }

                        if (attributes == null) attributes = .{ .lhs = ast.nodes.len, .rhs = ast.nodes.len };
                        const attributes_ptr = &attributes.?;
                        try ast.nodes.ensureUnusedCapacity(allocator, 1);
                        const name_idx = ast.index_pairs.items.len;
                        try ast.index_pairs.append(allocator, str_range);

                        ast.nodes.appendAssumeCapacity(.{
                            .kind = .attribute_no_eql,
                            .idx_a = name_idx,
                            .idx_b = std.math.maxInt(usize),
                            .next = 0,
                        });
                        attributes_ptr.rhs += 1;
                    },
                    .equals => {
                        if (name == null) name = .{ .lhs = 0, .rhs = 0 };
                        if (attributes == null) attributes = .{ .lhs = ast.nodes.len, .rhs = ast.nodes.len };
                        const attributes_ptr = &attributes.?;

                        const slice = ast.nodes.slice(); // careful to not use this after modifying `ast.nodes`
                        const kinds: []Node.Kind = slice.items(.kind); // careful not to use this after modifying `ast.nodes`
                        const idx_bs: []const usize = slice.items(.idx_b); // careful not to use this after modifying `ast.nodes`

                        const no_attributes_yet = attributes_ptr.lhs == attributes_ptr.rhs;
                        if (no_attributes_yet or
                            kinds[attributes_ptr.rhs - 1] != .attribute_no_eql or
                            idx_bs[attributes_ptr.rhs - 1] != std.math.maxInt(usize) //
                        ) {
                            if (!no_attributes_yet) assert( //
                                kinds[attributes_ptr.rhs - 1] == .attribute or
                                idx_bs[attributes_ptr.rhs - 1] != std.math.maxInt(usize) //
                            );
                            try ast.nodes.append(allocator, .{
                                .kind = .attribute,
                                .idx_a = std.math.maxInt(usize),
                                .idx_b = std.math.maxInt(usize),
                                .next = 0,
                            });
                            attributes_ptr.rhs += 1;
                            continue;
                        }
                        kinds[attributes_ptr.rhs - 1] = .attribute;
                    },
                    .quote_single,
                    .quote_double,
                    => |start_tag| {
                        if (name == null) name = .{ .lhs = 0, .rhs = 0 };
                        if (attributes == null) attributes = .{ .lhs = ast.nodes.len, .rhs = ast.nodes.len };
                        const attributes_ptr = &attributes.?;

                        if (attributes_ptr.lhs == attributes_ptr.rhs or
                            ast.nodes.items(.idx_b)[attributes_ptr.rhs - 1] != std.math.maxInt(usize) //
                        ) {
                            try ast.nodes.append(allocator, .{
                                .kind = .attribute_no_eql,
                                .idx_a = std.math.maxInt(usize),
                                .idx_b = std.math.maxInt(usize),
                                .next = 0,
                            });
                            attributes_ptr.rhs += 1;
                        }
                        const attribute_idx = ast.nodes.len - 1;

                        const idx_bs: []usize = ast.nodes.items(.idx_b); // careful not to use this after modifying `ast.nodes`

                        idx_bs[attribute_idx] = ast.index_pairs.items.len;
                        const text_datas_range = try ast.index_pairs.addOne(allocator);
                        text_datas_range.* = .{
                            .lhs = ast.text_datas.len,
                            .rhs = ast.text_datas.len,
                        };

                        const quote_or_eof_tag: xml.Scanner.TokenType = while (true) switch (try getNextNextTokenType(&scanner)) {
                            .text_data => {
                                try ast.text_datas.ensureUnusedCapacity(allocator, 1);
                                const str_range = try ast.nextStringRange(allocator, &scanner);
                                ast.text_datas.appendAssumeCapacity(.{
                                    .kind = .str,
                                    .range = str_range,
                                });
                                text_datas_range.rhs += 1;
                            },
                            .angle_bracket_left => {
                                try ast.text_datas.append(allocator, .{
                                    .kind = .invlaid_left_angle_bracket,
                                    .range = .{ .lhs = 0, .rhs = 0 },
                                });
                                text_datas_range.rhs += 1;
                            },
                            .ampersand => {
                                try ast.text_datas.ensureUnusedCapacity(allocator, 1);
                                const str_range: IndexPair, //
                                const ref_kind: TextData.Kind //
                                = try ast.getReferenceRangeAndKind(allocator, &scanner);
                                assert( //
                                    ref_kind == .ref or
                                    ref_kind == .ref_invalid_end //
                                );
                                ast.text_datas.appendAssumeCapacity(.{
                                    .kind = ref_kind,
                                    .range = str_range,
                                });
                                text_datas_range.rhs += 1;
                            },
                            // ending the token sequence.
                            .quote_single,
                            .quote_double,
                            .eof,
                            => |end_tag| break end_tag,
                            else => unreachable,
                        };
                        assert(quote_or_eof_tag == .eof or quote_or_eof_tag == start_tag);
                        if (quote_or_eof_tag == .eof) break quote_or_eof_tag;
                    },

                    .angle_bracket_right,
                    .slash_angle_bracket_right,
                    .eof,
                    => |tag| break tag,

                    else => unreachable,
                };
                try element_stack.ensureUnusedCapacity(1);
                try ast.nodes.ensureUnusedCapacity(allocator, 2);
                try ast.index_pairs.ensureUnusedCapacity(allocator, 2);

                const name_range = name orelse IndexPair{ .lhs = 0, .rhs = 0 };
                const name_range_idx = if (name_range.lhs != name_range.rhs) ast.index_pairs.items.len else std.math.maxInt(usize);
                if (name_range_idx != std.math.maxInt(usize)) ast.index_pairs.appendAssumeCapacity(name_range);

                const attr_range = attributes orelse IndexPair{ .lhs = 0, .rhs = 0 };
                const attr_range_idx = if (attr_range.lhs != attr_range.rhs) ast.index_pairs.items.len else std.math.maxInt(usize);
                if (attr_range_idx != std.math.maxInt(usize)) ast.index_pairs.appendAssumeCapacity(attr_range);

                const empty_element_node_idx = ast.nodes.addOneAssumeCapacity();
                ast.nodes.set(empty_element_node_idx, .{
                    .kind = .element_empty,
                    .idx_a = name_range_idx,
                    .idx_b = attr_range_idx,
                    .next = 0,
                });
                switch (elem_terminating_tok_type) {
                    .slash_angle_bracket_right => ast.appendChildNodeToElement(current_element, empty_element_node_idx),
                    .angle_bracket_right, .eof => {
                        const element_node_idx = ast.nodes.addOneAssumeCapacity();
                        ast.nodes.set(element_node_idx, .{
                            .kind = .element,
                            .idx_a = empty_element_node_idx,
                            .idx_b = 0,
                            .next = 0,
                        });
                        ast.appendChildNodeToElement(current_element, element_node_idx);
                        element_stack.appendAssumeCapacity(element_node_idx);
                    },
                    else => unreachable,
                }
            },
            .angle_bracket_right => unreachable,

            .square_bracket_left => unreachable,
            .square_bracket_right => unreachable,

            .percent => unreachable,
            .ampersand => {
                try ast.nodes.ensureUnusedCapacity(allocator, 1);
                const str_range: IndexPair, //
                const ref_kind: TextData.Kind //
                = try ast.getReferenceRangeAndKind(allocator, &scanner);

                const new_node_idx = ast.nodes.addOneAssumeCapacity();
                ast.nodes.set(new_node_idx, .{
                    .kind = switch (ref_kind) {
                        .ref => .reference,
                        .ref_invalid_end => .reference_invalid_end,
                        else => unreachable,
                    },
                    .idx_a = str_range.lhs,
                    .idx_b = str_range.rhs,
                    .next = 0,
                });
                ast.appendChildNodeToElement(current_element, new_node_idx);
            },
            .semicolon => unreachable,
            .invalid_reference_end => unreachable,

            .angle_bracket_left_slash => {
                _ = element_stack.popOrNull();

                var name: ?IndexPair = null;
                var invalid_data: ?IndexPair = null;

                const terminating_tok_type: xml.Scanner.TokenType = while (true) switch (try getNextNextTokenType(&scanner)) {
                    .tag_token => {
                        try ast.invalid_data.ensureUnusedCapacity(allocator, 1);
                        const str_range = try ast.nextStringRange(allocator, &scanner);
                        if (name == null) {
                            name = str_range;
                            continue;
                        }
                        if (invalid_data == null) {
                            invalid_data = .{ .lhs = ast.invalid_data.len, .rhs = ast.invalid_data.len };
                        }
                        const data_range = &invalid_data.?;
                        ast.invalid_data.appendAssumeCapacity(.{
                            .tag = .tag_token,
                            .range = str_range,
                        });
                        data_range.rhs += 1;
                    },
                    .tag_whitespace => {
                        if (name == null and !config.allow_element_close_name_leading_whitespace) {
                            name = .{ .lhs = 0, .rhs = 0 };
                        }
                        while (true) {
                            const seg = scanner.nextString() catch |err| {
                                if (@TypeOf(scanner) != xml.Scanner) return err;
                                unreachable;
                            };
                            if (seg == null) break;
                        }
                    },
                    .equals,
                    .quote_single,
                    .quote_double,
                    .ampersand,
                    .angle_bracket_left,
                    => |tag| {
                        if (name == null and !config.allow_element_close_name_leading_whitespace) {
                            name = .{ .lhs = 0, .rhs = 0 };
                        }
                        try ast.invalid_data.append(allocator, .{
                            .tag = tag,
                            .range = .{ .lhs = 0, .rhs = 0 },
                        });
                    },
                    .text_data => |tag| {
                        if (name == null and !config.allow_element_close_name_leading_whitespace) {
                            name = .{ .lhs = 0, .rhs = 0 };
                        }
                        try ast.invalid_data.ensureUnusedCapacity(allocator, 1);
                        ast.invalid_data.appendAssumeCapacity(.{
                            .tag = tag,
                            .range = try ast.nextStringRange(allocator, &scanner),
                        });
                    },
                    .angle_bracket_right,
                    .slash_angle_bracket_right,
                    .eof,
                    => |tag| break tag,
                    else => unreachable,
                };

                try ast.nodes.ensureUnusedCapacity(allocator, 1);
                try ast.index_pairs.ensureUnusedCapacity(allocator, 2);

                const name_range = name orelse IndexPair{ .lhs = 0, .rhs = 0 };
                const name_range_idx = if (name_range.lhs != name_range.rhs) ast.index_pairs.items.len else std.math.maxInt(usize);
                if (name_range_idx != std.math.maxInt(usize)) ast.index_pairs.appendAssumeCapacity(name_range);

                const data_range = invalid_data orelse IndexPair{ .lhs = 0, .rhs = 0 };
                const data_range_idx = if (data_range.lhs != data_range.rhs) ast.index_pairs.items.len else std.math.maxInt(usize);
                if (data_range_idx != std.math.maxInt(usize)) ast.index_pairs.appendAssumeCapacity(data_range);

                const elem_close_node_idx = ast.nodes.addOneAssumeCapacity();
                ast.nodes.set(elem_close_node_idx, .{
                    .kind = .element_close,
                    .idx_a = name_range_idx,
                    .idx_b = data_range_idx,
                    .next = 0,
                });
                ast.appendChildNodeToElement(current_element, elem_close_node_idx);

                switch (terminating_tok_type) {
                    .angle_bracket_right => {},
                    .slash_angle_bracket_right => @panic("TODO"),
                    .eof => break :mainloop,
                    else => unreachable,
                }
            },
            .slash_angle_bracket_right => unreachable,

            .pi_start => {
                try ast.nodes.ensureUnusedCapacity(allocator, 1);
                const range: IndexPair = switch (try getNextNextTokenType(&scanner)) {
                    .text_data => try ast.nextStringRange(allocator, &scanner),
                    .pi_end => .{ .lhs = 0, .rhs = 0 },
                    .eof => @panic("TODO"),
                    else => unreachable,
                };
                switch (try getNextNextTokenType(&scanner)) {
                    .pi_end => {},
                    .eof => {},
                    else => unreachable,
                }
                const new_node_idx = ast.nodes.addOneAssumeCapacity();
                ast.nodes.set(new_node_idx, .{
                    .kind = .pi,
                    .idx_a = range.lhs,
                    .idx_b = range.rhs,
                    .next = 0,
                });
                ast.appendChildNodeToElement(current_element, new_node_idx);
            },
            .pi_end => unreachable,

            .cdata_start => @panic("TODO"),
            .cdata_end => @panic("TODO"),

            .comment_start => @panic("TODO"),
            .invalid_comment_dash_dash => @panic("TODO"),
            .invalid_comment_end_triple_dash => @panic("TODO"),
            .comment_end => @panic("TODO"),

            .dtd_start => @panic("TODO"),
            .element_decl => @panic("TODO"),
            .entity_decl => @panic("TODO"),
            .attlist_decl => @panic("TODO"),
            .notation_decl => @panic("TODO"),
        }
    }

    if (SrcReader != null) ast.str = ast.str_buf.items;
    return ast;
}

fn appendChildNodeToElement(
    ast: *Ast,
    /// Index into `ast.nodes`, where the node is `.kind = .element`.
    element_idx: usize,
    /// Index into `ast.nodes` to be appended to the linked list of children of `element_idx`.
    new_node_idx: usize,
) void {
    const slice = ast.nodes.slice();
    const kinds: []const Node.Kind = slice.items(.kind);
    const idx_bs: []usize = slice.items(.idx_b);
    const nexts: []usize = slice.items(.next);
    assert(kinds[element_idx] == .element);
    assert(new_node_idx < slice.len);

    nexts[new_node_idx] = 0;

    if (idx_bs[element_idx] == 0) {
        idx_bs[element_idx] = new_node_idx;
        return;
    }

    var current = idx_bs[element_idx];
    while (nexts[current] != 0) current = nexts[current];
    nexts[current] = new_node_idx;
}

fn nextStringRange(
    ast: *Ast,
    allocator: std.mem.Allocator,
    scanner: anytype,
) !IndexPair {
    if (@TypeOf(scanner.*) == xml.Scanner) {
        const range = switch ((scanner.nextSrc() catch unreachable).?) {
            .range => |range| range,
            .literal => |literal| literal.toRange(scanner),
        };
        assert((scanner.nextSrc() catch unreachable) == null);
        return .{
            .lhs = range.start,
            .rhs = range.end,
        };
    }
    const start = ast.str_buf.items.len;
    try scanner.nextStringStream(ast.str_buf.writer(allocator));
    const end = ast.str_buf.items.len;
    return .{ .lhs = start, .rhs = end };
}

/// Returns the range into the buffer for the character or entity reference string,
/// and the kind of reference it is (normal or invalid).
/// Must only be called immediately after `scanner.nextType()` has returned `.ampersand`.
inline fn getReferenceRangeAndKind(ast: *Ast, allocator: std.mem.Allocator, scanner: anytype) !struct { IndexPair, TextData.Kind } {
    switch (try getNextNextTokenType(scanner)) {
        .tag_token => {
            const str_range = try ast.nextStringRange(allocator, scanner);
            const ref_kind: TextData.Kind = switch (try getNextNextTokenType(scanner)) {
                .semicolon => .ref,
                .invalid_reference_end => .ref_invalid_end,
                else => unreachable,
            };
            return .{ str_range, ref_kind };
        },
        inline //
        .semicolon,
        .invalid_reference_end,
        => |tag| {
            const ref_kind: TextData.Kind = comptime switch (tag) {
                .semicolon => .ref,
                .invalid_reference_end => .ref_invalid_end,
                else => unreachable,
            };
            return .{ .{ .lhs = 0, .rhs = 0 }, ref_kind };
        },
        else => unreachable,
    }
}

fn getNextNextTokenType(scanner: anytype) !xml.Scanner.TokenType {
    if (@TypeOf(scanner.*) == xml.Scanner) {
        return scanner.nextType() catch unreachable;
    }
    return scanner.nextType();
}

test Ast {
    var fbs = std.io.fixedBufferStream(
        \\<?xml version="1.0" encoding="UTF-8"?>
        \\
        \\<foo>
        \\  Lorem ipsum
        \\  <bar fizz='buzz'><baz/></bar>
        \\  <xyz/>
        \\</foo>
        \\
    );

    var read_buffer: [8]u8 = undefined;
    var ast = try Ast.parseFromReader(fbs.reader(), .{
        .allocator = std.testing.allocator,
        .read_buffer = &read_buffer,
    });
    defer ast.deinit(std.testing.allocator);

    const root: helpers.Element = .root;
    try std.testing.expectEqual(null, root.getName(&ast));
    try std.testing.expectEqual(null, root.getAttributes(&ast));
    var root_children_iter = root.childIndexIterator(&ast);

    {
        const root_child_index = root_children_iter.next() orelse return error.ExpectedNonNull;
        const root_child = ast.nodes.get(root_child_index);
        try std.testing.expectEqual(Node.Kind.pi, root_child.kind);
        try std.testing.expectEqualStrings("xml version=\"1.0\" encoding=\"UTF-8\"", ast.str[root_child.idx_a..root_child.idx_b]);
    }

    {
        const root_child_index = root_children_iter.next() orelse return error.ExpectedNonNull;
        const root_child = ast.nodes.get(root_child_index);
        try std.testing.expectEqual(Node.Kind.text_data, root_child.kind);
        try std.testing.expectEqualStrings("\n\n", ast.str[root_child.idx_a..root_child.idx_b]);
    }

    {
        const root_child_index = root_children_iter.next() orelse return error.ExpectedNonNull;
        const root_child = ast.nodes.get(root_child_index);
        try std.testing.expectEqual(Node.Kind.element, root_child.kind);
        const foo_elem = helpers.Element.from(root_child_index);
        try std.testing.expectEqualStrings("foo", foo_elem.getName(&ast) orelse return error.ExpectedNonNull);
        try std.testing.expectEqual(null, foo_elem.getAttributes(&ast));

        var foo_children_iter = foo_elem.childIndexIterator(&ast);

        {
            const foo_child_index = foo_children_iter.next() orelse return error.ExpectedNonNull;
            const foo_child = ast.nodes.get(foo_child_index);
            try std.testing.expectEqual(Node.Kind.text_data, foo_child.kind);
            try std.testing.expectEqualStrings("\n  Lorem ipsum\n  ", ast.str[foo_child.idx_a..foo_child.idx_b]);
        }

        {
            const foo_child_index = foo_children_iter.next() orelse return error.ExpectedNonNull;
            const foo_child = ast.nodes.get(foo_child_index);
            try std.testing.expectEqual(Node.Kind.element, foo_child.kind);
            const bar_elem = helpers.Element.from(foo_child_index);
            try std.testing.expectEqualStrings("bar", bar_elem.getName(&ast) orelse return error.ExpectedNonNull);

            const bar_attrs = (bar_elem.getAttributes(&ast) orelse return error.ExpectedNonNull).toList(&ast);
            try std.testing.expectEqual(1, bar_attrs.count());
            const fizz_attr = bar_attrs.get(0);
            try std.testing.expectEqualStrings("fizz", fizz_attr.name orelse return error.ExpectedNonNull);
            try std.testing.expect(fizz_attr.eql);

            var fizz_seg_iter = (fizz_attr.value_range orelse return error.ExpectedNonNull).toValue(&ast).segmentIterator();
            const first_seg = fizz_seg_iter.next() orelse return error.ExpectedNonNull;
            try std.testing.expectEqual(null, fizz_seg_iter.next());
            try std.testing.expectEqual(helpers.Attribute.ValueSegment.str, std.meta.activeTag(first_seg));
            try std.testing.expectEqualStrings("buzz", first_seg.str);

            var bar_children_iter = bar_elem.childIndexIterator(&ast);

            {
                const bar_child_index = bar_children_iter.next() orelse return error.ExpectedNonNull;
                const bar_child = ast.nodes.get(bar_child_index);
                try std.testing.expectEqual(Node.Kind.element_empty, bar_child.kind);
                const baz_elem = helpers.Element.from(bar_child_index);
                try std.testing.expectEqualStrings("baz", baz_elem.getName(&ast) orelse return error.ExpectedNonNull);
                try std.testing.expectEqual(null, baz_elem.getAttributes(&ast));

                var baz_child_iter = baz_elem.childIndexIterator(&ast);
                try std.testing.expectEqual(null, baz_child_iter.next());
            }

            {
                const bar_child_index = bar_children_iter.next() orelse return error.ExpectedNonNull;
                const bar_child = ast.nodes.get(bar_child_index);
                try std.testing.expectEqual(Node.Kind.element_close, bar_child.kind);
                const bar_elem_close = helpers.ElementClose.from(bar_child_index);
                try std.testing.expectEqualStrings("bar", bar_elem_close.getName(&ast) orelse return error.ExpectedNonNull);
                try std.testing.expectEqual(0, bar_elem_close.getInvalidData(&ast).count());
            }

            try std.testing.expectEqual(null, bar_children_iter.next());
        }

        {
            const foo_child_index = foo_children_iter.next() orelse return error.ExpectedNonNull;
            const foo_child = ast.nodes.get(foo_child_index);
            try std.testing.expectEqual(Node.Kind.text_data, foo_child.kind);
            try std.testing.expectEqualStrings("\n  ", ast.str[foo_child.idx_a..foo_child.idx_b]);
        }

        {
            const foo_child_index = foo_children_iter.next() orelse return error.ExpectedNonNull;
            const foo_child = ast.nodes.get(foo_child_index);
            try std.testing.expectEqual(Node.Kind.element_empty, foo_child.kind);
            const xyz = helpers.Element.from(foo_child_index);
            try std.testing.expectEqualStrings("xyz", xyz.getName(&ast) orelse return error.ExpectedNonNull);
            try std.testing.expectEqual(null, foo_elem.getAttributes(&ast));
        }

        {
            const foo_child_index = foo_children_iter.next() orelse return error.ExpectedNonNull;
            const foo_child = ast.nodes.get(foo_child_index);
            try std.testing.expectEqual(Node.Kind.text_data, foo_child.kind);
            try std.testing.expectEqualStrings("\n", ast.str[foo_child.idx_a..foo_child.idx_b]);
        }

        {
            const foo_child_index = foo_children_iter.next() orelse return error.ExpectedNonNull;
            const foo_child = ast.nodes.get(foo_child_index);
            try std.testing.expectEqual(Node.Kind.element_close, foo_child.kind);
            const foo_elem_close = helpers.ElementClose.from(foo_child_index);
            try std.testing.expectEqualStrings("foo", foo_elem_close.getName(&ast) orelse return error.ExpectedNonNull);
            try std.testing.expectEqual(0, foo_elem_close.getInvalidData(&ast).count());
        }

        try std.testing.expectEqual(null, foo_children_iter.next());
    }

    {
        const root_child_index = root_children_iter.next() orelse return error.ExpectedNonNull;
        const root_child = ast.nodes.get(root_child_index);
        try std.testing.expectEqual(Node.Kind.text_data, root_child.kind);
        try std.testing.expectEqualStrings("\n", ast.str[root_child.idx_a..root_child.idx_b]);
    }

    try std.testing.expectEqual(null, root_children_iter.next());
}
