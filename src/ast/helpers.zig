const std = @import("std");
const assert = std.debug.assert;
const xml = @import("../xml.zig");
const Ast = xml.Ast;
const Node = Ast.Node;
const IndexPair = Ast.IndexPair;
const TextData = Ast.TextData;

pub const Element = enum(usize) {
    root = 0,
    _,

    pub inline fn from(element_index: usize) Element {
        return @enumFromInt(element_index);
    }

    pub fn getName(elem: Element, ast: *const Ast) ?[]const u8 {
        const element_empty_node = elem.getElementEmptyNode(ast) orelse return null;
        if (element_empty_node.idx_a == std.math.maxInt(usize)) return null;
        const range = ast.index_pairs.items[element_empty_node.idx_a];
        return ast.str[range.lhs..range.rhs];
    }

    pub fn getAttributes(elem: Element, ast: *const Ast) ?AttrRange {
        const element_empty_node = elem.getElementEmptyNode(ast) orelse return null;
        if (element_empty_node.idx_b == std.math.maxInt(usize)) return null;
        const range = ast.index_pairs.items[element_empty_node.idx_b];
        return .{ .range = range };
    }

    pub fn childIndexIterator(elem: Element, ast: *const Ast) ChildIndexIterator {
        const nodes_slice = ast.nodes.slice();
        const nodes_next: []const usize = nodes_slice.items(.next);
        return .{
            .nodes_next = nodes_next,
            .current = switch (nodes_slice.items(.kind)[@intFromEnum(elem)]) {
                .element => nodes_slice.items(.idx_b)[@intFromEnum(elem)],
                .element_empty => 0,
                else => unreachable,
            },
        };
    }

    pub const ChildIndexIterator = struct {
        nodes_next: []const usize,
        current: usize,

        pub inline fn next(iter: *ChildIndexIterator) ?usize {
            const current = iter.current;
            if (current == 0) return null;
            iter.current = iter.nodes_next[current];
            return current;
        }
    };

    inline fn getElementEmptyNode(elem: Element, ast: *const Ast) ?Node {
        const main_node = ast.nodes.get(@intFromEnum(elem));
        const element_empty_node = switch (main_node.kind) {
            .element_empty => main_node,
            .element => blk: {
                if (main_node.idx_a == 0) return null;
                break :blk ast.nodes.get(main_node.idx_a);
            },
            else => unreachable,
        };
        assert(element_empty_node.kind == .element_empty);
        return element_empty_node;
    }
};

pub const ElementClose = enum(usize) {
    _,

    pub inline fn from(element_close_idx: usize) ElementClose {
        return @enumFromInt(element_close_idx);
    }

    pub inline fn getName(elem_close: ElementClose, ast: *const Ast) ?[]const u8 {
        const nodes_slice = ast.nodes.slice();
        assert(nodes_slice.items(.kind)[@intFromEnum(elem_close)] == .element_close);
        const range_idx = nodes_slice.items(.idx_a)[@intFromEnum(elem_close)];
        if (range_idx == std.math.maxInt(usize)) return null;
        const range = ast.index_pairs.items[range_idx];
        return ast.str[range.lhs..range.rhs];
    }

    pub inline fn getInvalidData(elem_close: ElementClose, ast: *const Ast) InvalidDataRange {
        const nodes_slice = ast.nodes.slice();
        assert(nodes_slice.items(.kind)[@intFromEnum(elem_close)] == .element_close);

        const range_idx = nodes_slice.items(.idx_b)[@intFromEnum(elem_close)];
        if (range_idx == std.math.maxInt(usize)) return .{ .range = .{ .lhs = 0, .rhs = 0 } };
        const range = ast.index_pairs.items[range_idx];
        return .{ .range = range };
    }

    pub const InvalidDataRange = struct {
        range: IndexPair,

        pub inline fn count(idr: InvalidDataRange) usize {
            return idr.range.rhs - idr.range.lhs;
        }
    };

    pub const InvalidDataList = struct {
        ids_tag: []const xml.Scanner.TokenType,
        ids_range: []const IndexPair,
        range: IndexPair,

        pub inline fn count(idr: InvalidDataList) usize {
            return idr.range.rhs - idr.range.lhs;
        }

        pub fn get(idr: InvalidDataList, index: usize) Ast.InvalidData {
            const offs = idr.range.lhs + index;
            assert(offs < idr.count());
            const tag = idr.ids_tag[offs];
            const range = idr.ids_range[offs];
            return .{
                .tag = tag,
                .range = range,
            };
        }
    };
};

pub const Attribute = struct {
    name: ?[]const u8,
    eql: bool,
    value_range: ?ValueRange,

    pub const ValueSegment = union(enum) {
        str: []const u8,
        ref: []const u8,
        ref_invalid_end: []const u8,
        left_angle_bracket,
    };

    pub const ValueRange = struct {
        range: IndexPair,

        pub inline fn segmentCount(val_range: ValueRange) usize {
            return val_range.range.rhs - val_range.range.lhs;
        }

        pub inline fn toValue(val_range: ValueRange, ast: *const Ast) Value {
            const text_datas_slice = ast.text_datas.slice();
            return .{
                .str = ast.str,

                .text_datas_kind = text_datas_slice.items(.kind),
                .text_datas_range = text_datas_slice.items(.range),

                .range = val_range.range,
            };
        }
    };

    pub const Value = struct {
        str: []const u8,

        text_datas_kind: []const TextData.Kind,
        text_datas_range: []const IndexPair,

        range: IndexPair,

        pub inline fn segmentCount(val: *const Value) usize {
            return val.range.rhs - val.range.lhs;
        }

        pub inline fn getSegment(val: *const Value, index: usize) ValueSegment {
            assert(index < val.segmentCount());
            const offs = val.range.lhs + index;
            const kind = val.text_datas_kind[offs];
            const range = val.text_datas_range[offs];
            return switch (kind) {
                .str => .{ .str = val.str[range.lhs..range.rhs] },
                .ref => .{ .ref = val.str[range.lhs..range.rhs] },
                .ref_invalid_end => .{ .ref_invalid_end = val.str[range.lhs..range.rhs] },
                .invalid_comment_dash_dash => unreachable,
                .invlaid_left_angle_bracket => .left_angle_bracket,
            };
        }

        pub inline fn segmentIterator(val: *const Value) SegmentIterator {
            return .{
                .value = val.*,
                .index = 0,
            };
        }

        pub const SegmentIterator = struct {
            value: Value,
            index: usize,

            pub inline fn next(iter: *SegmentIterator) ?ValueSegment {
                if (iter.index == iter.value.segmentCount()) return null;
                defer iter.index += 1;
                return iter.value.getSegment(iter.index);
            }
        };
    };
};

pub const AttrRange = struct {
    range: IndexPair,

    pub inline fn count(att_range: AttrRange) usize {
        return att_range.range.rhs - att_range.range.lhs;
    }

    pub inline fn get(att_range: AttrRange, ast: *const Ast, index: usize) Attribute {
        return att_range.toList(ast).get(index);
    }

    pub inline fn toList(att_range: AttrRange, ast: *const Ast) AttList {
        const nodes_slice = ast.nodes.slice();
        return .{
            .str = ast.str,
            .index_pairs = ast.index_pairs.items,
            .nodes_kind = nodes_slice.items(.kind),
            .nodes_idx_a = nodes_slice.items(.idx_a),
            .nodes_idx_b = nodes_slice.items(.idx_b),
            .range = att_range.range,
        };
    }
};

pub const AttList = struct {
    str: []const u8,
    index_pairs: []const IndexPair,
    nodes_kind: []const Node.Kind,
    nodes_idx_a: []const usize,
    nodes_idx_b: []const usize,
    range: IndexPair,

    pub inline fn count(att_list: *const AttList) usize {
        return att_list.range.rhs - att_list.range.lhs;
    }

    pub fn get(att_list: AttList, index: usize) Attribute {
        const offs = att_list.range.lhs + index;
        const node = .{
            .kind = att_list.nodes_kind[offs],
            .idx_a = att_list.nodes_idx_a[offs],
            .idx_b = att_list.nodes_idx_b[offs],
        };
        const eql = switch (node.kind) {
            .attribute => true,
            .attribute_no_eql => false,
            else => unreachable,
        };
        const name_range = if (node.idx_a != std.math.maxInt(usize)) att_list.index_pairs[node.idx_a] else null;
        const val_range = if (node.idx_b != std.math.maxInt(usize)) att_list.index_pairs[node.idx_b] else null;

        const name = if (name_range) |range| att_list.str[range.lhs..range.rhs] else null;

        return .{
            .name = name,
            .eql = eql,
            .value_range = if (val_range) |range| .{ .range = range } else null,
        };
    }

    pub inline fn iterator(att_list: *const AttList) Iterator {
        return .{
            .att_list = att_list.*,
            .index = 0,
        };
    }

    pub const Iterator = struct {
        att_list: AttList,
        index: usize,

        pub inline fn next(iter: *Iterator) ?Attribute {
            if (iter.index == iter.att_list.count()) return null;
            defer iter.index += 1;
            return iter.att_list.get(iter.index);
        }
    };
};
