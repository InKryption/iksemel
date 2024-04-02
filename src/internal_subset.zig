//! Utilities for scanning the DTD Internal Subset.

pub const entity = @import("internal_subset/entity.zig");

comptime {
    _ = entity;
}

pub const DeclType = enum {
    entity,
    element,
    attlist,
    notation,

    pub fn fromString(
        /// Should be the string returned from `Tokenizer.[Stream|Full].nextSrc(.dtd)`,
        /// encountered after `.angle_bracket_left_bang` and then `.tag_token` with a
        /// context of `.markup`.
        str: []const u8,
    ) ?DeclType {
        return if (str.len <= max_str_len) switch (declStrInt(str)) {
            declStrInt("ENTITY") => .entity,
            declStrInt("ELEMENT") => .element,
            declStrInt("ATTLIST") => .attlist,
            declStrInt("NOTATION") => .notation,
            else => null,
        } else null;
    }

    /// The maximum length of a `.tag_token` after `.angle_bracket_left_bang`
    /// which could represent a valid `DeclType`.
    pub const max_str_len = @max(
        "ENTITY".len,
        "ELEMENT".len,
        "ATTLIST".len,
        "NOTATION".len,
    );

    const Int = std.meta.Int(.unsigned, @bitSizeOf([max_str_len]u8));
    fn declStrInt(decl_str: []const u8) Int {
        assert(decl_str.len <= max_str_len);
        var bytes: [max_str_len]u8 = .{0} ** max_str_len;
        @memcpy(bytes[0..decl_str.len], decl_str);
        return @bitCast(bytes);
    }
};

test DeclType {
    try std.testing.expectEqual(.entity, DeclType.fromString("ENTITY"));
    try std.testing.expectEqual(.element, DeclType.fromString("ELEMENT"));
    try std.testing.expectEqual(.attlist, DeclType.fromString("ATTLIST"));
    try std.testing.expectEqual(.notation, DeclType.fromString("NOTATION"));
    try std.testing.expectEqual(null, DeclType.fromString("ABCD"));
}

const std = @import("std");
const assert = std.debug.assert;
