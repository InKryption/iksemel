pub fn expectEqualStringOrErrOrNull(
    expected: anyerror!?[]const u8,
    actual: anyerror!?[]const u8,
) !void {
    const ValKind = enum(u2) { err, null, str };
    const expected_kind: ValKind = if (expected) |maybe_str| if (maybe_str != null) .str else .null else |_| .err;
    const actual_kind: ValKind = if (actual) |maybe_str| if (maybe_str != null) .str else .null else |_| .err;

    const combo = packed struct {
        expected: ValKind,
        actual: ValKind,
        inline fn combo(exp: ValKind, act: ValKind) u4 {
            const bits: @This() = .{ .expected = exp, .actual = act };
            return @bitCast(bits);
        }
    }.combo;

    switch (combo(expected_kind, actual_kind)) {
        combo(.str, .str) => try std.testing.expectEqualStrings((expected catch unreachable).?, (actual catch unreachable).?),
        combo(.null, .null) => {},
        combo(.err, .err) => try std.testing.expectEqual(expected, actual),

        combo(.str, .null),
        combo(.str, .err),
        => {
            const expected_str = (expected catch unreachable).?;
            const actual_non_str: ?anyerror = if (actual) |must_be_null| if (must_be_null) |_| unreachable else null else |err| err;
            std.log.err("Expected '{[expected]}', got {[actual]?}", .{
                .expected = std.zig.fmtEscapes(expected_str),
                .actual = actual_non_str,
            });
            return error.TestExpectedEqual;
        },

        combo(.null, .str),
        combo(.err, .str),
        => {
            const expected_non_str: ?anyerror = if (expected) |must_be_null| if (must_be_null) |_| unreachable else null else |err| err;
            const actual_str = (actual catch unreachable).?;
            std.log.err("Expected {[expected]?}, got '{[actual]}'", .{
                .expected = expected_non_str,
                .actual = std.zig.fmtEscapes(actual_str),
            });
            return error.TestExpectedEqual;
        },

        combo(.null, .err),
        combo(.err, .null),
        => {
            const expected_non_str: ?anyerror = if (expected) |must_be_null| if (must_be_null) |_| unreachable else null else |err| err;
            const actual_non_str: ?anyerror = if (actual) |must_be_null| if (must_be_null) |_| unreachable else null else |err| err;
            std.log.err("Expected {[expected]?}, got {[actual]?}", .{
                .expected = expected_non_str,
                .actual = actual_non_str,
            });
            return error.TestExpectedEqual;
        },

        else => unreachable,
    }
}

const std = @import("std");
