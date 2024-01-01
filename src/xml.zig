const std = @import("std");
const assert = std.debug.assert;

pub const Scanner = @import("Scanner.zig");

comptime {
    _ = Scanner;
}
