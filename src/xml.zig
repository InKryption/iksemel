const std = @import("std");
const assert = std.debug.assert;

pub const Scanner = @import("Scanner.zig");

const reading_scanner = @import("reading_scanner.zig");
pub const ReadingScanner = reading_scanner.ReadingScanner;
pub const readingScanner = reading_scanner.readingScanner;

comptime {
    _ = Scanner;
    _ = reading_scanner;
}
