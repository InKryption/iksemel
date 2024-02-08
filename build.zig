const std = @import("std");
const Build = std.Build;

pub fn build(b: *Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const test_step = b.step("test", "Run all tests");
    const unit_test_step = b.step("unit-test", "Run unit tests");
    test_step.dependOn(unit_test_step);

    const iksemel_root = Build.LazyPath.relative("src/iksemel.zig");

    const iksemel_mod = b.addModule("iksemel", .{
        .root_source_file = iksemel_root,
        .target = target,
        .optimize = optimize,
    });
    _ = iksemel_mod;

    const iksemel_unit_tests_exe = b.addTest(.{
        .root_source_file = iksemel_root,
        .target = target,
        .optimize = optimize,
    });
    const xml_unit_tests_run = b.addRunArtifact(iksemel_unit_tests_exe);
    unit_test_step.dependOn(&xml_unit_tests_run.step);
}
