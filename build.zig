const std = @import("std");
const Build = std.Build;

pub fn build(b: *Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const unit_test_step = b.step("unit-test", "Run unit tests");

    const xml_root = Build.LazyPath.relative("src/xml.zig");

    const xml_mod = b.addModule("iksemel", .{
        .root_source_file = xml_root,
        .target = target,
        .optimize = optimize,
    });
    _ = xml_mod;

    const xml_unit_tests_exe = b.addTest(.{
        .root_source_file = xml_root,
        .target = target,
        .optimize = optimize,
    });
    const xml_unit_tests_run = b.addRunArtifact(xml_unit_tests_exe);
    unit_test_step.dependOn(&xml_unit_tests_run.step);
}
