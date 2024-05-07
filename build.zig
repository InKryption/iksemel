const std = @import("std");
const Build = std.Build;

pub fn build(b: *Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const test_filter = b.option([]const u8, "test-filter", "Filter for test names to run");

    const test_step = b.step("test", "Run all tests");
    const unit_test_step = b.step("unit-test", "Run unit tests");
    const integration_test_step = b.step("integration-test", "Run integration tests");

    test_step.dependOn(unit_test_step);
    test_step.dependOn(integration_test_step);

    const iksemel_root = b.path("src/iksemel.zig");

    const iksemel_mod = b.addModule("iksemel", .{
        .root_source_file = iksemel_root,
        .target = target,
        .optimize = optimize,
    });

    const iksemel_unit_tests_exe = b.addTest(.{
        .root_source_file = iksemel_root,
        .target = target,
        .optimize = optimize,
        .filter = test_filter,
    });
    const xml_unit_tests_run = b.addRunArtifact(iksemel_unit_tests_exe);
    unit_test_step.dependOn(&xml_unit_tests_run.step);

    const integration_test_exe = b.addTest(.{
        .root_source_file = b.path("test/root.zig"),
        .target = target,
        .optimize = optimize,
        .filter = test_filter,
    });
    const integration_test_run = b.addRunArtifact(integration_test_exe);
    integration_test_step.dependOn(&integration_test_run.step);

    integration_test_exe.root_module.addImport("iksemel", iksemel_mod);
}
