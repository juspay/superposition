const std = @import("std");

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall. Here we do not
    // set a preferred release mode, allowing the user to decide how to optimize.
    const optimize = b.standardOptimizeOption(.{});

    const lib = b.addStaticLibrary(.{
        .name = "pauli",
        // In this case the main source file is merely a path, however, in more
        // complicated build scripts, this could be a generated file.
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Mention the include directory for the c_bindings header files

    linkBindings(b, lib);

    // This declares intent for the library to be installed into the standard
    // location when the user invokes the "install" step (the default step when
    // running `zig build`).
    b.installArtifact(lib);

    // Tests section

    const test_step = b.step("test", "Run unit tests");

    // Root Tests

    const lib_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    linkBindings(b, lib_unit_tests);

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);
    test_step.dependOn(&run_lib_unit_tests.step);

    // CAC Tests

    const cac_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    linkBindings(b, lib_unit_tests);

    const run_cac_unit_tests = b.addRunArtifact(cac_unit_tests);
    test_step.dependOn(&run_cac_unit_tests.step);
}

fn linkBindings(b: *std.Build, c: *std.Build.Step.Compile) void {
    c.linkLibC();
    c.addIncludePath(.{ .src_path = .{ .owner = b, .sub_path = "../../headers" } });
    c.addLibraryPath(.{ .src_path = .{ .owner = b, .sub_path = "../../target/debug" } });
}
