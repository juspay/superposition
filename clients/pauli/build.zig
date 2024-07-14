const std = @import("std");

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});

    const optimize = b.standardOptimizeOption(.{});

    const superposition = buildSuperpositionClient(b);

    const lib = b.addStaticLibrary(.{
        .name = "pauli",
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    lib.step.dependOn(&superposition.step);

    linkBindingsDebug(b, lib);

    b.installArtifact(lib);

    // Tests section

    const test_step = b.step("test", "Run unit tests");

    // Root Tests

    const lib_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    lib_unit_tests.step.dependOn(&superposition.step);

    linkBindingsDebug(b, lib_unit_tests);

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);
    test_step.dependOn(&run_lib_unit_tests.step);

    const example_1 = b.addExecutable(.{ .name = "cli", .root_source_file = b.path("src/example_cli.zig"), .target = target, .optimize = optimize });

    example_1.step.dependOn(&superposition.step);
    linkBindingsDebug(b, example_1);

    b.installArtifact(example_1);

    const module = b.addModule("pauli", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    module.linkLibrary(lib);

    module.addIncludePath(b.path("superposition/headers"));
    module.addLibraryPath(b.path("superposition/target/release"));
    //
    // module.addIncludePath(b.path("../../headers"));
    // module.addLibraryPath(b.path("../../target/debug"));

    module.linkSystemLibrary("c", .{});

    module.linkSystemLibrary("cac_client", .{});
    module.linkSystemLibrary("experimentation_client", .{});
}

fn linkBindingsDebug(b: *std.Build, c: *std.Build.Step.Compile) void {
    c.addIncludePath(b.path("superposition/headers"));
    c.addLibraryPath(b.path("superposition/target/release"));

    c.linkLibC();
    c.linkSystemLibrary("cac_client");
    c.linkSystemLibrary("experimentation_client");
}

fn buildSuperpositionClient(b: *std.Build) *std.Build.Step.Run {
    const repository = b.addSystemCommand(&.{
        "sh", "-c", std.fmt.comptimePrint(
            \\if [ ! -d superposition ]; then
            \\   git clone https://github.com/juspay/superposition.git
            \\fi
        , .{}),
    });

    const rust_build = b.addSystemCommand(&.{ "cargo", "build", "--release" });

    rust_build.setCwd(b.path("superposition"));

    rust_build.step.dependOn(&repository.step);

    return rust_build;
}
