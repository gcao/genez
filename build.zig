const std = @import("std");

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});

    // 1) Native build
    const exe = b.addExecutable(.{
        .name = "gene",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Add build options
    const debug_mode = b.option(bool, "debug", "Enable debug mode") orelse false;
    const options = b.addOptions();
    options.addOption(bool, "debug_mode", debug_mode);
    exe.root_module.addOptions("build_options", options);

    b.installArtifact(exe);

    // Run command for native build
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // WASI build for running Gene files
    const wasi = b.addExecutable(.{
        .name = "gene-wasi",
        .root_source_file = b.path("src/main.zig"),
        .optimize = optimize,
        .target = b.resolveTargetQuery(.{
            .cpu_arch = .wasm32,
            .os_tag = .wasi,
            .abi = .none,
        }),
    });
    wasi.root_module.addOptions("build_options", options);
    b.installArtifact(wasi);

    // Tests
    const types_module = b.addModule("types", .{ .root_source_file = b.path("src/core/types.zig") });

    const main_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    main_tests.root_module.addOptions("build_options", options);
    main_tests.root_module.addImport("types", types_module);

    const unit_tests = b.addTest(.{
        .root_source_file = b.path("src/all_tests.zig"),
        .target = target,
        .optimize = optimize,
    });
    unit_tests.root_module.addOptions("build_options", options);
    unit_tests.root_module.addImport("types", types_module);

    // Add include path for test imports
    unit_tests.addIncludePath(b.path("src"));

    const test_step = b.step("test", "Run unit tests");

    // Create run steps that will fail if tests fail
    const run_main_tests = b.addRunArtifact(main_tests);
    const run_unit_tests = b.addRunArtifact(unit_tests);

    test_step.dependOn(&run_main_tests.step);
    test_step.dependOn(&run_unit_tests.step);

    // Clean step - temporarily disabled
    // const clean_step = b.step("clean", "Remove build artifacts");
    // clean_step.dependOn(@as(*std.Build.Step, b.addRemoveDirTree(b.path("zig-cache").getPath(b))));
    // clean_step.dependOn(@as(*std.Build.Step, b.addRemoveDirTree(b.path("zig-out").getPath(b))));
}
