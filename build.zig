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
    const main_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    main_tests.root_module.addOptions("build_options", options);

    const parser_tests = b.addTest(.{
        .root_source_file = b.path("src/parser_tests.zig"),
        .target = target,
        .optimize = optimize,
    });
    parser_tests.root_module.addOptions("build_options", options);

    const vm_tests = b.addTest(.{
        .root_source_file = b.path("src/vm_tests.zig"),
        .target = target,
        .optimize = optimize,
    });
    vm_tests.root_module.addOptions("build_options", options);

    const bytecode_tests = b.addTest(.{
        .root_source_file = b.path("src/bytecode_tests.zig"),
        .target = target,
        .optimize = optimize,
    });
    bytecode_tests.root_module.addOptions("build_options", options);

    const ast_to_hir_tests = b.addTest(.{
        .root_source_file = b.path("src/ast_to_hir_tests.zig"),
        .target = target,
        .optimize = optimize,
    });
    ast_to_hir_tests.root_module.addOptions("build_options", options);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&main_tests.step);
    test_step.dependOn(&parser_tests.step);
    test_step.dependOn(&bytecode_tests.step);
    test_step.dependOn(&ast_to_hir_tests.step);
    test_step.dependOn(&vm_tests.step);

    // Clean step - temporarily disabled
    // const clean_step = b.step("clean", "Remove build artifacts");
    // clean_step.dependOn(@as(*std.Build.Step, b.addRemoveDirTree(b.path("zig-cache").getPath(b))));
    // clean_step.dependOn(@as(*std.Build.Step, b.addRemoveDirTree(b.path("zig-out").getPath(b))));
}
