const std = @import("std");

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});

    // 1) Native build
    const exe = b.addExecutable(.{
        .name = "gene",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = .Debug, // Force debug mode for development
    });
    b.installArtifact(exe);

    // Run command for native build
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // 2) WASM build with special options
    const wasm = b.addExecutable(.{
        .name = "gene-wasm",
        .root_source_file = b.path("src/wasm.zig"),
        .optimize = optimize,
        .target = b.resolveTargetQuery(.{
            .cpu_arch = .wasm32,
            .os_tag = .freestanding,
            .abi = .none,
        }),
    });
    wasm.entry = .disabled; // Disable entry point for WASM
    wasm.rdynamic = true;
    wasm.import_memory = true;
    const page_size = 65536;
    const min_pages = 2; // Need at least 2 pages for stack + some heap
    wasm.initial_memory = page_size * min_pages;
    wasm.max_memory = page_size * min_pages;
    wasm.stack_size = 32768;

    // Install WASM artifact
    b.installArtifact(wasm);

    // Add a step to copy WASM file to web directory
    const copy_wasm = b.addInstallFileWithDir(wasm.getEmittedBin(), .prefix, "../public/gene.wasm");
    const copy_step = b.step("copy-wasm", "Copy WASM file to web directory");
    copy_step.dependOn(&copy_wasm.step);

    // 3) Tests
    const main_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const parser_tests = b.addTest(.{
        .root_source_file = b.path("src/parser_tests.zig"),
        .target = target,
        .optimize = optimize,
    });

    const vm_tests = b.addTest(.{
        .root_source_file = b.path("src/vm_tests.zig"),
        .target = target,
        .optimize = optimize,
    });

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&main_tests.step);
    test_step.dependOn(&parser_tests.step);
    test_step.dependOn(&vm_tests.step);

    // Clean step - temporarily disabled
    // const clean_step = b.step("clean", "Remove build artifacts");
    // clean_step.dependOn(@as(*std.Build.Step, b.addRemoveDirTree(b.path("zig-cache").getPath(b))));
    // clean_step.dependOn(@as(*std.Build.Step, b.addRemoveDirTree(b.path("zig-out").getPath(b))));
}
