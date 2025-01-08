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
    b.installArtifact(exe);

    // Run command for native build
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // 2) WASM build with special options
    const wasm = b.addExecutable(.{
        .name = "gene-wasm",
        .root_source_file = b.path("src/main.zig"),
        .optimize = optimize,
        .target = b.resolveTargetQuery(.{
            .cpu_arch = .wasm32,
            .os_tag = .freestanding,
            .abi = .none,
        }),
    });
    // Add special options for WASM build
    wasm.rdynamic = true;
    wasm.import_memory = true;
    wasm.initial_memory = 65536;
    wasm.max_memory = 65536;
    wasm.stack_size = 32768;

    // Install WASM artifact
    b.installArtifact(wasm);

    // 3) Tests
    const main_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const parser_tests = b.addTest(.{
        .root_source_file = b.path("tests/parser_tests.zig"),
        .target = target,
        .optimize = optimize,
    });

    const vm_tests = b.addTest(.{
        .root_source_file = b.path("tests/vm_tests.zig"),
        .target = target,
        .optimize = optimize,
    });

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&main_tests.step);
    test_step.dependOn(&parser_tests.step);
    test_step.dependOn(&vm_tests.step);
}
