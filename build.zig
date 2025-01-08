const std = @import("std");

pub fn build(b: *std.Build) void {
    // Use standardOptimizeOption for optimization modes
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});

    // 1) Create an executable named 'gene', entry at src/main.zig
    const exe = b.addExecutable(.{
        .name = "gene",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Install the executable
    b.installArtifact(exe);

    // 2) Create a 'run' step for `zig build run`
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // 3) Add test step for main.zig
    const main_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Add test steps for other test files
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

    // 4) WASM build
    const wasm_target = std.Target.Query{
        .cpu_arch = .wasm32,
        .os_tag = .freestanding,
    };
    const exe_wasm = b.addExecutable(.{
        .name = "gene-wasm",
        .root_source_file = b.path("src/main.zig"),
        .optimize = optimize,
        .target = b.resolveTargetQuery(wasm_target),
    });

    b.installArtifact(exe_wasm);
}
