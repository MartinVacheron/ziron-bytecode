const std = @import("std");

// https://github.com/zackradisic/rust-vs-zig/blob/master/zlox/build.zig
pub const BuildOptions = struct {
    tracing: bool = false,
    print_stack: bool = false,
    print_code: bool = false,
    stress_gc: bool = false,
    log_gc: bool = false,

    fn from_builder(b: *std.Build) BuildOptions {
        const tracing = b.option(bool, "tracing", "trace execution of each instruction") orelse false;
        const print_stack = b.option(bool, "print_stack", "prints the stack on each instruction") orelse false;
        const print_code = b.option(bool, "print_code", "prints the compiled code") orelse false;
        const stress_gc = b.option(bool, "stress_gc", "triggers GC as often as possible") orelse false;
        const log_gc = b.option(bool, "log_gc", "prints GC logs") orelse false;

        return .{ .tracing = tracing, .print_stack = print_stack, .print_code = print_code, .stress_gc = stress_gc, .log_gc = log_gc };
    }
};

pub fn build(b: *std.Build) void {
    const options = b.addOptions();
    const opts = BuildOptions.from_builder(b);
    options.addOption(@TypeOf(opts.tracing), "TRACING", opts.tracing);
    options.addOption(@TypeOf(opts.print_stack), "PRINT_STACK", opts.print_stack);
    options.addOption(@TypeOf(opts.print_code), "PRINT_CODE", opts.print_code);
    options.addOption(@TypeOf(opts.stress_gc), "STRESS_GC", opts.stress_gc);
    options.addOption(@TypeOf(opts.log_gc), "LOG_GC", opts.log_gc);

    const optimization = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "rizon",
        .root_source_file = b.path("src/main.zig"),
        .target = b.host,
        .optimize = optimization,
    });
    exe.root_module.addOptions("config", options);

    b.installArtifact(exe);

    const run_exe = b.addRunArtifact(exe);
    const run_step = b.step("run", "Run the application");

    // Add args given via : zig build run -- arg1 arg2
    if (b.args) |args| {
        run_exe.addArgs(args);
    }

    run_step.dependOn(&run_exe.step);

    // For ZLS
    const exe_check = b.addExecutable(.{
        .name = "foo",
        .root_source_file = b.path("src/main.zig"),
        .target = b.host,
    });
    exe_check.root_module.addOptions("config", options);

    // These two lines you might want to copy
    // (make sure to rename 'exe_check')
    const check = b.step("check", "Check if foo compiles");
    check.dependOn(&exe_check.step);
}
