const std = @import("std");

// https://github.com/zackradisic/rust-vs-zig/blob/master/zlox/build.zig
pub const BuildOptions = struct {
    tracing: bool = false,
    print_stack: bool = false,

    fn from_builder(b: *std.Build) BuildOptions {
        const tracing = b.option(bool, "tracing", "trace execution of each instruction") orelse false;
        const print_stack = b.option(bool, "print_stack", "prints the stack on each instruction") orelse false;

        return .{ .tracing = tracing, .print_stack = print_stack };
    }
};

pub fn build(b: *std.Build) void {
    const options = b.addOptions();
    const opts = BuildOptions.from_builder(b);
    options.addOption(@TypeOf(opts.tracing), "TRACING", opts.tracing);
    options.addOption(@TypeOf(opts.tracing), "PRINT_STACK", opts.print_stack);

    const exe = b.addExecutable(.{
        .name = "rizon",
        .root_source_file = b.path("src/main.zig"),
        .target = b.host,
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
}
