const std = @import("std");

// https://github.com/zackradisic/rust-vs-zig/blob/master/zlox/build.zig
pub const BuildOptions = struct {
    tracing: bool = false,

    fn from_builder(b: *std.Build) BuildOptions {
        const tracing = b.option(bool, "tracing", "trace execution of each op code") orelse false;

        return .{ .tracing = tracing };
    }
};

pub fn build(b: *std.Build) void {
    const options = b.addOptions();
    const opts = BuildOptions.from_builder(b);
    options.addOption(@TypeOf(opts.tracing), "TRACING", opts.tracing);

    const exe = b.addExecutable(.{
        .name = "rizon",
        .root_source_file = b.path("src/main.zig"),
        .target = b.host,
    });
    exe.root_module.addOptions("config", options);

    b.installArtifact(exe);
}
