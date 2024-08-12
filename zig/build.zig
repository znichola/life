const std = @import("std");

pub fn build(b: *std.Build) void {
    const exe = b.addExecutable(.{
        .name = "life",
        .root_source_file = b.path("src/main.zig"),
        .target = b.host,
    });

    const install = b.addInstallArtifact(exe, .{ .dest_dir = .{ .override = .{ .custom = "." } } });

    b.default_step.dependOn(&install.step);

    const run_exe = b.addRunArtifact(exe);

    const run_step = b.step("run", "Run the life simulation");
    run_step.dependOn(&run_exe.step);
}
