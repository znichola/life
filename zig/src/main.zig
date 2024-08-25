const std = @import("std");
const World = @import("world.zig").World;

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.print("All your {s} are belong to us.\n", .{"codebase"});

    var test_world = World{ .map = "....#....", .height = 3, .width = 3 };

    test_world.print(stdout);
    try bw.flush();
}

// fn step(world: World) World {}
