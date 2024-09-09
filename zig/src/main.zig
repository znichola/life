const std = @import("std");
const World = @import("world.zig").World;

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    //    try stdout.print("All your {s} are belong to us.\n", .{"codebase"});

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var world = try World.init(".#.####..", 3, 3, allocator);
    world.print(stdout);

    try stdout.print("\ngot result as <{}>\n\n", .{world.count_neighbours(0, 0)});

    world.evolve_map(1);
    world.print(stdout);

    allocator.free(world.map);
    allocator.free(world.alt_map);
    try bw.flush();
}

// fn step(world: World) World {}
