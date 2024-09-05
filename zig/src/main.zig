const std = @import("std");
const World = @import("world.zig").World;

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    //    try stdout.print("All your {s} are belong to us.\n", .{"codebase"});

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var test_world = try World.init("....#....", 3, 3, allocator);
    test_world.print(stdout);

    std.debug.print("got result as <{}>\n", .{test_world.count_neighbours('.', 0, 0)});

    allocator.free(test_world.map);
    try bw.flush();
}

// fn step(world: World) World {}
