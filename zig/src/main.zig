const std = @import("std");
const World = @import("world.zig").World;

pub fn main() !void {
    // init stdout
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    // init allocator
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    // pars args into string array, uses allocator for windows compat
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    // test stuff, printing arguments
    for (args) |arg| {
        try stdout.print(">{s}\n", .{arg});
    }

    // init world with preset
    var world = try World.init(".#.####..", 3, 3, allocator);
    defer world.free();

    world.print(stdout);

    try stdout.print("\ngot result as <{}>\n\n", .{world.count_neighbours(0, 0)});

    // evolve the map through 10 generations, and pretty print it
    for (0..10) |_| {
        world.evolve_map(1);
        world.print(stdout);
        for (0..world.height + 1) |_| try stdout.print("\x1b[A", .{});

        std.time.sleep(1_000_000_000);
        try bw.flush();
    }
    for (0..world.height + 1) |_| try stdout.print("\x1b[B", .{});

    try bw.flush();
}

const ParsArgsError = error{
    IncorrectArgs,
    InvalidMap,
};

fn pars_args(args: [][]u8) ParsArgsError![]u8 {
    if (args.len != 2) {
        return ParsArgsError.IncorrectArgs;
    }

    const map = args[1];

    return map;
}
