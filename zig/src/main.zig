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

    // |err| {
    //         std.debug.print("\nError with args, {}\n Using default values\n", .{err});

    // test stuff, printing arguments
    const parsed_args = parse_args(args) catch Args{ .map = ".#.####..", .x_dim = 3, .y_dim = 3 };

    // init world with preset
    var world = try World.init(parsed_args.map, parsed_args.x_dim, parsed_args.y_dim, allocator);
    defer world.free();

    world.print(stdout);

    try stdout.print("\ngot result as <{}>\n\n", .{world.count_neighbours(0, 0)});

    // evolve the map through 10 generations, and pretty print it
    //    for (0..10) |_| {
    //        world.evolve_map(1);
    //        world.print(stdout);
    //        for (0..world.height + 1) |_| try stdout.print("\x1b[A", .{});
    //
    //        std.time.sleep(1_000_000_000);
    //        try bw.flush();
    //    }
    //    for (0..world.height + 1) |_| try stdout.print("\x1b[B", .{});

    try bw.flush();
}

const ParseArgsError = error{
    IncorrectArgs,
    InvalidMap,
};

const Args = struct {
    map: []const u8,
    x_dim: usize,
    y_dim: usize,
};

fn parse_args(args: [][]u8) ParseArgsError!Args {
    //    if (args.len != 4) {
    //        return ParseArgsError.IncorrectArgs;
    //    }

    std.debug.print("\ngot ant args? {}\n", .{args.len});
    for (args) |arg| {
        std.debug.print(">{s}\n", .{arg});
    }

    const res = Args{
        .map = args[1],
        .x_dim = std.fmt.parseInt(usize, args[2], 10) catch |err| {
            std.debug.print("\nError parsing x_dim {}\n", .{err});
            return ParseArgsError.InvalidMap;
        },
        .y_dim = std.fmt.parseInt(usize, args[3], 10) catch |err| {
            std.debug.print("\nError parsing y_dim {}\n", .{err});
            return ParseArgsError.InvalidMap;
        },
    };
    return res;
}
