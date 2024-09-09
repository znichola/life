const std = @import("std");
const expect = std.testing.expect;
const expectError = std.testing.expectError;

pub const World = struct {
    map: []u8,
    alt_map: []u8,
    height: usize,
    width: usize,
    live: u8 = '#',
    dead: u8 = '.',
    allocator: std.mem.Allocator,

    const DirectionError = error{ OutOfBounds, EmptyWorld };

    pub fn print(self: World, out: anytype) void {
        out.print("h: {}\nw: {}\n", .{ self.height, self.width }) catch {};
        for (0..self.height) |i| {
            out.print("{s}\n", .{self.map[i * self.width .. i * self.width + self.width]}) catch {};
        }
    }

    pub fn clone(self: World) !World {
        const new_map = try self.allocator.alloc(u8, self.map.len);
        const new_alt_map = try self.allocator.alloc(u8, self.alt_map.len);
        std.mem.copyForwards(u8, new_map, self.map);
        std.mem.copyForwards(u8, new_alt_map, self.alt_map);
        return World{
            .map = new_map,
            .alt_map = new_alt_map,
            .height = self.height,
            .width = self.width,
            .allocator = self.allocator,
        };
    }

    pub fn init(map: []const u8, height: usize, width: usize, allocator: std.mem.Allocator) !World {
        const m = try allocator.dupe(u8, map);
        const alt_map = try allocator.dupe(u8, map);
        return (World){ .map = m, .alt_map = alt_map, .width = width, .height = height, .allocator = allocator };
    }

    pub fn evolve_map(self: *World, steps: usize) void {
        _ = steps;
        for (0..self.height) |y| {
            const iy: i32 = @intCast(y);
            for (0..self.width) |x| {
                const ix: i32 = @intCast(x);
                const iw: i32 = @intCast(self.width);
                const pos: usize = @intCast(iy * iw + ix);
                self.alt_map[pos] = self.evolve_cell(ix, iy);
            }
        }

        std.debug.print("type of {} and {}\n", .{ @TypeOf(self.map), @TypeOf(self.alt_map) });

        var tmp = self.alt_map.ptr;
        std.debug.print("type of {}\n", .{@TypeOf(tmp)});
        //self.alt_map.ptr = self.map.ptr;
        //self.map.ptr = tmp;
        tmp = self.map.ptr;

        std.mem.swap([]u8, &self.map, &self.alt_map);
        return;
    }

    pub fn evolve_cell(self: World, x: i32, y: i32) u8 {
        const nc = count_neighbours(self, x, y);
        var live_cell = false;

        if (self.get_at(x, y)) |cell| {
            live_cell = cell == self.live;
        } else |err| {
            std.debug.print("Error fetching alive stage <{}>\n", .{err});
        }

        if (live_cell) {
            if (nc == 2 or nc == 3) {
                return self.live;
            }
        } else {
            if (nc == 3) {
                return self.live;
            }
        }
        return self.dead;
    }

    pub fn count_neighbours(self: World, x: i32, y: i32) i32 {
        var count: i32 = 0;

        // std.debug.print("\nStarting debugg ({}, {})\n", .{ x, y });
        // const DirectionFn = fn (self: World, x: usize, y: usize) !u8(DirectionError);
        const directions = [_][2]i32{
            .{ 0, -1 }, // North
            .{ 1, 0 }, // East
            .{ 0, 1 }, // South
            .{ -1, 0 }, // West
            .{ 1, -1 }, // NorthEast
            .{ 1, 1 }, // SouthEast
            .{ -1, 1 }, // SouthWest
            .{ -1, -1 }, // NothWest
        };

        for (directions) |dir| {
            const nx = x + dir[0];
            const ny = y + dir[1];
            if (self.get_at(nx, ny)) |neighbour| {
                // std.debug.print("({}, {}): <{c}>\n", .{ nx, ny, neighbour });
                if (neighbour == self.live) {
                    count += 1;
                }
            } else |err| {
                if (err != error.OutOfBounds) @panic("Unexpected error");
            }
        }

        return count;
    }

    pub fn get_at(self: World, x: i32, y: i32) !u8 {
        if (self.map.len == 0) {
            return error.EmptyWorld;
        }
        if (x < 0 or y < 0 or x >= self.width or y >= self.height) {
            return error.OutOfBounds;
        }
        const uw: i32 = @intCast(self.width);
        const pos: usize = @intCast(y * uw + x);
        return self.map[pos];
    }
};

test "Checking cardinal getters" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var test_world = try World.init("123456789", 3, 3, gpa.allocator());

    try expect(test_world.get_at(1, 1) catch unreachable == '5');
    try expect(test_world.get_at(1, 0) catch unreachable == '2');
    try expect(test_world.get_at(2, 1) catch unreachable == '6');
    try expect(test_world.get_at(1, 2) catch unreachable == '8');
    try expect(test_world.get_at(0, 1) catch unreachable == '4');

    try expect(test_world.get_at(2, 0) catch unreachable == '3');
    try expect(test_world.get_at(2, 2) catch unreachable == '9');
    try expect(test_world.get_at(0, 2) catch unreachable == '7');
    try expect(test_world.get_at(0, 0) catch unreachable == '1');

    try expectError(error.OutOfBounds, test_world.get_at(4, 4));
    try expectError(error.OutOfBounds, test_world.get_at(-1, 1));
    try expectError(error.OutOfBounds, test_world.get_at(1, -1));
}

test "Test World Clone World" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var test_world = try World.init("123456789", 3, 3, gpa.allocator());
    const clone_world = try test_world.clone();
    try expect(std.mem.eql(u8, test_world.map, clone_world.map));
    try expect(test_world.width == clone_world.width);
    try expect(test_world.height == clone_world.height);
}

// ..##
// ....
// ..#.
// ...#

test "Counting neighbours" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var test_world = try World.init("..##......#....#", 4, 4, gpa.allocator());

    try expect(test_world.count_neighbours(0, 0) == 0);
    try expect(test_world.count_neighbours(3, 0) == 1);
    try expect(test_world.count_neighbours(2, 1) == 3);
    try expect(test_world.count_neighbours(3, 3) == 1);
}

// .#.
// ###
// #..

test "Evolve cells" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var test_world = try World.init(".#.####..", 3, 3, gpa.allocator());

    try expect(test_world.evolve_cell(0, 0) == test_world.live);
    try expect(test_world.evolve_cell(2, 2) == test_world.dead);
    try expect(test_world.evolve_cell(1, 2) == test_world.dead);
    try expect(test_world.evolve_cell(1, 1) == test_world.dead);
    try expect(test_world.evolve_cell(2, 1) == test_world.live);
}

// ###
// #.#
// ##.

test "Evelove map" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var test_world = try World.init(".#.####..", 3, 3, gpa.allocator());

    // const stdout_file = std.io.getStdOut().writer();
    const stdout_file = std.io.getStdErr().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    test_world.evolve_map();
    test_world.print(stdout);
}
