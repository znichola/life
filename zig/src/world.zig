const std = @import("std");
const expect = std.testing.expect;

pub const World = struct {
    map: []u8,
    height: usize,
    width: usize,
    allocator: std.mem.Allocator,

    pub fn print(self: World, out: anytype) void {
        out.print("h: {}\nw: {}\n", .{ self.height, self.width }) catch {};
        for (0..self.height) |i| {
            out.print("{s}\n", .{self.map[i * self.width .. i * self.width + self.width]}) catch {};
        }
    }

    pub fn clone(self: World) !World {
        const new_map = try self.allocator.alloc(u8, self.map.len);
        std.mem.copyForwards(u8, new_map, self.map);
        return World{
            .map = new_map,
            .height = self.height,
            .width = self.width,
            .allocator = self.allocator,
        };
    }

    pub fn init(map: []const u8, height: usize, width: usize, allocator: std.mem.Allocator) !World {
        const m = try allocator.dupe(u8, map);
        return (World){ .map = m, .width = width, .height = height, .allocator = allocator };
    }

    pub fn get_at(self: World, x: usize, y: usize) !u8 {
        if (self.map.len == 0) {
            return error.EmptyWorld;
        }
        if (x < 0 or y < 0 or x > self.width or y > self.height) {
            return error.OutOfBounds;
        }
        return self.map[y * self.width + x];
    }

    pub fn get_n(self: World, x: usize, y: usize) !u8 {
        return self.get_at(x, y - 1);
    }

    pub fn get_e(self: World, x: usize, y: usize) !u8 {
        return self.get_at(x + 1, y);
    }

    pub fn get_s(self: World, x: usize, y: usize) !u8 {
        return self.get_at(x, y + 1);
    }

    pub fn get_w(self: World, x: usize, y: usize) !u8 {
        return self.get_at(x - 1, y);
    }

    pub fn get_ne(self: World, x: usize, y: usize) !u8 {
        return self.get_at(x + 1, y - 1);
    }

    pub fn get_se(self: World, x: usize, y: usize) !u8 {
        return self.get_at(x + 1, y + 1);
    }

    pub fn get_sw(self: World, x: usize, y: usize) !u8 {
        return self.get_at(x - 1, y + 1);
    }

    pub fn get_nw(self: World, x: usize, y: usize) !u8 {
        return self.get_at(x - 1, y - 1);
    }
};

test "Checking cardinal getters" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var test_world = try World.init("123456789", 3, 3, gpa.allocator());
    //var test_world = World{ .map = "123456789"[0..9], .height = 3, .width = 3 };

    try expect(test_world.get_at(1, 1) catch unreachable == '5');
    try expect(test_world.get_n(1, 1) catch unreachable == '2');
    try expect(test_world.get_e(1, 1) catch unreachable == '6');
    try expect(test_world.get_s(1, 1) catch unreachable == '8');
    try expect(test_world.get_w(1, 1) catch unreachable == '4');

    try expect(test_world.get_ne(1, 1) catch unreachable == '3');
    try expect(test_world.get_se(1, 1) catch unreachable == '9');
    try expect(test_world.get_sw(1, 1) catch unreachable == '7');
    try expect(test_world.get_nw(1, 1) catch unreachable == '1');
}

test "Test World Clone World" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var test_world = try World.init("123456789", 3, 3, gpa.allocator());
    // const test_world = World{ .map = "123456789", .height = 3, .width = 3 };
    // const foo = gpa.allocator();
    const clone_world = try test_world.clone();
    try expect(std.mem.eql(u8, test_world.map, clone_world.map));
    try expect(test_world.width == clone_world.width);
    try expect(test_world.height == clone_world.height);
}