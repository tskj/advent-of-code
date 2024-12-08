const std = @import("std");
const assert = std.debug.assert;

const Position = struct {
    x: isize,
    y: isize,

    pub fn hash(self: Position) u64 {
        return std.hash.uint32(self.x) ^ std.hash.uint32(self.y);
    }

    pub fn eql(self: Position, other: Position) bool {
        return self.x == other.x and self.y == other.y;
    }
};

const Direction = enum {
    north,
    east,
    south,
    west,
};

const CharacterPosition = struct {
    dir: Direction,
    pos: Position,

    pub fn eql(self: CharacterPosition, other: CharacterPosition) bool {
        return self.pos.eql(other.pos) and self.dir == other.dir;
    }
};

var m: []const u8 = undefined;

var w: usize = 0;
var h: usize = 0;

fn getOffset(pos: Position) usize {
    assert(pos.x >= 0);
    assert(pos.y >= 0);
    assert(pos.x < w);
    assert(pos.y < h);
    return @as(usize, @intCast(pos.y)) * (w + 1) + @as(usize, @intCast(pos.x));
}

fn getGuard() CharacterPosition {
    var y: isize = 0;
    while (y < h) : (y += 1) {
        var x: isize = 0;
        while (x < w) : (x += 1) {
            const pos = Position{ .x = x, .y = y };
            if (m[getOffset(pos)] == '^') {
                return CharacterPosition{
                    .dir = .north,
                    .pos = pos,
                };
            }
        }
    }
    @panic("guard not found");
}

fn add(pos: Position, dir: Direction) Position {
    return switch (dir) {
        .north => Position{
            .x = pos.x,
            .y = pos.y - 1,
        },
        .east => Position{
            .x = pos.x + 1,
            .y = pos.y,
        },
        .south => Position{
            .x = pos.x,
            .y = pos.y + 1,
        },
        .west => Position{
            .x = pos.x - 1,
            .y = pos.y,
        },
    };
}

fn rotate(dir: Direction) Direction {
    return switch (dir) {
        .north => .east,
        .east => .south,
        .south => .west,
        .west => .north,
    };
}

fn isBlock(pos: Position) bool {
    const char = m[getOffset(pos)];
    return char == '#';
}

fn isOffMap(p: Position) bool {
    if (p.y < 0) return true;
    if (p.x < 0) return true;
    if (p.x >= w) return true;
    if (p.y >= h) return true;
    return false;
}

fn isOnMap(p: Position) bool {
    return !isOffMap(p);
}

fn nextState(cp: CharacterPosition, obstacle: ?Position) ?CharacterPosition {
    const new_potential_position = add(cp.pos, cp.dir);
    if (isOffMap(new_potential_position)) return null;

    const literal_block = isBlock(new_potential_position);
    const blocked_by_obstacle = if (obstacle != null) new_potential_position.eql(obstacle.?) else false;
    if (blocked_by_obstacle or literal_block) {
        const new_dir = rotate(cp.dir);
        const new_pos = add(cp.pos, new_dir);
        assert(isOnMap(new_pos));
        if (isBlock(new_pos)) {
            return nextState(CharacterPosition{
                .pos = cp.pos,
                .dir = new_dir,
            }, obstacle);
        }
        return CharacterPosition{
            .pos = new_pos,
            .dir = new_dir,
        };
    }

    return CharacterPosition{
        .pos = new_potential_position,
        .dir = cp.dir,
    };
}

fn loops(alloc: std.mem.Allocator, obs: Position, cp: CharacterPosition) !bool {
    if (isOffMap(obs)) return false;

    var char_posses = std.ArrayList(CharacterPosition).init(alloc);
    defer char_posses.deinit();

    var curr_cp = cp;
    while (true) {
        const new_cp = nextState(curr_cp, obs);
        if (new_cp == null) return false;

        for (char_posses.items) |item| {
            if (item.eql(curr_cp)) {
                return true;
            }
        }

        if (new_cp.?.dir != curr_cp.dir) {
            try char_posses.append(curr_cp);
        }
        curr_cp = new_cp.?;
    }
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const file_content = try file.readToEndAlloc(allocator, 0xFFFFFFFFFFFFFFFF);
    defer allocator.free(file_content);

    m = file_content;

    while (m[w] != '\n') : (w += 1) {}

    var i: usize = 0;
    while (i < m.len) : (i += 1) {
        if (m[i] == '\n') h += 1;
    }

    std.debug.print("the file\n{s}\nwidth {d}\nheight {d}\n", .{ m, w, h });
    const initial_guard = getGuard();
    std.debug.print("\nthe intial guard: {}\n", .{initial_guard});
    std.debug.print("\nguard is a block? {}\n", .{isBlock(initial_guard.pos)});

    var path_locations = std.AutoHashMap(Position, CharacterPosition).init(allocator);
    defer path_locations.deinit();

    var cp: ?CharacterPosition = initial_guard;
    while (cp != null) : (cp = nextState(cp.?, null)) {
        if (path_locations.get(cp.?.pos) == null) {
            if (!cp.?.pos.eql(initial_guard.pos)) {
                try path_locations.put(cp.?.pos, cp.?);
            }
        }
    }

    var counter: usize = 0;

    var it = path_locations.iterator();
    while (it.next()) |entry| {
        const obstacle = entry.key_ptr;
        // const char_pos = entry.value_ptr;
        if (try loops(allocator, obstacle.*, initial_guard)) {
            counter += 1;
        }
    }

    std.debug.print("\nresult is: {d}\n", .{counter});
}
