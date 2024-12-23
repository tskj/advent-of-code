const std = @import("std");
const assert = std.debug.assert;

fn isDigit(d: u8) bool {
    return 48 <= d and d <= 57;
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const input = try file.readToEndAlloc(allocator, 0xFFFFFFFFFFFFFFFF);
    defer allocator.free(input);

    var i: usize = 0;
    const len = while (i < input.len) : (i += 1) {
        if (!isDigit(input[i])) break i;
    } else i;

    const digits = input[0..len];

    // i = 0;
    // while (i < digits.len) : (i += 1) {
    //     std.debug.print("{c}", .{digits[i]});
    // }
    // std.debug.print("\n", .{});

    const used_free_list = try allocator.alloc(i32, len);

    i = 0;
    while (i < used_free_list.len) : (i += 1) {
        assert(i < digits.len);
        assert(isDigit(digits[i]));

        const digit = digits[i] - 48;
        assert(digit >= 0);
        assert(digit <= 9);

        used_free_list[i] = digit;
    }

    var total_space: i32 = 0;
    for (used_free_list) |item| {
        // std.debug.print("{d}", .{item});
        total_space += item;
    }

    std.debug.print("\n\ntotal needed memory is: {d}\n", .{total_space});

    const memory = try allocator.alloc(i32, @as(usize, @intCast(total_space)));

    var current_offset: usize = 0;
    var current_id: i32 = 0;
    var is_free = false;
    for (used_free_list) |n| {
        for (0..@intCast(n)) |j| {
            assert(j < n);
            memory[current_offset + j] = if (is_free) -1 else current_id;
        }
        if (is_free) current_id += 1;
        current_offset += @intCast(n);
        is_free = !is_free;
    }

    // for (memory) |b| {
    //     if (b >= 0) std.debug.print("|{d}|", .{b}) else std.debug.print(".", .{});
    // }

    var free_offset: usize = 0;
    var end_offset: usize = memory.len - 1;
    while (true) {
        // find next free
        while (free_offset < memory.len) : (free_offset += 1) {
            if (memory[free_offset] == -1) break;
        } else break;

        // find last used
        while (end_offset > 0) : (end_offset -= 1) {
            if (memory[end_offset] != -1) break;
        } else break;

        if (free_offset > end_offset) break;

        const file_id = memory[end_offset];
        var start_offset = end_offset;
        while (memory[start_offset] == file_id) : (start_offset -= 1) {}
        assert(free_offset <= start_offset);

        const block_size = end_offset - start_offset;
        assert(block_size >= 1);

        var this_free_offset = free_offset;
        while (true) {
            const l = getLengthOfFreeBlock(memory, this_free_offset);
            if (l >= block_size) break;

            // find next free
            while (this_free_offset < memory.len) : (this_free_offset += 1) {
                if (memory[this_free_offset] != -1) break;
            } else break;
            while (this_free_offset < memory.len) : (this_free_offset += 1) {
                if (memory[this_free_offset] == -1) break;
            } else break;
        }

        if (this_free_offset < memory.len and this_free_offset < end_offset) {
            const block_id = memory[end_offset];
            fill(memory, this_free_offset, block_size, block_id);
            fill(memory, start_offset + 1, block_size, -1);
        } else {
            // can't be moved, let's find next block
            const block_id = memory[end_offset];
            while (end_offset > 0) : (end_offset -= 1) {
                if (memory[end_offset] != block_id and memory[end_offset] != -1) break;
            }
        }
    }

    std.debug.print("\n\n", .{});
    for (memory) |b| {
        if (b >= 0) std.debug.print("|{d}|", .{b}) else std.debug.print(".", .{});
    }
    std.debug.print("\n\n", .{});

    var check_sum: usize = 0;
    for (memory, 0..) |b, j| {
        if (b >= 0) check_sum += @as(usize, @intCast(b)) * j;
    }

    std.debug.print("checksum is: {d}\n", .{check_sum});
}

fn fill(m: []i32, offset: usize, n: usize, x: i32) void {
    var i: usize = 0;
    while (offset + i < m.len and i < n) : (i += 1) {
        m[offset + i] = x;
    }
}

fn getLengthOfFreeBlock(m: []i32, offset: usize) usize {
    assert(m[offset] == -1);
    // find next non-free
    var next = offset;
    while (next < m.len) : (next += 1) {
        if (m[next] != -1) break;
    }

    return next - offset;
}

fn swap(m: []i32, a: usize, b: usize) void {
    const t = m[a];
    m[a] = m[b];
    m[b] = t;
}
