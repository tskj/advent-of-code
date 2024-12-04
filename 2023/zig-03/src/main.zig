const std = @import("std");

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const file_content = try file.readToEndAlloc(allocator, 0xFFFFFFFFFFFFFFFF);
    defer allocator.free(file_content);

    var line_length: u32 = 0;
    while (file_content[line_length] != '\n') : (line_length += 1) {}

    const result = find_thing(file_content, line_length + 1);

    std.debug.print("the file is:\n{d}\n", .{result});
}

fn is_symbol(buf: []const u8, line_length: u32, x: isize, y: isize) bool {
    const offset = line_length * y + x;

    if (offset >= buf.len) return false;
    if (offset < 0) return false;

    const i = @as(usize, @intCast(offset));

    if ('0' <= buf[i] and buf[i] <= '9') return false;
    if (buf[i] == '.') return false;
    if (buf[i] == '\n') return false;

    return true;
}

fn is_symbol_around(buf: []const u8, line_length: u32, x: isize, y: isize) bool {
    const north = is_symbol(buf, line_length, x, y - 1);
    const south = is_symbol(buf, line_length, x, y + 1);
    const west = is_symbol(buf, line_length, x - 1, y);
    const east = is_symbol(buf, line_length, x + 1, y);

    const northwest = is_symbol(buf, line_length, x - 1, y - 1);
    const southwest = is_symbol(buf, line_length, x - 1, y + 1);
    const northeast = is_symbol(buf, line_length, x + 1, y - 1);
    const southeast = is_symbol(buf, line_length, x + 1, y + 1);

    return north or south or west or east or northwest or southwest or northeast or southeast;
}

fn find_thing(buf: []const u8, line_length: u32) isize {
    var acc: u32 = 0;
    var which_line: u32 = 0;

    var i: u32 = 0;
    while (true) {
        var local_acc: u32 = 0;
        while (i < buf.len and !(48 <= buf[i] and buf[i] <= 57)) : (i += 1) {
            if (buf[i] == '\n') which_line += 1;
        }
        if (i >= buf.len) return acc;

        var is_adjacent: bool = false;
        while (48 <= buf[i] and buf[i] <= 57) : (i += 1) {
            local_acc *= 10;
            local_acc += buf[i] - 48;
            is_adjacent = is_adjacent or is_symbol_around(buf, line_length, i % line_length, which_line);
        }
        if (is_adjacent) {
            acc += local_acc;
        }
    }
}
