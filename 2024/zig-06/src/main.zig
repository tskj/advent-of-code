const std = @import("std");

pub fn main() !void {
    // var timer: ?std.time.Timer = null;
    // timer = std.time.Timer.start() catch unreachable;

    const allocator = std.heap.page_allocator;
    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const file_content = try file.readToEndAlloc(allocator, 0xFFFFFFFFFFFFFFFF);
    defer allocator.free(file_content);

    var x_size: usize = 0;
    while (file_content[x_size] != '\n') : (x_size += 1) {}

    var y_size: usize = 0;
    var i: usize = 0;
    while (i < file_content.len) : (i += 1) {
        if (file_content[i] == '\n') y_size += 1;
    }

    std.debug.print("the answer\n{s}\n{d}\n{d}\n", .{ file_content, x_size, y_size });
}
