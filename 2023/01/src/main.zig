const std = @import("std");

pub fn main() !void {
    var timer: ?std.time.Timer = null;
    timer = std.time.Timer.start() catch unreachable;

    const allocator = std.heap.page_allocator;
    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const file_content = try file.readToEndAlloc(allocator, 0xFFFFFFFFFFFFFFFF);
    defer allocator.free(file_content);

    var first: i64 = 0;
    var last: i64 = 0;
    var have_found_in_this_line: bool = false;

    var acc: i64 = 0;

    var i: usize = 0;
    while (i < file_content.len) : (i += 1) {
        const char = file_content[i];
        if (48 <= char and char <= 57) {
            const digit = char - 48;
            if (!have_found_in_this_line) first = digit;
            last = digit;
            have_found_in_this_line = true;
        }
        if (char == 10) {
            acc += first * 10 + last;
            first = 0;
            last = 0;
            have_found_in_this_line = false;
        }
    }

    const end = timer.?.read();

    const time = end;

    std.debug.print("the answer is:\n{d}\nin so many m seconds: {}", .{ acc, time });
}
