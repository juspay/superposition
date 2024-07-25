const std = @import("std");

pub fn parse_json(comptime T: type, alloc: std.mem.Allocator, json: []const u8) !T {
    const root = try std.json.parseFromSlice(T, alloc, json, .{});

    return root.value;
}

pub fn parse_filter_keys(alloc: std.mem.Allocator, keys: []const []const u8, del: []const u8) ![]const u8 {
    if (keys.len != 0) {
        var output = std.ArrayList(u8).init(alloc);

        for (keys) |key| {
            try output.appendSlice(key);
            try output.appendSlice(del);
        }
        _ = output.orderedRemove(output.items.len - 1);
        try output.append(0);

        const keys_ptr = try output.toOwnedSlice();

        return keys_ptr;
    } else {
        return "";
    }
}
