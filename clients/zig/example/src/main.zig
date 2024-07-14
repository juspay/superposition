const cac_client = @import("pauli").cac;
const std = @import("std");

pub fn main() !void {
    var logging_alloc = std.heap.loggingAllocator(std.heap.page_allocator);
    const alloc = logging_alloc.allocator();

    const setup_config = cac_client.SetupConfig{ .tenant = "public", .update_frequency = 10, .hostname = "http://localhost:8080" };

    const client = try cac_client.Client.init(setup_config);

    std.log.info("Client Initialized", .{});

    const input = [_][]const u8{"test"};

    const config = struct { test_2: u8 };

    const output_1 = try client.getDefaultConfig(config, alloc, &input);
    std.log.info("Output: {}", .{output_1});
}
