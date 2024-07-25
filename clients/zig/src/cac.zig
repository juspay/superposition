//!
//! Wrapper implementation around the CAC (Context Aware Config) client library.
//!

// Imports
// ---------------------------------------------------------------------------

const std = @import("std");
const cac_bindings = @cImport({
    @cInclude("libcac_client.h");
});

const parse_json = @import("utils.zig").parse_json;
const parse_filter_keys = @import("utils.zig").parse_filter_keys;

const ClientError = @import("type.zig").ClientError;
pub const SetupConfig = @import("type.zig").SetupConfig;

pub const MergeStrategy = enum {
    MERGE,
    REPLACE,
};

// Types
// ---------------------------------------------------------------------------

pub const Client = struct {
    config: *const SetupConfig,
    ///
    /// [`client`] is a pointer to the CAC client instance.
    /// This is created when the cac client library is initialized. (see [`initCacClient`])
    ///
    client: *cac_bindings.Arc_Client,

    ///
    /// [`getClient`] returns a new client instance.
    ///
    /// Safety: This client as null pointer checks in place; if the client is not initialized the function returns an error.
    ///
    fn getClient(config: SetupConfig) ClientError!Client {
        const unsafe_client = cac_bindings.cac_get_client(config.tenant.ptr);

        if (unsafe_client) |safe_client| {
            return Client{ .client = safe_client, .config = &config };
        } else {
            return ClientError.ClientNotInitialized;
        }
    }

    ///
    /// [`deinit`] frees the client instance.
    ///
    /// consider using this function to defer the cleanup of the client instance.
    ///
    ///
    /// ```example
    ///
    /// const client = try Client.init(config);
    /// defer client.deinit();
    ///
    /// ```
    ///
    pub fn deinit(self: *const Client) void {
        cac_bindings.cac_free_client(self.client);
    }

    ///
    /// [`init`] initializes the CAC client.
    ///
    /// This client is initialized outside the zig scope and is accessed via the [`getClient`] function.
    /// This is done to manage a async runtime for managing cache and other artifacts optimizing the client.
    ///
    pub fn init(config: SetupConfig) ClientError!Client {
        const output = cac_bindings.cac_new_client(config.tenant.ptr, config.update_frequency, config.hostname.ptr);

        if (output == 1) {
            return ClientError.ClientInitializationError;
        }

        return try Client.getClient(config);
    }

    ///
    /// [`poll`] starts polling the CAC client for updates.
    ///
    /// This function is used to start polling the CAC client for updates.
    /// This is a blocking call, make sure to run this in a separate thread. The polling frequency is set in the [`StartupConfig`] struct using `update_frequency`.
    ///
    pub fn poll(self: *const Client) void {
        cac_bindings.cac_start_polling_update(self.config.tenant.ptr);
    }

    pub fn lastModifiedRaw(self: *const Client) ClientError![]const u8 {
        const c_string = cac_bindings.cac_get_last_modified(self.client);

        const string: [:0]const u8 = std.mem.span(c_string);

        return string;
    }

    fn getConfigRaw(self: *const Client, query: []const u8, prefix: []const u8) ClientError![]const u8 {
        const c_string = cac_bindings.cac_get_config(self.client, query.ptr, prefix.ptr);

        if (c_string == null) {
            return ClientError.FailedToGetConfig;
        }

        const string: [:0]const u8 = std.mem.span(c_string);

        return string;
    }

    fn getResolvedConfigRaw(self: *const Client, query: []const u8, filter_keys: []const u8, merge_strategy: []const u8) ClientError![]const u8 {
        const c_string = cac_bindings.cac_get_resolved_config(self.client, query.ptr, filter_keys.ptr, merge_strategy.ptr);

        if (c_string == null) {
            return ClientError.FailedToGetConfig;
        }

        const string: [:0]const u8 = std.mem.span(c_string);

        return string;
    }

    fn getDefaultConfigRaw(self: *const Client, filter_keys: []const u8) ClientError![]const u8 {
        const c_string = cac_bindings.cac_get_default_config(self.client, filter_keys.ptr);

        if (c_string == null) {
            return ClientError.FailedToGetConfig;
        }

        const string: [:0]const u8 = std.mem.span(c_string);

        return string;
    }

    // Allocation Based Functions

    pub fn getDefaultConfig(self: *const Client, comptime T: type, alloc: std.mem.Allocator, keys: []const []const u8) !T {
        const output = try self.getDefaultConfigRaw(try parse_filter_keys(alloc, keys, "|"));

        const config = try parse_json(T, alloc, output);

        freeFfiStringRaw(output);

        return config;
    }

    pub fn getResolvedConfig(self: *const Client, comptime O: type, alloc: std.mem.Allocator, query: anytype, filter_keys: []const []const u8, merge_strategy: MergeStrategy) !O {
        const query_ptr = try std.json.stringifyAlloc(alloc, query, .{});

        const output = try self.getResolvedConfigRaw(query_ptr, try parse_filter_keys(alloc, filter_keys, "|"), merge_strategy);

        const config = try parse_json(O, alloc, output);

        freeFfiStringRaw(output);

        return config;
    }

    pub fn getConfig(self: *const Client, comptime O: type, alloc: std.mem.Allocator, query: anytype, prefix: []const []const u8) !O {
        const prefix_ptr = try parse_filter_keys(alloc, prefix, ",");

        const query_ptr = try std.json.stringifyAlloc(alloc, query, .{});

        std.debug.print("Query: {s}\n", .{query_ptr});

        const output = try self.getConfigRaw(query_ptr, prefix_ptr);

        const config = try parse_json(O, alloc, output);

        freeFfiStringRaw(output);

        return config;
    }
};

///
///
/// [`freeFfiStringRaw`] frees the memory allocated by the CAC client.
///
/// As the memory allocations are done by the CAC client, it is important to free the memory allocated by the client.
///
/// Used to construct safe abstractions around the raw functions. (This can be used to copy over data to a memory space allocated by the provided allocator)
///
fn freeFfiStringRaw(_: []const u8) void {

    // BUG: The current implementation of `cac_free_string` accepts `char *` but a significant other functions defined return `const char *`. This makes it impossible to free the memory allocated by the CAC client.
    // cac_bindings.cac_free_string(s_ptr.ptr);
}

// Functions
// ---------------------------------------------------------------------------

test "test init" {
    const config = SetupConfig{ .tenant = "dev", .update_frequency = 10, .hostname = "http://localhost:8080" };

    const client = try Client.init(config);
    defer client.deinit();
}

test "test lastModifiedRaw" {
    const config = SetupConfig{ .tenant = "dev", .update_frequency = 10, .hostname = "http://localhost:8080" };

    const client = try Client.init(config);
    defer client.deinit();

    const last_modified = try client.lastModifiedRaw();

    try std.testing.expectEqualStrings(last_modified, "1970-01-01 00:00:00 UTC");
}
