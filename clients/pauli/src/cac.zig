//!
//! Wrapper implementation around the CAC (Context Aware Config) client library.
//!

// Imports
// ---------------------------------------------------------------------------

const std = @import("std");
const cac_bindings = @cImport({
    @cInclude("libcac_client.h");
});

const ClientError = @import("type.zig").ClientError;
const SetupConfig = @import("type.zig").SetupConfig;

// Types
// ---------------------------------------------------------------------------

const Client = struct {
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

    pub fn getDefaultConfigRaw(self: *const Client, filter_keys: []const u8) ClientError![]const u8 {
        const c_string = cac_bindings.cac_get_default_config(self.client, filter_keys.ptr);

        if (c_string == null) {
            return ClientError.FailedToGetConfig;
        }

        const string: [:0]const u8 = std.mem.span(c_string);

        return string;
    }

    ///
    ///
    /// [`freeFfiStringRaw`] frees the memory allocated by the CAC client.
    ///
    /// As the memory allocations are done by the CAC client, it is important to free the memory allocated by the client.
    ///
    /// Used to construct safe abstractions around the raw functions. (This can be used to copy over data to a memory space allocated by the provided allocator)
    ///
    fn freeFfiStringRaw(s_ptr: []const u8) void {
        cac_bindings.cac_free_string(s_ptr.ptr);
    }
};

// Functions
// ---------------------------------------------------------------------------

test "test init" {
    const config = SetupConfig{ .tenant = "public", .update_frequency = 10, .hostname = "http://localhost:8081" };

    const client = try Client.init(config);
    defer client.deinit();
}

test "test lastModifiedRaw" {
    const config = SetupConfig{ .tenant = "public", .update_frequency = 10, .hostname = "http://localhost:8081" };

    const client = try Client.init(config);
    defer client.deinit();

    const last_modified = try client.lastModifiedRaw();

    try std.testing.expectEqualStrings(last_modified, "1970-01-01 00:00:00 UTC");
}
