//!
//! Wrapper implementation around the **Experimentation** client library.
//!

// Imports
// ---------------------------------------------------------------------------

const std = @import("std");
const expt_bindings = @cImport({
    @cInclude("libexperimentation_client.h");
});

const parse_json = @import("utils.zig").parse_json;
const parse_filter_keys = @import("utils.zig").parse_filter_keys;

const ClientError = @import("type.zig").ClientError;
pub const SetupConfig = @import("type.zig").SetupConfig;

// Types
// ---------------------------------------------------------------------------

pub const Client = struct {
    config: *const SetupConfig,

    ///
    /// [`client`] is a pointer to the underlying Experimentation client object.
    ///
    client: *expt_bindings.Arc_Client,

    ///
    /// [`init`] initializes the Expt client.
    ///
    /// This client is initialized outside the zig scope and is accessed via the [`getClient`] function.
    /// This is done to manage a async runtime for managing cache and other artifacts optimizing the client.
    ///
    pub fn init(config: SetupConfig) ClientError!Client {
        const unsafe_client = expt_bindings.expt_new_client(config.tenant.ptr, config.update_frequency, config.hostname.ptr);

        if (unsafe_client == 1) {
            return ClientError.ClientInitializationError;
        }

        return try Client.getClient(config);
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
        expt_bindings.expt_free_client(self.client);
    }

    ///
    /// [`getClient`] returns a new client instance.
    ///
    /// Safety: This client as null pointer checks in place; if the client is not initialized the function returns an error.
    ///
    fn getClient(config: SetupConfig) ClientError!Client {
        const unsafe_client = expt_bindings.expt_get_client(config.tenant.ptr);

        if (unsafe_client) |safe_client| {
            return Client{ .config = &config, .client = safe_client };
        } else {
            return ClientError.ClientNotInitialized;
        }
    }

    ///
    /// [`poll`] starts polling the Expt client for updates.
    ///
    /// This function is used to start polling the Expt client for updates.
    /// This is a blocking call, make sure to run this in a separate thread. The polling frequency is set in the [`StartupConfig`] struct using `update_frequency`.
    ///
    pub fn poll(self: *const Client) void {
        expt_bindings.expt_start_polling_update(self.config.tenant.ptr);
    }

    fn getApplicableVariantRaw(self: *const Client, context: []const u8, toss: i16) ClientError![]const u8 {
        const c_expr = expt_bindings.expt_get_applicable_variant(self.client, context.ptr, toss);

        if (c_expr == null) {
            return ClientError.FailedToGetExperimentVariant;
        }

        const expr: [:0]const u8 = std.mem.span(c_expr);

        return expr;
    }

    pub fn getApplicableVariant(self: *const Client, comptime O: type, alloc: std.mem.Allocator, context: anytype, toss: i16) !O {
        const context_bytes = try std.json.stringifyAlloc(alloc, context, .{});

        const output = try self.getApplicableVariantRaw(context_bytes, toss);

        const config = try parse_json(O, alloc, output);

        freeFfiStringRaw(output);

        return config;
    }

    fn getSatisfiedExperimentsRaw(self: *const Client, context: []const u8, filter_prefix: []const u8) ClientError![]const u8 {
        const c_expr = expt_bindings.expt_get_satisfied_experiments(self.client, context.ptr, filter_prefix.ptr);

        if (c_expr == null) {
            return ClientError.FailedToGetSatisfiedExperiments;
        }

        const expr: [:0]const u8 = std.mem.span(c_expr);

        return expr;
    }

    pub fn getSatisfiedExperiments(self: *const Client, comptime O: type, alloc: std.mem.Allocator, context: anytype, filter_prefix: []const []const u8) !O {
        const context_bytes = try std.json.stringifyAlloc(alloc, context, .{});

        const filter_prefix_bytes = parse_filter_keys(alloc, filter_prefix, ",");

        const output = try self.getSatisfiedExperimentsRaw(context_bytes, filter_prefix_bytes);

        const config = try parse_json(O, alloc, output);

        freeFfiStringRaw(output);

        return config;
    }

    fn getFilteredSatisfiedExperimentsRaw(self: *const Client, context: []const u8, filter_prefix: []const u8) ClientError![]const u8 {
        const c_expr = expt_bindings.expt_get_filtered_satisfied_experiments(self.client, context.ptr, filter_prefix.ptr);

        if (c_expr == null) {
            return ClientError.FailedToGetFilteredSatisfiedExperiments;
        }

        const expr: [:0]const u8 = std.mem.span(c_expr);

        return expr;
    }

    pub fn getFilteredSatisfiedExperiments(self: *const Client, comptime O: type, alloc: std.mem.Allocator, context: anytype, filter_prefix: []const []const u8) !O {
        const context_bytes = try std.json.stringifyAlloc(alloc, context, .{});

        const filter_prefix_bytes = parse_filter_keys(alloc, filter_prefix, ",");

        const output = try self.getFilteredSatisfiedExperimentsRaw(context_bytes, filter_prefix_bytes);

        const config = try parse_json(O, alloc, output);

        freeFfiStringRaw(output);

        return config;
    }

    fn getRunningExperimentsRaw(self: *const Client) ClientError![]const u8 {
        const c_expr = expt_bindings.expt_get_running_experiments(self.client);

        if (c_expr == null) {
            return ClientError.FailedToGetRunningExperiments;
        }

        const expr: [:0]const u8 = std.mem.span(c_expr);

        return expr;
    }

    pub fn getRunningExperiments(self: *const Client, comptime O: type, alloc: std.mem.Allocator) !O {
        const output = try self.getRunningExperimentsRaw();

        const config = try parse_json(O, alloc, output);

        freeFfiStringRaw(output);

        return config;
    }
};

///
///
/// [`freeFfiStringRaw`] frees the memory allocated by the Expt client.
///
/// As the memory allocations are done by the Expt client, it is important to free the memory allocated by the client.
///
/// Used to construct safe abstractions around the raw functions. (This can be used to copy over data to a memory space allocated by the provided allocator)
///
fn freeFfiStringRaw(s_ptr: []const u8) void {
    expt_bindings.expt_free_string(s_ptr.ptr);
}

test "test init" {
    const config = SetupConfig{ .tenant = "dev", .update_frequency = 10, .hostname = "http://localhost:8081" };

    const client = try Client.init(config);
    defer client.deinit();
}
