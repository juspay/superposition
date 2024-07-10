//!
//! Wrapper implementation around the CAC (Context Aware Config) client library.
//!

// Imports
// ---------------------------------------------------------------------------

const std = @import("std");
const cac_bindings = @cImport({
    @cInclude("libcac_client.h");
});

// Types
// ---------------------------------------------------------------------------

const ConfigError = error{CStringConversionError};
const ClientError = error{ClientNotInitialized};

const Config = struct { tenant: []const u8, update_frequency: u32, hostname: []const u8 };

const Client = struct {
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
    pub fn getClient(config: Config) ClientError!Client {
        const unsafe_client = cac_bindings.cac_get_client(config.tenant.ptr);

        if (unsafe_client) |safe_client| {
            return Client{ .client = safe_client };
        } else {
            return ClientError.ClientNotInitialized;
        }
    }

    ///
    /// [`freeClient`] frees the client instance.
    ///
    /// consider using this function to defer the cleanup of the client instance.
    ///
    ///
    /// ```example
    ///
    /// const client = try Client.getClient();
    /// defer client.freeClient();
    ///
    /// ```
    ///
    pub fn freeClient(self: *Client) void {
        cac_bindings.cac_free_client(self.client);
    }
};

// Functions
// ---------------------------------------------------------------------------

fn initCacClient(config: Config) ConfigError!void {
    const output = cac_bindings.cac_new_client(config.tenant.ptr, config.update_frequency, config.hostname.ptr);

    std.debug.print("{}", .{output});

    if (output == 1) {
        return ConfigError.CStringConversionError;
    }
}

test "test initCacClient" {
    const config = Config{ .tenant = "public", .update_frequency = 10, .hostname = "http://localhost:8081" };

    try initCacClient(config);
    var client = try Client.getClient(config);
    defer client.freeClient();
}
