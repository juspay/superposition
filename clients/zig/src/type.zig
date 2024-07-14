pub const ClientError = error{ ClientNotInitialized, ClientInitializationError, FailedToGetConfig, FailedToGetExperimentVariant };

pub const SetupConfig = struct { tenant: []const u8, update_frequency: u32, hostname: []const u8 };
