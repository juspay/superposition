local ffi = require("ffi")
local platform = string.lower(jit.os)

local lib_path = os.getenv("SUPERPOSITION_LIB_PATH")
if not lib_path then
    error("Environment variable SUPERPOSITION_LIB_PATH is not set")
end

if platform == "osx" then
    lib_path = lib_path .. "/libexperimentation_client.dylib"
elseif platform == "linux" then
    lib_path = lib_path .. "/libexperimentation_client.so"
elseif platform == "windows" then
    lib_path = lib_path .. "/libexperimentation_client.dll"
else
    error("Unsupported platform: " .. platform)
end

ffi.cdef[[
    int expt_new_client(const char* tenant_name, int polling_frequency, const char* cac_host_name);
    void expt_start_polling_update(const char* tenant_name);
    const char* expt_get_client(const char* tenant_name);
    const char* expt_get_applicable_variant(const char* client_ptr, const char* context, int toss);
    const char* expt_get_satisfied_experiments(const char* client_ptr, const char* context, const char* filter_prefix);
    const char* expt_get_filtered_satisfied_experiments(const char* client_ptr, const char* context, const char* filter_prefix);
    const char* expt_get_running_experiments(const char* client_ptr);
    void expt_free_string(const char* string);
    const char* expt_last_error_message();
    int expt_last_error_length();
    void expt_free_client(const char* client_ptr);
]]

local rust_lib = ffi.load(lib_path)

local ExperimentationClient = {}
ExperimentationClient.__index = ExperimentationClient

function ExperimentationClient:new(tenant_name, polling_frequency, cac_host_name)
    assert(tenant_name and #tenant_name > 0, "tenantName cannot be null or empty")
    assert(cac_host_name and #cac_host_name > 0, "cacHostName cannot be null or empty")

    local self = setmetatable({}, ExperimentationClient)
    self.tenant = tenant_name
    self.polling_frequency = polling_frequency
    self.cac_host_name = cac_host_name
    return self
end

function ExperimentationClient:get_experimentation_last_error_message()
    local error_message = rust_lib.expt_last_error_message();
    if error_message == nil then
        return "No Error"
    end
    return ffi.string(rust_lib.expt_last_error_message())
end

function ExperimentationClient:create_new_experimentation_client()
    local resp_code = rust_lib.expt_new_client(self.tenant, self.polling_frequency, self.cac_host_name)
    if resp_code == 1 then
        local error_message = self:get_experimentation_last_error_message()
        print("Some error occurred while creating new experimentation client:", error_message)
        error("Client Creation Error")
    end
    return resp_code
end

function ExperimentationClient:get_experimentation_client()
    return ffi.string(rust_lib.expt_get_client(self.tenant))
end

function ExperimentationClient:get_running_experiments()
    return ffi.string(rust_lib.expt_get_running_experiments(self:get_experimentation_client()))
end

function ExperimentationClient:free_string(string)
    rust_lib.expt_free_string(string)
end

function ExperimentationClient:start_experimentation_polling_update()
    rust_lib.expt_start_polling_update(self.tenant)
end

function ExperimentationClient:get_experimentation_last_error_length()
    return rust_lib.expt_last_error_length()
end

function ExperimentationClient:free_experimentation_client()
    rust_lib.expt_free_client(self:get_experimentation_client())
end

function ExperimentationClient:get_filtered_satisfied_experiments(context, filter_prefix)
    local resp = rust_lib.expt_get_filtered_satisfied_experiments(self:get_experimentation_client(), context, filter_prefix)
    if resp == nil then
        return ""
    end
    return ffi.string(resp)
end

function ExperimentationClient:get_applicable_variant(context, toss)
    return ffi.string(rust_lib.expt_get_applicable_variant(self:get_experimentation_client(), context, toss))
end

function ExperimentationClient:get_satisfied_experiments(context, filter_prefix)
    return ffi.string(rust_lib.expt_get_satisfied_experiments(self:get_experimentation_client(), context, filter_prefix))
end

return ExperimentationClient