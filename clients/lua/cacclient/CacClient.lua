local ffi = require("ffi")
local platform = string.lower(jit.os)

local lib_path = os.getenv("SUPERPOSITION_LIB_PATH")
if not lib_path then
    error("Environment variable SUPERPOSITION_LIB_PATH is not set")
end

if platform == "osx" then
    lib_path = lib_path .. "/libcac_client.dylib"
elseif platform == "linux" then
    lib_path = lib_path .. "/libcac_client.so"
elseif platform == "windows" then
    lib_path = lib_path .. "/libcac_client.dll"
else
    error("Unsupported platform: " .. platform)
end


local lib_path = "/Users/namit.goel/Desktop/repos/namit_superposition/superposition/target/debug/libcac_client.dylib"

ffi.cdef[[
    int cac_new_client(const char* tenant_name, int polling_frequency, const char* cac_host_name);
    const char* cac_get_client(const char* tenant_name);
    void cac_start_polling_update(const char* tenant_name);
    void cac_free_client(const char* client_ptr);
    const char* cac_last_error_message();
    int cac_last_error_length();
    const char* cac_get_config(const char* client_ptr, const char* filter_query, const char* filter_prefix);
    void cac_free_string(const char* string);
    const char* cac_get_last_modified(const char* client_ptr);
    const char* cac_get_resolved_config(const char* client_ptr, const char* query, const char* filter_keys, const char* merge_strategy);
    const char* cac_get_default_config(const char* client_ptr, const char* filter_keys);
]]

local rust_lib = ffi.load(lib_path)

local CacClient = {}
CacClient.__index = CacClient

function CacClient:new(tenant_name, polling_frequency, cac_host_name)
    assert(tenant_name and #tenant_name > 0, "tenantName cannot be null or empty")
    assert(cac_host_name and #cac_host_name > 0, "cacHostName cannot be null or empty")

    local self = setmetatable({}, CacClient)
    self.tenant = tenant_name
    self.polling_frequency = polling_frequency
    self.cac_host_name = cac_host_name
    return self
end

function CacClient:get_cac_last_error_message()
    local error_message = rust_lib.cac_last_error_message()
    if error_message == nil then
        return "No Error"
    end
    return ffi.string(error_message)
end

function CacClient:get_cac_last_error_length()
    return rust_lib.cac_last_error_length()
end

function CacClient:get_cac_client()
    return ffi.string(rust_lib.cac_get_client(self.tenant))
end

function CacClient:create_new_cac_client()
    local resp = rust_lib.cac_new_client(self.tenant, self.polling_frequency, self.cac_host_name)
    if resp == 1 then
        local error_message = self:get_cac_last_error_message()
        print("Some Error Occur while creating new client ", error_message)
    end
    return resp
end

function CacClient:start_cac_polling_update()
    rust_lib.cac_start_polling_update(self.tenant)
end

function CacClient:get_cac_config(filter_query, filter_prefix)
    local client_ptr = self:get_cac_client()
    return ffi.string(rust_lib.cac_get_config(client_ptr, filter_query, filter_prefix))
end

function CacClient:free_cac_client(client_ptr)
    rust_lib.cac_free_client(client_ptr)
end

function CacClient:free_cac_string(string)
    rust_lib.cac_free_string(string)
end

function CacClient:get_last_modified()
    return ffi.string(rust_lib.cac_get_last_modified(self:get_cac_client()))
end

function CacClient:get_resolved_config(query, filter_keys, merge_strategy)
    return ffi.string(rust_lib.cac_get_resolved_config(self:get_cac_client(), query, filter_keys, merge_strategy))
end

function CacClient:get_default_config(filter_keys)
    return ffi.string(rust_lib.cac_get_default_config(self:get_cac_client(), filter_keys))
end

return CacClient