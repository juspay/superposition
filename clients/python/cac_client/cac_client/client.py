import ctypes
import os
import threading
import ast

platform = os.uname().sysname.lower()
lib_path = os.environ.get("SUPERPOSITION_LIB_PATH")
if lib_path == None:
    raise Exception("SUPERPOSITION_LIB_PATH not set on env")

file_name = (
    "libcac_client.dylib" if platform == "darwin"
    else "libcac_client.dll" if platform == "linux"
    else "libcac_client.so"
)

lib_path = os.path.join(lib_path, file_name)


from enum import Enum, auto

class MergeStrategy(Enum):
    MERGE = auto()
    REPLACE = auto()

class Config:
    def __init__(self, config_dict):
        try:
            self.contexts = config_dict['contexts']
            self.overrides = config_dict['overrides']
            self.default_configs = config_dict['default_configs']
        except Exception as e:
            raise Exception("Invalid config dictionary", e)

class CacClient:
    rust_lib = ctypes.CDLL(lib_path)

    rust_lib.cac_new_client.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_char_p, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int)]
    rust_lib.cac_new_client.restype = ctypes.c_int

    rust_lib.cac_get_client.argtypes = [ctypes.c_char_p]
    rust_lib.cac_get_client.restype = ctypes.c_char_p

    rust_lib.cac_start_polling_update.argtypes = [ctypes.c_char_p]
    rust_lib.cac_start_polling_update.restype = None

    rust_lib.cac_free_client.argtypes = [ctypes.c_char_p]
    rust_lib.cac_free_client.restype = None

    rust_lib.cac_last_error_message.restype = ctypes.c_char_p

    rust_lib.cac_get_config.argtypes = [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_char_p]
    rust_lib.cac_get_config.restype = ctypes.c_char_p

    rust_lib.cac_last_error_length.restype = ctypes.c_int

    rust_lib.cac_free_string.argtypes = [ctypes.c_char_p]
    rust_lib.cac_free_string.restype = None

    rust_lib.cac_get_last_modified.argtypes = [ctypes.c_char_p]
    rust_lib.cac_get_last_modified.restype = ctypes.c_char_p

    rust_lib.cac_get_resolved_config.argtypes = [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_char_p, ctypes.c_char_p]
    rust_lib.cac_get_resolved_config.restype = ctypes.c_char_p

    rust_lib.cac_get_default_config.argtypes = [ctypes.c_char_p, ctypes.c_char_p]
    rust_lib.cac_get_default_config.restype = ctypes.c_char_p

    def __init__(self, tenant_name: str, polling_frequency: int, cac_host_name: str, cache_max_capacity: int| None = None, cache_ttl: int| None = None, cache_tti: int| None = None):
        if not tenant_name or not cac_host_name:
            raise ValueError("tenantName cannot be null or empty")

        self.tenant = tenant_name
        self.polling_frequency = polling_frequency
        self.cac_host_name = cac_host_name
        self.cache_max_capacity = cache_max_capacity
        self.cache_ttl = cache_ttl
        self.cache_tti = cache_tti

        max_capacity_ptr = ctypes.pointer(ctypes.c_int(cache_max_capacity)) if cache_max_capacity is not None else ctypes.POINTER(ctypes.c_int)()
        ttl_ptr = ctypes.pointer(ctypes.c_int(cache_ttl)) if cache_ttl is not None else ctypes.POINTER(ctypes.c_int)()
        tti_ptr = ctypes.pointer(ctypes.c_int(cache_tti)) if cache_tti is not None else ctypes.POINTER(ctypes.c_int)()


        resp = self.rust_lib.cac_new_client(
            self.tenant.encode(), self.polling_frequency, self.cac_host_name.encode(), max_capacity_ptr, ttl_ptr, tti_ptr)
        if resp == 1:
            error_message = self.get_cac_last_error_message()
            raise Exception("Error Occured while creating new client ", error_message)

    def get_cac_last_error_message(self) -> str:
        return self.rust_lib.cac_last_error_message().decode()

    def get_cac_last_error_length(self) -> int:
        return self.rust_lib.cac_last_error_length()

    def get_cac_client(self) -> str:
        return self.rust_lib.cac_get_client(self.tenant.encode())

    def start_cac_polling_update(self):
        threading.Thread(target=self._polling_update_worker).start()

    def _polling_update_worker(self):
        self.rust_lib.cac_start_polling_update(self.tenant.encode())

    def get_cac_config(self, filter_query: str | None = None, filter_prefix: str | None = None) -> Config:
        client_ptr = self.get_cac_client()
        filter_prefix_ptr = None if filter_prefix is None else filter_prefix.encode()
        filter_query_ptr = None if filter_query is None else filter_query.encode()
        try:
            result =  self.rust_lib.cac_get_config(client_ptr, filter_query_ptr, filter_prefix_ptr).decode()
            return Config(ast.literal_eval(result))
        except:
            raise Exception(self.rust_lib.get_cac_last_error_message())
        
    def free_cac_client(self, client_ptr: str):
        self.rust_lib.cac_free_client(client_ptr.encode())

    def free_cac_string(self, string: str):
        self.rust_lib.cac_free_string(string.encode())

    def get_last_modified(self) -> str:
        try:
            return self.rust_lib.cac_get_last_modified(self.get_cac_client()).decode()
        except:
            raise Exception(self.rust_lib.get_cac_last_error_message())

    def get_resolved_config(self, query: dict, merge_strategy: MergeStrategy, filter_keys: str | None = None) -> dict:
        filter_keys_ptr = None if filter_keys is None else filter_keys.encode()
        try:
            result =  self.rust_lib.cac_get_resolved_config(
                self.get_cac_client(), str(query).encode(), filter_keys_ptr, merge_strategy.name.encode()).decode()
            return ast.literal_eval(result)
        except:
            raise Exception(self.rust_lib.get_cac_last_error_message())
        
    def get_default_config(self, filter_keys: list[str] | None = None) -> dict:
        filter_keys_ptr = None if filter_keys is None else filter_keys.encode()
        try:
            result = self.rust_lib.cac_get_default_config(self.get_cac_client(), filter_keys_ptr).decode()
            return ast.literal_eval(result)
        except:
            raise Exception(self.rust_lib.get_cac_last_error_message())
