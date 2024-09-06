import ctypes
import os
import threading

platform = os.uname().sysname.lower()
lib_path = os.environ.get("SUPERPOSITION_LIB_PATH")
if lib_path == None:
    raise Exception("SUPERPOSITION_LIB_PATH not set on env")

file_name = (
    "libexperimentation_client.dylib" if platform == "darwin"
    else "libexperimentation_client.dll" if platform == "linux"
    else "libexperimentation_client.so"
)

lib_path = os.path.join(lib_path, file_name)

class ExperimentationClient:
    rust_lib = ctypes.CDLL(lib_path)

    rust_lib.expt_new_client.argtypes = [ctypes.c_char_p, ctypes.c_int, ctypes.c_char_p]
    rust_lib.expt_new_client.restype = ctypes.c_int

    rust_lib.expt_start_polling_update.argtypes = [ctypes.c_char_p]
    rust_lib.expt_start_polling_update.restype = ctypes.c_void_p

    rust_lib.expt_get_client.argtypes = [ctypes.c_char_p]
    rust_lib.expt_get_client.restype = ctypes.c_char_p

    rust_lib.expt_get_applicable_variant.argtypes = [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_int]
    rust_lib.expt_get_applicable_variant.restype = ctypes.c_char_p

    rust_lib.expt_get_satisfied_experiments.argtypes = [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_char_p]
    rust_lib.expt_get_satisfied_experiments.restype = ctypes.c_char_p

    rust_lib.expt_get_filtered_satisfied_experiments.argtypes = [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_char_p]
    rust_lib.expt_get_filtered_satisfied_experiments.restype = ctypes.c_char_p

    rust_lib.expt_get_running_experiments.argtypes = [ctypes.c_char_p]
    rust_lib.expt_get_running_experiments.restype = ctypes.c_char_p

    rust_lib.expt_free_string.argtypes = [ctypes.c_char_p]
    rust_lib.expt_free_string.restype = ctypes.c_void_p

    rust_lib.expt_last_error_message.argtypes = []
    rust_lib.expt_last_error_message.restype = ctypes.c_char_p

    rust_lib.expt_last_error_length.argtypes = []
    rust_lib.expt_last_error_length.restype = ctypes.c_int

    rust_lib.expt_free_client.argtypes = [ctypes.c_char_p]
    rust_lib.expt_free_client.restype = ctypes.c_void_p

    def __init__(self, tenant_name: str, polling_frequency: int, cac_host_name: str):
        if not tenant_name or not cac_host_name:
            raise ValueError("tenantName cannot be null or empty")
        
        self.tenant = tenant_name
        self.polling_frequency = polling_frequency
        self.cac_host_name = cac_host_name

    def get_experimentation_last_error_message(self) -> str:
        return self.rust_lib.expt_last_error_message().decode()

    def create_new_experimentation_client(self) -> int:
        resp_code = self.rust_lib.expt_new_client(self.tenant.encode(), self.polling_frequency, self.cac_host_name.encode())
        if resp_code == 1:
            error_message = self.get_experimentation_last_error_message()
            print("Some error occurred while creating new experimentation client:", error_message)
            raise Exception("Client Creation Error")
        return resp_code

    def get_experimentation_client(self) -> str:
        return self.rust_lib.expt_get_client(self.tenant.encode())

    def get_running_experiments(self) -> str:
        return self.rust_lib.expt_get_running_experiments(self.get_experimentation_client()).decode()

    def free_string(self, string: str):
        self.rust_lib.expt_free_string(string.encode())

    def start_experimentation_polling_update(self):
        threading.Thread(target=self._polling_update_worker).start()

    def _polling_update_worker(self):
        self.rust_lib.expt_start_polling_update(self.tenant.encode())

    def get_experimentation_last_error_length(self) -> int:
        return self.rust_lib.expt_last_error_length()

    def free_experimentation_client(self):
        self.rust_lib.expt_free_client(self.get_experimentation_client())

    def get_filtered_satisfied_experiments(self, context: str, filter_prefix: str | None = None) -> str:
        filter_prefix_ptr = None if filter_prefix is None else filter_prefix.encode()
        return self.rust_lib.expt_get_filtered_satisfied_experiments(
            self.get_experimentation_client(), context.encode(), filter_prefix_ptr
        ).decode()

    def get_applicable_variant(self, context: str, toss: int) -> str:
        return self.rust_lib.expt_get_applicable_variant(
            self.get_experimentation_client(), context.encode(), toss
        ).decode()

    def get_satisfied_experiments(self, context: str, filter_prefix: str | None = None) -> str:
        filter_prefix_ptr = None if filter_prefix is None else filter_prefix.encode()
        return self.rust_lib.expt_get_satisfied_experiments(
            self.get_experimentation_client(), context.encode(), filter_prefix_ptr
        ).decode()