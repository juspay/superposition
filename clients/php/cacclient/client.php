<?php
class CACClient
{
    private $tenant = null;
    private $polling_frequency = null;
    private $cac_host_name = null;
    public $rustFFI;

    function __construct($tenant_name, $polling_frequency, $cac_host_name)
    {
        $this->tenant = $tenant_name;
        $this->polling_frequency = $polling_frequency;
        $this->cac_host_name = $cac_host_name;
        $libPath = getenv("SUPERPOSITION_LIB_PATH");
        $libFile = $libPath . DIRECTORY_SEPARATOR . "lib" . "cac_client" . (PHP_OS_FAMILY === 'Windows' ? '.dll' : (PHP_OS_FAMILY === 'Darwin' ? '.dylib' : '.so'));
        if (!file_exists($libFile)) {
            throw new InvalidArgumentException("Library file does not exist:");
        }
        $this->rustFFI = FFI::cdef("
            int cac_last_error_length();
            char* cac_last_error_message();
            void cac_free_string(char* str);
            int cac_new_client(char* tenant, int update_frequency, char* hostname);
            void cac_start_polling_update(char* tenant);
            void cac_free_client(char* tenantPtr);
            char* cac_get_client(char* tenant);
            char* cac_get_last_modified(char* tenantPtr);
            char* cac_get_config(char* tenantPtr, char* filter_query, char* filter_prefix);
            char* cac_get_resolved_config(char* tenantPtr, char* query, char* filter_keys, char* merge_strategy);
            char* cac_get_default_config(char* tenantPtr, char* filter_keys);
        ", "/Users/namit.goel/Desktop/repos/namit_superposition/superposition/target/debug/libcac_client.dylib");
    }

    function get_cac_last_error_length()
    {
        return $this->rustFFI->cac_last_error_length();
    }

    function get_cac_last_error_message()
    {
        try {
            $resp = $this->rustFFI->cac_last_error_message();
            return FFI::string($resp);
        } catch (\Throwable $th) {
            return $th;
        }
    }

    function free_cac_string($str)
    {
        $this->rustFFI->cac_free_string($str);
    }

    function create_new_cac_client()
    {
        $resp = $this->rustFFI->cac_new_client($this->tenant, $this->polling_frequency, $this->cac_host_name);
        if ($resp == 1) {
            echo $this->get_cac_last_error_message();
        }
        return $resp;
    }

    function start_cac_polling_update()
    {
        $this->rustFFI->cac_start_polling_update($this->tenant);
    }

    function get_cac_client()
    {
        return $this->rustFFI->cac_get_client($this->tenant);
    }

    function free_cac_client()
    {
        $this->rustFFI->cac_free_client($this->get_cac_client());
    }

    function get_last_cac_modified()
    {
        try {
            $resp = $this->rustFFI->cac_get_last_modified($this->get_cac_client());
            return FFI::string($resp);
        } catch (\Throwable $th) {
            return $th;
        }
    }

    function get_cac_config($filter_query, $filter_prefix)
    {
        try {
            $resp = $this->rustFFI->cac_get_config($this->get_cac_client(), $filter_query, $filter_prefix);
            return FFI::string($resp);
        } catch (\Throwable $th) {
            return $th;
        }
    }

    function get_cac_resolved_config($tenantPtr, $query, $filter_keys, $merge_strategy)
    {
        try {
            $resp = $this->rustFFI->cac_get_resolved_config($this->get_cac_client(), $query, $filter_keys, $merge_strategy);
            return FFI::string($resp);
        } catch (\Throwable $th) {
            return $th;
        }
    }

    function get_cac_default_config($filter_keys)
    {
        try {
            $resp = $this->rustFFI->cac_get_default_config($this->get_cac_client(), $filter_keys);
            return FFI::string($resp);
        } catch (\Throwable $th) {
            return $th;
        }
    }
}
?>