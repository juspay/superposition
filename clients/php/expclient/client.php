<?php
class ExperimentationClient
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
        $libFile = $libPath . DIRECTORY_SEPARATOR . "lib" . "experimentation_client" . (PHP_OS_FAMILY === 'Windows' ? '.dll' : (PHP_OS_FAMILY === 'Darwin' ? '.dylib' : '.so'));
        if (!file_exists($libFile)) {
            throw new InvalidArgumentException("Library file does not exist:");
        }
        $this->rustFFI = FFI::cdef("
            int expt_last_error_length();
            char* expt_last_error_message();
            void expt_free_string(char* str);
            int expt_new_client(char* tenant, int update_frequency, char* hostname);
            void expt_start_polling_update(char* tenant);
            void expt_free_client(char* exp_ptr);
            char* expt_get_client(char* tenant);
            char* expt_get_applicable_variant(char* exp_ptr, char* c_context, int toss);
            char* expt_get_satisfied_experiments(char* exp_ptr, char* c_context, char* filter_prefix);
            char* expt_get_filtered_satisfied_experiments(char* exp_ptr, char* c_context, char* filter_prefix);
            char* expt_get_running_experiments(char* exp_ptr);
        ", "/Users/namit.goel/Desktop/repos/namit_superposition/superposition/target/debug/libexperimentation_client.dylib");
    }

    function get_expt_last_error_length() {
        return $this->rustFFI->expt_last_error_length();
    }

    function get_expt_last_error_message() {
        try {
            $resp = $this->rustFFI->expt_last_error_message();
            return FFI::string($resp);
        } catch (\Throwable $th) {
            return $th;
        }
    }

    function free_expt_string() {
        $this->rustFFI->expt_free_string();
    }

    function create_expt_new_client() {
        $resp = $this->rustFFI->expt_new_client($this->tenant, $this->polling_frequency, $this->cac_host_name);
        if ($resp == 1) {
            echo $this->get_expt_last_error_message();
        }
        return $resp;
    }

    function start_expt_polling_update() {
        $this->rustFFI->expt_start_polling_update($this->tenant);
    }

    function get_expt_client() {
        return $this->rustFFI->expt_get_client($this->tenant);
    }

    function free_exp_client() {
        $this->rustFFI->expt_free_client($this->get_expt_client());
    }

    function get_expt_applicable_variant($context, $toss) {
        try {
            $resp = $this->rustFFI->expt_get_applicable_variant($this->get_expt_client(), $context, $toss);
            return FFI::string($resp);
        } catch (\Throwable $th) {
            return $th;
        }
    }

    function expt_get_satisfied_experiments($context, $filter_prefix) {
        try {
            $resp = $this->rustFFI->expt_get_satisfied_experiments($this->get_expt_client(), $context, $filter_prefix);
            return FFI::string($resp);
        } catch (\Throwable $th) {
            return $th;
        }
    }

    function get_expt_filtered_satisfied_experiments($context, $filter_prefix) {
        try {
            $resp = $this->rustFFI->expt_get_filtered_satisfied_experiments($this->get_expt_client(), $context, $filter_prefix);
            return FFI::string($resp);
        } catch (\Throwable $th) {
            return $th;
        }
    }

    function get_expt_running_experiments() {
        try {
            $resp = $this->rustFFI->expt_get_running_experiments($this->get_expt_client());
            return FFI::string($resp);
        } catch (\Throwable $th) {
            return $th;
        }
    }
}
?>