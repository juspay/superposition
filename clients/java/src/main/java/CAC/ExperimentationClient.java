package CAC;

import jnr.ffi.Pointer;


public class ExperimentationClient {
    public interface RustLib {
        int expt_new_client(String tenant, long updateFrequency, String hostName);

        void expt_free_client(long ptr);

        long expt_get_client(String tenant);

        String expt_last_error_message();

        void expt_free_string(Pointer s);

        void expt_start_polling_update(String tenant);

        Pointer expt_get_applicable_variant(long clientPtr, String context, short toss);

        Pointer expt_get_satisfied_experiments(long clientPtr, String context, String filter_prefix);

        Pointer expt_get_filtered_satisfied_experiments(long clientPtr, String context, String filter_prefix);

        Pointer expt_get_running_experiments(long clientPtr);
    }
}
