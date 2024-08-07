package CAC;

import java.io.IOException;

import jnr.ffi.LibraryLoader;
import jnr.ffi.Pointer;


public class ExperimentationClient {
    public interface RustLib {
        int expt_new_client(String tenant, long updateFrequency, String hostName);

        void expt_free_client(Pointer ptr);

        Pointer expt_get_client(String tenant);

        String expt_last_error_message();

        void expt_free_string(Pointer s);

        void expt_start_polling_update(String tenant);

        Pointer expt_get_applicable_variant(Pointer clientPtr, String context, short toss);

        Pointer expt_get_satisfied_experiments(Pointer clientPtr, String context, String filter_prefix);

        Pointer expt_get_filtered_satisfied_experiments(Pointer clientPtr, String context, String filter_prefix);

        Pointer expt_get_running_experiments(Pointer clientPtr);
    }

    public static RustLib rustLib;

    public ExperimentationClient(String libraryPath, String libraryName) {
        System.setProperty("jnr.ffi.library.path", libraryPath);
        ExperimentationClient.rustLib = LibraryLoader.create(RustLib.class).load(libraryName);
    }

    public int exptNewClient(String tenant, long updateFrequency, String hostName) throws IOException {
        int result = rustLib.expt_new_client(tenant, updateFrequency, hostName);
        if (result > 0) {
            String errorMessage = rustLib.expt_last_error_message();
            throw new IOException("Failed to create new Experimentation client: " + errorMessage);
        }
        return result;
    }

    public Pointer getExptClient(String tenant) throws IOException {
        Pointer clientPtr = rustLib.expt_get_client(tenant);
        return clientPtr;
    }

    public void startPollingUpdate(String tenant) throws IOException {
        rustLib.expt_start_polling_update(tenant);
    }

    public String getApplicableVariants (Pointer clientPtr, String context, short toss) throws IOException {
        Pointer result = rustLib.expt_get_applicable_variant(clientPtr, context, toss);
        if (result == null) {
            String errorMessage = rustLib.expt_last_error_message();
            throw new IOException("Failed to get applicable variants for the experiment: " + errorMessage);
        }
        String applicableVariants = getStringAndFree(result);
        return applicableVariants;
    }

    public String getSatisfiedVariants(Pointer clientPtr, String context, String filter_prefix) throws IOException {
        Pointer result = rustLib.expt_get_satisfied_experiments(clientPtr, context, filter_prefix);
        if (result == null) {
            String errorMessage = rustLib.expt_last_error_message();
            throw new IOException("Failed to get satisfied variants for the experiment: " + errorMessage);
        }
        String satisfiedVariants = getStringAndFree(result);
        return satisfiedVariants;
    }

    public String getFilteredSatisfiedVariants(Pointer clientPtr, String context, String filter_prefix) throws IOException {
        Pointer result = rustLib.expt_get_filtered_satisfied_experiments(clientPtr, context, filter_prefix);
        if (result == null) {
            String errorMessage = rustLib.expt_last_error_message();
            throw new IOException("Failed to get filtered satisfied variants for the experiment: " + errorMessage);
        }
        String filteredSatisfiedVariants = getStringAndFree(result);
        return filteredSatisfiedVariants;
    }

    public String getRunningExperiments(Pointer clientPtr) throws IOException {
        Pointer result = rustLib.expt_get_running_experiments(clientPtr);
        if (result == null) {
            String errorMessage = rustLib.expt_last_error_message();
            throw new IOException("Failed to get the running variants for the experiment: " + errorMessage);
        }
        String runningExperiments = getStringAndFree(result);
        return runningExperiments;
    }

    public String getLastError() throws IOException {
        String errorMessage = rustLib.expt_last_error_message();
        if (errorMessage != null) {
            return errorMessage;
        } else {
            return "No error";
        }
    }

    public void freeString(Pointer ptr) {
        rustLib.expt_free_string(ptr);
    }

    public String getStringAndFree(Pointer ptr) {
        if (ptr == null) {
            return null;
        }
        try {
            return ptr.getString(0);
        } finally {
            freeString(ptr);
        }
    }
}
