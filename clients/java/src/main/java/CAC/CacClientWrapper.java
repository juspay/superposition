package CAC;

import java.io.IOException;

import jnr.ffi.LibraryLoader;
import jnr.ffi.Pointer;

public class CacClientWrapper {

    public interface RustLib {
        String cac_last_error_message();

        int cac_new_client(String tenant, long updateFrequency, String hostname);

        void cac_free_client(long ptr);

        long cac_get_client(String tenant);

        Pointer cac_get_config(long clientPtr, String filterQuery, String filterPrefix);

        Pointer cac_get_default_config(long clientPtr, String filterKeys);

        void cac_start_polling_update(String tenant);

        Pointer cac_get_last_modified(long clientPtr);

        Pointer cac_get_resolved_config(long clientPtr, String filterQuery, String filterPrefix, String merge_strategy);

        void cac_free_string(Pointer s);
    }

    private static RustLib rustLib;

    public CacClientWrapper(String libraryPath, String libraryName) {
        System.setProperty("jnr.ffi.library.path", libraryPath);

        // Load the Rust library
        CacClientWrapper.rustLib = LibraryLoader.create(RustLib.class).load(libraryName);
    }

    public int wrappedCacNewClient(String tenant, long updateFrequency, String hostname) throws IOException {
        int result = rustLib.cac_new_client(tenant, updateFrequency, hostname);
        if (result > 0) {
            String errorMessage = rustLib.cac_last_error_message();
            throw new IOException("Failed to create new client: " + errorMessage);
        }
        return result;
    }

    public long wrappedCacGetClient(String tenant) throws IOException {
        long clientPtr = rustLib.cac_get_client(tenant);
        if (clientPtr == 0) {
            String errorMessage = rustLib.cac_last_error_message();
            throw new IOException("Failed to get CAC client: " + errorMessage);
        }
        return clientPtr;
    }

    public String getConfig(long clientPtr, String filterQuery, String filterPrefix) throws IOException {
        Pointer result = rustLib.cac_get_config(clientPtr, filterQuery, filterPrefix);
        if (result == null) {
            String errorMessage = rustLib.cac_last_error_message();
            throw new IOException("Failed to get config: " + errorMessage);
        }
        String config = getStringAndFree(result);
        return config;
    }

    public String getDefaultConfig(long clientPtr, String filterKeys) throws IOException {
        Pointer result = rustLib.cac_get_default_config(clientPtr, filterKeys);
        if (result == null) {
            String errorMessage = rustLib.cac_last_error_message();
            throw new IOException("Failed to get default config: " + errorMessage);
        }
        String config = getStringAndFree(result);
        return config;
    }

    public void startPollingUpdate(String tenant) throws IOException {
        rustLib.cac_start_polling_update(tenant);
    }

    public String getLastModified(long clientPtr) throws IOException {
        Pointer result = rustLib.cac_get_last_modified(clientPtr);
        if (result == null) {
            String errorMessage = rustLib.cac_last_error_message();
            throw new IOException("Failed to get last modified: " + errorMessage);
        }
        String lastModified = getStringAndFree(result);
        return lastModified;
    }

    public String getResolvedConfig(long clientPtr, String filterQuery, String filterPrefix, String mergeStrategy)
            throws IOException {
        Pointer result = rustLib.cac_get_resolved_config(clientPtr, filterQuery, filterPrefix, mergeStrategy);
        if (result == null) {
            String errorMessage = rustLib.cac_last_error_message();
            throw new IOException("Failed to get resolved config: " + errorMessage);
        }
        String resolvedConfig = getStringAndFree(result);
        return resolvedConfig;
    }

    public String wrappedLastError() throws IOException {
        String errorMessage = rustLib.cac_last_error_message();
        if (errorMessage != null) {
            return errorMessage;
        } else {
            return "No error";
        }
    }

    public void freeString(Pointer ptr) {
        rustLib.cac_free_string(ptr);
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
