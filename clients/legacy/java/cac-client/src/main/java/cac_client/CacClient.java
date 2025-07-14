package cac_client;

import java.io.IOException;

import jnr.ffi.LibraryLoader;
import jnr.ffi.Pointer;

public class CacClient {

    public interface RustLib {
        String cac_last_error_message();

        int cac_new_client(String tenant, long updateFrequency, String hostName);
        
        int cac_new_client_with_cache_properties(String tenant, long updateFrequency, String hostName, long cacheMaxCapacity, long cacheTTL, long cacheTTI);

        void cac_free_client(Pointer ptr);

        Pointer cac_get_client(String tenant);

        Pointer cac_get_config(Pointer clientPtr, String filterQuery, String filterPrefix);

        Pointer cac_get_default_config(Pointer clientPtr, String filterKeys);

        void cac_start_polling_update(String tenant);

        Pointer cac_get_last_modified(Pointer clientPtr);

        Pointer cac_get_resolved_config(Pointer clientPtr, String filterQuery, String filterPrefix, String merge_strategy);

        void cac_free_string(Pointer s);
    }

    public static RustLib rustLib;

    public CacClient() {
        String libraryName = "cac_client";
        String libraryPath = System.getenv("SUPERPOSITION_LIB_PATH");
        System.out.println("libraryPath" + libraryPath);
        System.setProperty("jnr.ffi.library.path", libraryPath);
        CacClient.rustLib = LibraryLoader.create(RustLib.class).load(libraryName);
    }

    public int cacNewClient(String tenant, long updateFrequency, String hostName) throws CACClientException {
        
        int result = rustLib.cac_new_client(tenant, updateFrequency, hostName);
        if (result > 0) {
            String errorMessage = rustLib.cac_last_error_message();
            throw new CACClientException("Failed to create new CAC client: " + errorMessage);
        }
        return result;
    }

    public int cacNewClientWithCacheProperties(String tenant, long updateFrequency, String hostName, Long cacheMaxCapacity, Long cacheTTL, Long cacheTTI) throws CACClientException {
       
        
        int result = rustLib.cac_new_client_with_cache_properties(tenant, updateFrequency, hostName, cacheMaxCapacity, cacheTTL, cacheTTI);
        if (result > 0) {
            String errorMessage = rustLib.cac_last_error_message();
            throw new CACClientException("Failed to create new CAC client: " + errorMessage);
        }
        return result;
    }

    public Pointer getCacClient(String tenant) throws IOException {
        Pointer clientPtr = rustLib.cac_get_client(tenant);
        return clientPtr;
    }

    public String getConfig(Pointer clientPtr, String filterQuery, String filterPrefix) throws IOException {
        Pointer result = rustLib.cac_get_config(clientPtr, filterQuery, filterPrefix);
        if (result == null) {
            String errorMessage = rustLib.cac_last_error_message();
            throw new IOException("Failed to get config: " + errorMessage);
        }
        String config = getStringAndFree(result);
        return config;
    }

    public String getDefaultConfig(Pointer clientPtr, String filterKeys) throws IOException {
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

    public String getLastModified(Pointer clientPtr) throws IOException {
        Pointer result = rustLib.cac_get_last_modified(clientPtr);
        if (result == null) {
            String errorMessage = rustLib.cac_last_error_message();
            throw new IOException("Failed to get last modified: " + errorMessage);
        }
        String lastModified = getStringAndFree(result);
        return lastModified;
    }

    public String getResolvedConfig(Pointer clientPtr, String filterQuery, String filterPrefix, String mergeStrategy)
            throws IOException {
        Pointer result = rustLib.cac_get_resolved_config(clientPtr, filterQuery, filterPrefix, mergeStrategy);
        if (result == null) {
            String errorMessage = rustLib.cac_last_error_message();
            throw new IOException("Failed to get resolved config: " + errorMessage);
        }
        String resolvedConfig = getStringAndFree(result);
        return resolvedConfig;
    }

    public void cacFreeClient(Pointer clientPtr) throws IOException{
        rustLib.cac_free_client(clientPtr);
    }

    public String getLastError() throws IOException {
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
