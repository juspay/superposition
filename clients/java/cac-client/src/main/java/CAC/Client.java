package CAC;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.CountDownLatch;

import jnr.ffi.Pointer;

public class Client {

    private static void callCacClient() {
        String dylib = "cac_client";
        File currentDir = new File(System.getProperty("user.dir"));
        String libraryPath = currentDir.getParentFile().getParentFile().getParentFile() + "/target/debug";
        String tenant = "dev";

        System.out.println("------------------------------------------");

        System.out.println("CAC Client");

        System.out.println("---------------------");

        CacClient wrapper = new CacClient(libraryPath, dylib);

        int newClient;
        try {
            newClient = wrapper.cacNewClient(tenant, 1, "http://localhost:8080");
            System.out.println("New client created successfully. Client ID: " + newClient);
        } catch (IOException e) {
            System.err.println(e.getMessage());
        }

        System.out.println("---------------------");

        Thread pollingThread = new Thread(() -> {
            try {
                wrapper.startPollingUpdate(tenant);
            } catch (IOException e) {
                System.err.println("Error in polling thread: " + e.getMessage());
            }
        });
        pollingThread.setDaemon(true);
        pollingThread.start();
        System.out.println("Started polling in a new thread for tenant: " + tenant);

        System.out.println("---------------------");

        Pointer clientPtr;
        try {
            clientPtr = wrapper.getCacClient(tenant);
            System.out.println("Result from getCacClient: " + clientPtr);

            System.out.println("---------------------");

            String config;
            try {
                config = wrapper.getConfig(clientPtr, null, null);
                System.out.println("Config: " + config);
            } catch (IOException e) {
                System.err.println(e.getMessage());
            }

            System.out.println("---------------------");

            String defaultConfig;
            try {
                defaultConfig = wrapper.getDefaultConfig(clientPtr, null);
                System.out.println("Default Config: " + defaultConfig);
            } catch (IOException e) {
                System.err.println(e.getMessage());
            }

            System.out.println("---------------------");

            String lastModified;
            try {
                lastModified = wrapper.getLastModified(clientPtr);
                System.out.println("Last Modified: " + lastModified);
            } catch (IOException e) {
                System.err.println(e.getMessage());
            }

            System.out.println("---------------------");

            String resolvedConfig;
            try {
                resolvedConfig = wrapper.getResolvedConfig(clientPtr, "{\"clientId\": \"zepto\"}", null, "MERGE");
                System.out.println("Resolved Config: " + resolvedConfig);
            } catch (IOException e) {
                System.err.println(e.getMessage());
            }
        } catch (IOException e) {
            System.err.println(e.getMessage());
        }
    }

    public static void main(String[] args) {
        CountDownLatch latch = new CountDownLatch(1);

        try {
            callCacClient();
            latch.await(); // This will keep the main thread alive
        } catch (InterruptedException e) {
            System.err.println("Main thread interrupted: " + e.getMessage());
        } finally {
            System.out.println("Application stopped.");
        }
    }
}
