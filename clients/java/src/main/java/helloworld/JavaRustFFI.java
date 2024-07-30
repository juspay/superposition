package helloworld;

import java.io.File;
import java.io.IOException;

public class JavaRustFFI {

    public static void main(String[] args) {
        String dylib = "cac_client";
        File currentDir = new File(System.getProperty("user.dir"));
        String libraryPath = currentDir.getParentFile().getParentFile() + "/target/debug";
        String tenant = "dev";

        System.out.println("---------------------");

        // Create an instance of the wrapper class
        RustLibraryWrapper wrapper = new RustLibraryWrapper(libraryPath, dylib);

        int newClient;
        try {
            newClient = wrapper.wrappedCacNewClient(tenant, 10, "http://localhost:8080");
            System.out.println("New client created successfully. Client ID: " + newClient);
        } catch (IOException e) {
            System.err.println(e.getMessage());
        }

        System.out.println("---------------------");

        try {
            wrapper.startPollingUpdate(tenant);
            System.out.println("Started polling update for tenant: " + tenant);
        } catch (IOException e) {
            System.err.println(e.getMessage());
        }

        System.out.println("---------------------");

        long clientPtr;
        try {
            clientPtr = wrapper.wrappedCacGetClient(tenant);
            System.out.println("Result from wrappedCacGetClient: " + clientPtr);

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

            System.out.println("---------------------");

        } catch (IOException e) {
            System.err.println(e.getMessage());
        }
    }
}

// rm -rf .gradle
// export PATH=$JAVA_HOME/bin:$PATH  
// export JAVA_HOME=/opt/homebrew/opt/openjdk
// arch -arm64 ./gradlew run