package example;

import java.io.IOException;
import java.util.concurrent.CountDownLatch;

import exp_client.EXPClientException;
import exp_client.ExperimentationClient;
import jnr.ffi.Pointer;

public class Demo {

    private static void callExperimentationClient() {
        String tenant = "dev";
        
        System.out.println("Experimentation Client");

        System.out.println("---------------------");

        ExperimentationClient wrapper = new ExperimentationClient();

        int newClient;
        try {
            newClient = wrapper.exptNewClient(tenant, 1, "http://localhost:8080");
            System.out.println("New Experimentation client created successfully. Client ID: " + newClient);
        } catch (EXPClientException e) {
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

        pollingThread.setDaemon(true); // Optional: Set as daemon thread
        pollingThread.start();
        System.out.println("Started polling in a new thread for tenant: " + tenant);

        System.out.println("---------------------");

        Pointer clientPtr;
        try {
            clientPtr = wrapper.getExptClient(tenant);
            System.out.println("Result from getExptClient: " + clientPtr);

            System.out.println("---------------------");

            String runningExperiments;
            try {
                runningExperiments = wrapper.getRunningExperiments(clientPtr);
                System.out.println("runningExperiments: " + runningExperiments);
            } catch (IOException e) {
                System.err.println(e.getMessage());
            }

            System.out.println("---------------------");

            String satisfiedVariants;
            try {
                satisfiedVariants = wrapper.getSatisfiedVariants(clientPtr,
                        "{\"os\": \"android\", \"client\": \"1mg\"}", null);
                System.out.println("Satisfied Variants: " + satisfiedVariants);
            } catch (IOException e) {
                System.err.println(e.getMessage());
            }

            System.out.println("---------------------");

            String filteredSatisfiedVariants;
            try {
                filteredSatisfiedVariants = wrapper.getFilteredSatisfiedVariants(clientPtr,
                        "{\"os\": \"android\", \"client\": \"1mg\"}", "hyperpay");
                System.out.println("Filtered Satisfied Variants: " + filteredSatisfiedVariants);
            } catch (IOException e) {
                System.err.println(e.getMessage());
            }

            System.out.println("---------------------");

            String applicableVariants;
            try {
                applicableVariants = wrapper.getApplicableVariants(clientPtr,
                        "{\"os\": \"android\", \"client\": \"1mg\"}", (short) 9);
                System.out.println("Applicable Variants: " + applicableVariants);
            } catch (IOException e) {
                System.err.println(e.getMessage());
            }

            System.out.println("---------------------");

        } catch (IOException e) {
            System.err.println(e.getMessage());
        }
    }

    public static void main(String[] args) {
        CountDownLatch latch = new CountDownLatch(1);

        try {
            callExperimentationClient();
            latch.await(); // This will keep the main thread alive
        } catch (InterruptedException e) {
            System.err.println("Main thread interrupted: " + e.getMessage());
        } finally {
            System.out.println("Application stopped.");
        }
    }
}
