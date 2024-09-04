# JAVA Client

## Prerequisites

- Java JDK
- Gradle 7.0 or later

## Building the Project

To build the project, run:
```
./gradlew build
```

## Running the Application

To run the application:
```
./gradlew run
```

## Publishing the clients locally and using it

Publish cac-client
```
cd clients/java/cac-client
./gradlew publishToMavenLocal
```

Publish exp-client
```
cd clients/java/exp-client
./gradlew publishToMavenLocal
```

### Use the clients in another project

build.gradle
```
plugins {
    id 'java'  
    id 'application'
}

repositories {
    // mavenLocal() // if using local Maven repository
    mavenCentral() // It's good to have this as a fallback
    maven {
        name = "GitHubPackages"
        url = uri("https://maven.pkg.github.com/juspay/superposition")
        credentials {
            username = System.getenv("GITHUB_USERNAME")
            password = System.getenv("GITHUB_TOKEN")
        }
    }
}

dependencies {
    implementation 'com.github.jnr:jnr-ffi:2.2.16'
    implementation 'com.github.jnr:jffi:1.3.13'
    implementation 'juspay.superposition:cac-client:0.0.1'
    implementation 'juspay.superposition:exp-client:0.0.1'
}

application {
    mainClassName = 'Client' // main class name
}
```

Client.java
```
import cac_client.CacClient;
import exp_client.ExperimentationClient;
import cac_client.CACClientException;
import exp_client.EXPClientException;
import java.util.concurrent.CountDownLatch;

public class Client {
    public static void main(String[] args) {
        CountDownLatch latch = new CountDownLatch(1);
        try {
            CacClient cac_wrapper = new CacClient();
            // Use cac-client's functions
            ExperimentationClient exp_wrapper = new ExperimentationClient();
            // Use exp-client's functions
            latch.await(); // This will keep the main thread alive
        } catch (InterruptedException e) {
            System.err.println("Main thread interrupted: " + e.getMessage());
        } finally {
            System.out.println("Application stopped.");
        }
    }
}
```

## If having issues 
Try exporting these
```
export SUPERPOSITION_LIB_PATH=".../superposition/target/debug"
export PATH=$JAVA_HOME/bin:$PATH
export JAVA_HOME=/opt/homebrew/opt/openjdk
```