allprojects {
    group = "io.juspay.superposition"
    version = System.getenv("VERSION") ?: "0.0.1-SNAPSHOT"
    repositories {
        mavenCentral()
        google()
        gradlePluginPortal()
    }
    publishing {
        repositories {
            maven {
                name = "CodeArtifact"
                url = uri(System.getenv("CODEARTIFACT_REPOSITORY_ENDPOINT") ?: "")
                credentials {
                    username = "aws"
                    password = System.getenv("CODEARTIFACT_AUTH_TOKEN")
                }
            }
        }
    }
}
