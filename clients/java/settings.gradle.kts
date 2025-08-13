plugins {
    id("org.gradle.toolchains.foojay-resolver-convention") version "0.8.0"
}

rootProject.name = "superposition-java-clients"
include("openfeature-provider")
include("sdk")
include("bindings")
