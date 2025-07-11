allprojects {
    group = "io.juspay.superposition"
    version = System.getenv("VERSION") ?: "0.0.1-SNAPSHOT"
    repositories {
        mavenCentral()
        google()
        gradlePluginPortal()
    }
}
