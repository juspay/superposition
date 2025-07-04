allprojects {
    extra["maven-repo"] = rootDir.resolve("build/m2")
    group = "io.juspay.superposition"
    version = "0.0.1-dev"
    repositories {
        mavenCentral()
        google()
        gradlePluginPortal()
    }
}
