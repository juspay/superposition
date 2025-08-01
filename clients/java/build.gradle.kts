plugins {
    id("io.github.gradle-nexus.publish-plugin") version "1.3.0"
}

allprojects {
    group = "io.juspay.superposition"
    version = System.getenv("VERSION") ?: "0.0.1-SNAPSHOT"
    repositories {
        mavenCentral()
        google()
        gradlePluginPortal()
    }
}

nexusPublishing {
    repositories {
        // see https://central.sonatype.org/publish/publish-portal-ossrh-staging-api/#configuration
        sonatype {
            nexusUrl.set(uri("https://ossrh-staging-api.central.sonatype.com/service/local/"))
            snapshotRepositoryUrl.set(uri("https://central.sonatype.com/repository/maven-snapshots/"))
            // These are provided via envs.
            username.set(project.properties["sonatypeUsername"].toString())
            password.set(project.properties["sonatypePassword"].toString())
        }
    }
}
