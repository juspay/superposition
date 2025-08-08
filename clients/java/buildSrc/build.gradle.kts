plugins {
    `kotlin-dsl`
}

repositories {
    gradlePluginPortal()
    mavenCentral()
    google()
}

dependencies {
    implementation("org.jetbrains.kotlin.jvm:org.jetbrains.kotlin.jvm.gradle.plugin:1.9.10")
    implementation("org.jetbrains.dokka:org.jetbrains.dokka.gradle.plugin:2.0.0")
    constraints {
        // org.owasp.dependencycheck needs at least this version of jackson. Other plugins pull in older versions..
        add("implementation", "com.fasterxml.jackson:jackson-bom:2.16.1")

        // org.owasp.dependencycheck needs these versions. Other plugins pull in older versions..
        add("implementation", "org.apache.commons:commons-lang3:3.14.0")
        add("implementation", "org.apache.commons:commons-text:1.11.0")
    }
}
