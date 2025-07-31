plugins {
    `java-library`
    `publishing-conventions`
    `kotlin-conventions`
    id("io.freefair.lombok") version "8.6"
}

java {
    withSourcesJar()
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(17))
    }
}

group = "${rootProject.group}.openfeature"
extra["displayName"] = "Superposition Openfeature Provider"
description = "Openfeature provider implementation for Superposition."

dependencies {
    implementation(project(":bindings"))
    implementation(project(":sdk"))
    implementation("software.amazon.smithy.java:client-http:0.0.1")
    implementation("software.amazon.smithy.java:client-core:0.0.1")
    implementation("dev.openfeature:sdk:1.15.1")
    implementation("com.google.code.gson:gson:2.10")
    implementation("org.slf4j:slf4j-api:2.0.9")
    implementation("org.jetbrains.kotlin:kotlin-stdlib")
    compileOnly("org.jetbrains:annotations:24.1.0")
    compileOnly("org.projectlombok:lombok")
    annotationProcessor("org.projectlombok:lombok")

    testImplementation("org.junit.jupiter:junit-jupiter:5.7.1")
    testImplementation("org.slf4j:slf4j-simple:2.0.9")
}

tasks.test {
    useJUnitPlatform()
    testLogging {
        events("passed", "skipped", "failed", "standardOut", "standardError")
        exceptionFormat = org.gradle.api.tasks.testing.logging.TestExceptionFormat.FULL
        showExceptions = true
        showCauses = true
        showStackTraces = true
        showStandardStreams = true
    }
}
