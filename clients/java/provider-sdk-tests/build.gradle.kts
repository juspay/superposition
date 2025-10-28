plugins {
    `kotlin-conventions`
    application
}

java {
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(17))
    }
}

extra["displayName"] = "Superposition Provider SDK Tests"
description = "Integration tests for Superposition OpenFeature Provider in Kotlin."

dependencies {
    implementation(project(":sdk"))
    implementation(project(":openfeature-provider"))
    implementation("dev.openfeature:sdk:1.15.1")
    implementation("org.jetbrains.kotlin:kotlin-stdlib")
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.7.3")
    implementation("org.slf4j:slf4j-api:2.0.9")
    implementation("org.slf4j:slf4j-simple:2.0.9")
    implementation("com.google.code.gson:gson:2.10")

    // Add missing Smithy dependencies
    implementation("software.amazon.smithy.java:client-core:0.0.1")
    implementation("software.amazon.smithy:smithy-aws-traits:1.55.0")
    implementation("software.amazon.smithy.java:aws-client-restjson:0.0.1")

    testImplementation("org.junit.jupiter:junit-jupiter:5.7.1")
    testImplementation("org.jetbrains.kotlin:kotlin-test")
    testImplementation("org.jetbrains.kotlin:kotlin-test-junit5")
}

application {
    mainClass.set("io.juspay.superposition.providertests.MainKt")
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

tasks.named<JavaExec>("run") {
    standardInput = System.`in`
    // Allow the application to read from stdin during gradle run
    if (System.getProperty("os.name").lowercase().contains("windows")) {
        // Windows-specific configuration if needed
    }
}
