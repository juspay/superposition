plugins {
    `java-library`
    `maven-publish`
    kotlin("jvm") version "1.9.10"
    id("io.freefair.lombok") version "8.6"
}

java {
    toolchain {
        languageVersion = JavaLanguageVersion.of(17)
    }
}

kotlin {
    jvmToolchain(17)
}

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

publishing {
    publications {
        create<MavenPublication>("maven") {
            groupId = "${rootProject.group}.openfeature"
            artifactId = "superposition-provider"
            from(components["java"])
        }
    }
    repositories {
        maven {
            name = "GitHubPackages"
            url = uri("https://maven.pkg.github.com/juspay/superposition")
            credentials {
                username = System.getenv("GITHUB_ACTOR")
                password = System.getenv("GITHUB_TOKEN")
            }
        }
    }
}
