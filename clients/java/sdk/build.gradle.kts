
plugins {
    `java-library`
    `publishing-conventions`
}

extra["displayName"] = "Superposition SDK"
description = "Java SDK for Superposition."


java {
    withJavadocJar()
    withSourcesJar()
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(17))
    }
}

dependencies {
    implementation("software.amazon.smithy.java:client-core:0.0.1")
    implementation("software.amazon.smithy:smithy-aws-traits:1.55.0")
    implementation("software.amazon.smithy.java:aws-client-restjson:0.0.1")
}
