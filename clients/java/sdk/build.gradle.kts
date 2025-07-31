plugins {
    `java-library-conventions`
    `publishing-conventions`
}

extra["displayName"] = "Superposition SDK"
description = "Java SDK for Superposition."

dependencies {
    implementation("software.amazon.smithy.java:client-core:0.0.1")
    implementation("software.amazon.smithy:smithy-aws-traits:1.55.0")
    implementation("software.amazon.smithy.java:aws-client-restjson:0.0.1")
}
