plugins {
    `java-library-conventions`
    `publishing-conventions`
    kotlin("jvm") version "1.9.10"
}

extra["displayName"] = "Superposition Foreign Function Interface"
description = "Bindings for some of superpositions core functions."

dependencies {
    implementation("org.jetbrains.kotlin:kotlin-stdlib")
    implementation("net.java.dev.jna:jna:5.13.0")
}
