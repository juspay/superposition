plugins {
    `publishing-conventions`
    `kotlin-conventions`
    `java-library`
}

java {
    withSourcesJar()
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(17))
    }
}

extra["displayName"] = "Superposition Foreign Function Interface"
description = "Bindings for some of superpositions core functions."

dependencies {
    implementation("org.jetbrains.kotlin:kotlin-stdlib")
    implementation("net.java.dev.jna:jna:5.13.0")
}
