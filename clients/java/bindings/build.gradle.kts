plugins {
    `publishing-conventions`
    `kotlin-conventions`
    `java-library`
    id("org.jetbrains.dokka")
}

java {
    withSourcesJar()
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(17))
    }
}

extra["displayName"] = "Superposition Core Java Bindings"
description = "Bindings for some of superpositions core functions."

dependencies {
    implementation("org.jetbrains.kotlin:kotlin-stdlib")
    implementation("net.java.dev.jna:jna:5.13.0")

    // Test dependencies
    testImplementation("junit:junit:4.13.2")
    testImplementation("com.google.code.gson:gson:2.10.1")
}

tasks.test {
    // Use environment variable if set (for CI/Make), otherwise compute relative path
    val libPath = System.getenv("SUPERPOSITION_LIB_PATH")
        ?: project.rootDir.parentFile.parentFile.parentFile.resolve("target/release").absolutePath
    systemProperty("java.library.path", libPath)
    systemProperty("jna.library.path", libPath)
    environment("LD_LIBRARY_PATH", libPath)
    environment("DYLD_LIBRARY_PATH", libPath)
}

tasks.register<Jar>("dokkaJavadocJar") {
    dependsOn(tasks.dokkaJavadoc)
    from(tasks.dokkaJavadoc.flatMap { it.outputDirectory })
    archiveClassifier.set("javadoc")
}
