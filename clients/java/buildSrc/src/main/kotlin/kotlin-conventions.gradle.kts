plugins {
    kotlin("jvm")
    id("org.jetbrains.dokka")
}

kotlin {
    jvmToolchain(17)
}

tasks.register<Jar>("dokkaJavadocJar") {
    dependsOn(tasks.dokkaJavadoc)
    from(tasks.dokkaJavadoc.flatMap { it.outputDirectory })
    archiveClassifier.set("javadoc")
}
