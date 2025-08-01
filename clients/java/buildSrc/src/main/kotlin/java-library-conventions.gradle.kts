plugins {
    `java-library`
}

java {
    // Can't use this as provider is a mixed project.
    // withJavadocJar()
    withSourcesJar()
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(17))
    }
}
