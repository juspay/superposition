plugins {
    `java-library`
    `maven-publish`
}

java {
    toolchain {
        languageVersion = JavaLanguageVersion.of(17)
    }
}

dependencies {
    implementation("software.amazon.smithy.java:client-core:0.0.1")
    implementation("software.amazon.smithy:smithy-aws-traits:1.55.0")
    implementation("software.amazon.smithy.java:aws-client-restjson:0.0.1")
}

publishing {
    publications {
        create<MavenPublication>("maven") {
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
