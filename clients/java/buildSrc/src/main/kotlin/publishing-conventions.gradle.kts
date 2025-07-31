import org.gradle.api.publish.maven.MavenPublication
import org.gradle.kotlin.dsl.extra
import org.gradle.kotlin.dsl.provideDelegate

plugins {
    `maven-publish`
    signing
}

publishing {
    // Add license spec to all maven publications
    publications {
        afterEvaluate {
            create<MavenPublication>("maven") {
                from(components["java"])
                val displayName: String by extra
                tasks.findByName("dokkaJavadocJar")?.let {
                    artifact(it)
                }
                pom {
                    name.set(displayName)
                    description.set(project.description)
                    url.set("https://github.com/juspay/superposition")
                    licenses {
                        license {
                            name.set("Apache License 2.0")
                            url.set("http://www.apache.org/licenses/LICENSE-2.0.txt")
                        }
                    }
                    developers {
                        developer {
                            id.set("superposition")
                            name.set("superposition")
                            email.set("superposition@juspay.in")
                            organization.set("Juspay")
                            organizationUrl.set("https://juspay.io")
                            roles.add("developer")
                        }
                    }
                    scm {
                        connection.set("https://github.com/juspay/superposition.git")
                        developerConnection.set("https://github.com/juspay/superposition.git")
                        url.set("https://github.com/juspay/superposition.git")
                    }
                }
            }
        }
    }
    repositories {
        maven {
            url = uri(rootProject.layout.buildDirectory.dir("m2"))
        }
    }
}

signing {
    setRequired {
        // signing is required only if the artifacts are to be published to a maven repository
        gradle.taskGraph.allTasks.any { it is PublishToMavenRepository }
    }

    // Don't sign the artifacts if we didn't get a key and password to use.
    if (project.hasProperty("signingKey") && project.hasProperty("signingPassword")) {
        signing {
            useInMemoryPgpKeys(
                project.properties["signingKey"].toString(),
                project.properties["signingPassword"].toString())
            sign(publishing.publications["maven"])
        }
    }
}
