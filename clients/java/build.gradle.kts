allprojects {
    extra["maven-repo"] = rootDir.resolve("build/m2")
    group = "io.juspay.superposition"
    version = System.getenv("VERSION") ?: "0.0.1-SNAPSHOT"
    repositories {
        mavenCentral()
        google()
        gradlePluginPortal()
        maven {
            name = "GitHubPackages"
            url = "https://maven.pkg.github.com/octocat/hello-world"
            credentials {
                username = System.getenv("GITHUB_ACTOR")
                password = System.getenv("GITHUB_TOKEN")
            }
        }
    }
}
