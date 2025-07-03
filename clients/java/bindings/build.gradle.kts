import java.net.URL

plugins {
    `maven-publish`
    `java-library`
    kotlin("jvm") version "1.9.10"
}

java {
    toolchain {
        languageVersion = JavaLanguageVersion.of(17)
    }
}

dependencies {
    implementation("org.jetbrains.kotlin:kotlin-stdlib")
    implementation("net.java.dev.jna:jna:5.13.0")
}


publishing {
    publications {
        create<MavenPublication>("maven") {
            from(components["kotlin"])
        }
    }
    repositories {
        maven {
            url = uri(extra["maven-repo"]!!)
        }
    }
}

// Download core-lib as a resource.
tasks.register("downloadCoreLib") {
    group = "build"
    description = "Downloads native libraries for all platforms."
    val urlTemplate = "https://github.com/juspay/superposition/releases/download/v0.76.0/superposition_core-%s.zip"
    val libraryName = "libsuperposition_core"
    val platforms = mapOf(
        // JNA platform to target-triple
        "darwin-x86-64" to "x86_64-apple-darwin",
        "darwin-aarch64" to "aarch64-apple-darwin",
        "linux-x86-64" to "x86_64-unknown-linux-gnu",
    )
    val resourcesDir = file("src/main/resources")
    outputs.files(platforms.keys.map { platform ->
        val extension = when {
            platform.startsWith("darwin") -> "dylib"
            platform.startsWith("win") -> "dll"
            else -> "so"
        }
        file("$resourcesDir/$platform/$libraryName.$extension")
    })

    doLast {
        platforms.forEach { (platform, ttriple) ->
            val url = URL(urlTemplate.format(ttriple))
            val targetDir = file("$resourcesDir/$platform")
            targetDir.mkdirs()
            val targetFile = file("$targetDir/$libraryName.${if (platform.startsWith("darwin")) "dylib" else "so"}")
            if (!targetFile.exists()) {
                println("Downloading ${url.file} to $targetFile")
                val zip = File(temporaryDir, url.file)
                zip.parentFile.mkdirs()
                zip.createNewFile()
                zip.writeBytes(url.readBytes())
                val ztree = zipTree(zip.absolutePath)
                var copied = false
                val zipPath = "target/$ttriple/release/${targetFile.name}"
                ztree.visit {
                    if (isDirectory) return@visit
                    if (path == zipPath) {
                        targetFile.writeBytes(file.readBytes())
                        copied = true
                        println("Extracted $platform to $targetFile")
                    } else {
                        println("Skipping $file, not matching target file.")
                    }
                }
                if (!copied) {
                    throw RuntimeException("Failed to find $zipPath in the downloaded archive.")
                }
            } else {
                println("File $targetFile already exists, skipping download.")
            }
        }
    }
}

tasks.named("processResources") {
    dependsOn("downloadCoreLib")
}
