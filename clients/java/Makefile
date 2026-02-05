# =============================================================================
# Java Client Build
# =============================================================================
# This Makefile wraps Gradle commands for the Java client SDK.
# =============================================================================

# Include common variables from repo root
include ../../makefiles/common.mk

# Gradle wrapper
GRADLE := ./gradlew

.PHONY: all build assemble test clean publish check \
	provider-test setup-binaries help

# Default target
all: build

# Help target - list available targets
help:
	@echo "Java Client Makefile"
	@echo ""
	@echo "Usage: make [target] or make java-[target] from repo root"
	@echo ""
	@echo "Targets:"
	@echo "  build          - Build all Java modules"
	@echo "  assemble       - Assemble without tests"
	@echo "  test           - Run tests"
	@echo "  clean          - Clean build artifacts"
	@echo "  check          - Run lint checks"
	@echo "  publish-local  - Publish to local Maven repo"
	@echo "  publish        - Publish to remote repository"
	@echo "  deploy         - Deploy to Sonatype"
	@echo "  provider-test  - Run provider SDK tests"
	@echo "  setup-binaries - Setup native binaries"
	@echo "  help           - Show this help message"

# Make gradlew executable
$(GRADLE):
	chmod +x $(GRADLE)

# Build all Java modules
build: $(GRADLE)
	$(GRADLE) build

# Assemble without running tests
assemble: $(GRADLE)
	$(GRADLE) assemble

# Run tests
test: $(GRADLE)
	$(GRADLE) test

# Clean build artifacts
clean: $(GRADLE)
	$(GRADLE) clean

# Check (lint, etc.)
check: $(GRADLE)
	$(GRADLE) check

# Publish to local Maven repository
publish-local: $(GRADLE)
	$(GRADLE) publishToMavenLocal

# Publish to remote repository (used in CI release)
publish: $(GRADLE)
	$(GRADLE) clean publish

# Deploy to Sonatype (used in CI release)
deploy: $(GRADLE)
	$(GRADLE) jreleaserDeploy --info --stacktrace

# Run provider SDK tests (requires superposition server running)
provider-test: $(GRADLE)
	$(GRADLE) :provider-sdk-tests:run

# Setup native binaries for the bindings package
# This expects binaries to be in rust-binaries/ directory
setup-binaries:
	@echo "Setting up native binaries for Java bindings..."
	mkdir -p bindings/src/main/resources
	@if [ -d "rust-binaries" ]; then \
		mkdir -p temp_extract; \
		for zip_file in rust-binaries/*.zip; do \
			echo "Extracting $$zip_file"; \
			unzip -o "$$zip_file" -d temp_extract/; \
			find temp_extract -name "libsuperposition_core.*" -o -name "superposition_core.*" | while read lib; do \
				filename=$$(basename "$$lib"); \
				extension="$${filename##*.}"; \
				zip_basename=$$(basename "$$zip_file" .zip); \
				target_triple="$${zip_basename#superposition_core-}"; \
				case "$$target_triple" in \
					"x86_64-apple-darwin") platform="darwin-x86-64" ;; \
					"aarch64-apple-darwin") platform="darwin-aarch64" ;; \
					"x86_64-unknown-linux-gnu") platform="linux-x86-64" ;; \
					"x86_64-pc-windows-msvc") platform="win32-x86-64" ;; \
					*) echo "Unknown target: $$target_triple"; continue ;; \
				esac; \
				target_dir="bindings/src/main/resources/$$platform"; \
				mkdir -p "$$target_dir"; \
				cp "$$lib" "$$target_dir/libsuperposition_core.$$extension"; \
				echo "Copied to $$target_dir/"; \
			done; \
			rm -rf temp_extract; \
		done; \
		echo "Native libraries setup complete"; \
		ls -laR bindings/src/main/resources/; \
	else \
		echo "No rust-binaries directory found. Skipping binary setup."; \
	fi
