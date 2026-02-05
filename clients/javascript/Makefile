# =============================================================================
# JavaScript Client Build
# =============================================================================
# This Makefile handles JavaScript/TypeScript SDK, bindings, and provider builds.
# Uses npm workspaces for package management.
# =============================================================================

# Include common variables from repo root
include ../../makefiles/common.mk

# Package directories (npm workspaces)
BINDINGS_DIR := bindings
SDK_DIR := sdk
PROVIDER_DIR := open-feature-provider
TESTS_DIR := provider-sdk-tests

# Native library directory
NATIVE_LIB_DIR := $(BINDINGS_DIR)/native-lib

.PHONY: all build install test clean publish \
	build-sdk setup-binaries provider-test

# Default target
all: build

# Install dependencies
install:
	@echo "Installing npm dependencies..."
	npm ci

# Build all packages
build: install build-sdk
	@echo "All JavaScript packages built successfully"

# Build the SDK package
build-sdk:
	@echo "Building JavaScript SDK..."
	cd $(SDK_DIR) && npm ci && \
		npm run build:cjs && \
		npm run build:types && \
		npm run build:es
	@echo "SDK built successfully"

# Run tests
test:
	@echo "Running JavaScript tests..."
	npm test

# Run provider SDK tests (requires superposition server running)
provider-test:
	@echo "Running JavaScript provider SDK tests..."
	cd $(TESTS_DIR) && npm ci
	bash ../../scripts/setup_provider_binaries.sh js
	node $(TESTS_DIR)/index.js

# Clean build artifacts
clean:
	rm -rf node_modules
	rm -rf $(SDK_DIR)/node_modules $(SDK_DIR)/dist $(SDK_DIR)/lib
	rm -rf $(BINDINGS_DIR)/node_modules $(BINDINGS_DIR)/dist
	rm -rf $(PROVIDER_DIR)/node_modules $(PROVIDER_DIR)/dist
	rm -rf $(TESTS_DIR)/node_modules
	rm -rf $(NATIVE_LIB_DIR)

# Publish all packages to npm
publish: install
	@echo "Publishing JavaScript packages to npm..."
	npm publish --access public --workspace bindings --workspace sdk --workspace open-feature-provider

# Setup native binaries for the bindings package
# This expects binaries to be in rust-binaries/ directory (relative to repo root)
setup-binaries:
	@echo "Setting up native binaries for JavaScript bindings..."
	mkdir -p $(NATIVE_LIB_DIR)
	@if [ -d "../../rust-binaries" ]; then \
		for zip_file in ../../rust-binaries/*.zip; do \
			echo "Extracting $$zip_file"; \
			zip_basename=$$(basename "$$zip_file" .zip); \
			target_triple="$${zip_basename#superposition_core-}"; \
			mkdir -p /tmp/js_extract/$$target_triple; \
			unzip -o "$$zip_file" -d /tmp/js_extract/$$target_triple/; \
			find /tmp/js_extract/$$target_triple -name "libsuperposition_core.*" -o -name "superposition_core.*" | while read lib; do \
				filename=$$(basename "$$lib"); \
				extension="$${filename##*.}"; \
				file="libsuperposition_core-$$target_triple.$$extension"; \
				cp "$$lib" $(NATIVE_LIB_DIR)/$$file; \
				echo "Copied $$file"; \
			done; \
			rm -rf /tmp/js_extract; \
		done; \
		echo "Native libraries in bindings package:"; \
		ls -la $(NATIVE_LIB_DIR)/; \
	else \
		echo "No rust-binaries directory found. Skipping binary setup."; \
	fi
