# =============================================================================
# Smithy SDK Generation
# =============================================================================
# This Makefile handles Smithy model compilation and SDK generation.
# =============================================================================

# Include common variables from parent directory
include ../makefiles/common.mk

# Local paths
BUILD_OUTPUT := output
BUILD_SRC := $(BUILD_OUTPUT)/source
PATCHES_DIR := patches

# Client output directories (relative to repo root)
JAVA_CLIENT_DIR := ../clients/java/sdk/src/main/java
PYTHON_CLIENT_DIR := ../clients/python/sdk
HASKELL_CLIENT_DIR := ../clients/haskell/sdk
JS_CLIENT_DIR := ../clients/javascript/sdk
RUST_CLIENT_DIR := ../crates/superposition_sdk
GENERATED_CLIENT_DIR := ../clients/generated/smithy
DOCS_API_DIR := ../docs/docs/api

.PHONY: all build clean clean-build clients api-docs updates help install-cli

# Default target
all: updates

# Help target - list available targets
help:
	@echo "Smithy SDK Generation Makefile"
	@echo ""
	@echo "Usage: make [target] or make smithy-[target] from repo root"
	@echo ""
	@echo "Targets:"
	@echo "  build        - Build Smithy models"
	@echo "  clean        - Remove build output"
	@echo "  clean-build  - Clean and rebuild"
	@echo "  clients      - Generate all client SDKs"
	@echo "  api-docs     - Generate API documentation"
	@echo "  updates      - Generate clients and API docs (default)"
	@echo "  install-cli  - Install Smithy CLI"
	@echo "  help         - Show this help message"

# Build Smithy models
build:
	@echo "Building Smithy models..."
	smithy build

# Clean build output
clean:
	@echo "Cleaning Smithy build output..."
	rm -rf $(BUILD_OUTPUT)

# Clean and rebuild
clean-build: clean build

# Generate all client SDKs from Smithy models
clients: build
	@echo "Generating client SDKs..."

	# Java client
	rm -rf $(JAVA_CLIENT_DIR)
	mkdir -p $(JAVA_CLIENT_DIR)
	cp -r $(BUILD_SRC)/java-client-codegen/* $(JAVA_CLIENT_DIR)
	git restore $(JAVA_CLIENT_DIR)/io/juspay/superposition/client/auth/BasicAuthIdentityResolver.java 2>/dev/null || true
	git restore $(JAVA_CLIENT_DIR)/io/juspay/superposition/client/auth/BearerTokenIdentityResolver.java 2>/dev/null || true

	# Python client
	rm -rf $(PYTHON_CLIENT_DIR)
	mkdir -p $(PYTHON_CLIENT_DIR)
	cp -r $(BUILD_SRC)/python-client-codegen/* $(PYTHON_CLIENT_DIR)
	git restore $(PYTHON_CLIENT_DIR)/superposition_sdk/auth_helpers.py 2>/dev/null || true
	git restore $(PYTHON_CLIENT_DIR)/AUTH_README.md 2>/dev/null || true

	# Haskell client
	rm -rf $(HASKELL_CLIENT_DIR)
	mkdir -p $(HASKELL_CLIENT_DIR)
	cp -r $(BUILD_SRC)/haskell-client-codegen/* $(HASKELL_CLIENT_DIR)

	# JavaScript client
	rm -rf $(JS_CLIENT_DIR)
	mkdir -p $(JS_CLIENT_DIR)
	git restore $(JS_CLIENT_DIR)/README.md 2>/dev/null || true
	cp -r $(BUILD_SRC)/typescript-client-codegen/* $(JS_CLIENT_DIR)
	git restore $(JS_CLIENT_DIR)/LICENSE 2>/dev/null || true

	# Rust client
	rm -rf $(RUST_CLIENT_DIR)
	mkdir -p $(RUST_CLIENT_DIR)
	git restore $(RUST_CLIENT_DIR)/README.md 2>/dev/null || true
	git restore $(RUST_CLIENT_DIR)/CHANGELOG.md 2>/dev/null || true
	cp -r $(BUILD_SRC)/rust-client-codegen/* $(RUST_CLIENT_DIR)

	# Other clients (Go, etc.)
	@for d in $(BUILD_SRC)/*-client-codegen; do \
		[ -d "$$d" ] || continue; \
		case "$$d" in *java*|*haskell*|*python*|*typescript*|*rust*) continue;; esac; \
		name=$$(basename "$$d" -client-codegen); \
		mkdir -p "$(GENERATED_CLIENT_DIR)/$$name"; \
		cp -r "$$d"/* "$(GENERATED_CLIENT_DIR)/$$name"; \
	done

	# Restore files and apply patches
	git restore $(PYTHON_CLIENT_DIR)/README.md 2>/dev/null || true
	git restore ../LICENSE 2>/dev/null || true
	git apply $(PATCHES_DIR)/*.patch 2>/dev/null || true

	@echo "Client SDKs generated successfully"

# Generate API documentation
api-docs: build
	@echo "Generating API documentation..."
	rm -rf $(DOCS_API_DIR)
	mkdir -p $(DOCS_API_DIR)
	cp $(BUILD_SRC)/openapi/Superposition.openapi.json $(DOCS_API_DIR)/
	cd ../docs && npm ci && npm run openapi-docs
	@echo "API documentation generated successfully"

# Generate both clients and API docs
updates: clients api-docs
	@echo "All Smithy updates completed"

# Install Smithy CLI (for CI or local setup)
install-cli:
	@echo "Installing Smithy CLI $(SMITHY_VERSION)..."
	@if ! command -v smithy &> /dev/null; then \
		mkdir -p /tmp/smithy-install && \
		curl -L https://github.com/smithy-lang/smithy/releases/download/$(SMITHY_VERSION)/smithy-cli-linux-x86_64.zip -o /tmp/smithy-install/smithy-cli.zip && \
		unzip -qo /tmp/smithy-install/smithy-cli.zip -d /tmp/smithy-install && \
		/tmp/smithy-install/smithy-cli-linux-x86_64/install && \
		rm -rf /tmp/smithy-install; \
	else \
		echo "Smithy CLI already installed"; \
	fi
