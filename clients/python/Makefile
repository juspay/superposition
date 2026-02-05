# =============================================================================
# Python Client Build
# =============================================================================
# This Makefile handles Python SDK, bindings, and provider package builds.
# Uses uv for dependency management and package building.
# =============================================================================

# Include common variables from repo root
include ../../makefiles/common.mk

# Package directories
BINDINGS_DIR := bindings
SDK_DIR := sdk
PROVIDER_DIR := provider
TESTS_DIR := provider-sdk-tests

# Build output
DIST_DIR := dist

.PHONY: all build build-bindings build-sdk build-provider \
	test clean install-deps publish setup-binaries

# Default target
all: build

# Build all Python packages
build: build-bindings build-sdk build-provider
	@echo "All Python packages built successfully"

# Build bindings package
build-bindings:
	@echo "Building Python bindings package..."
	cd $(BINDINGS_DIR) && uv build --wheel
	@echo "Bindings wheel built:"
	@ls -la $(BINDINGS_DIR)/dist/

# Build SDK package
build-sdk:
	@echo "Building Python SDK package..."
	cd $(SDK_DIR) && uv build --wheel
	@echo "SDK wheel built:"
	@ls -la $(SDK_DIR)/dist/

# Build provider package
build-provider:
	@echo "Building Python provider package..."
	cd $(PROVIDER_DIR) && uv build --wheel
	@echo "Provider wheel built:"
	@ls -la $(PROVIDER_DIR)/dist/

# Run provider SDK tests (requires superposition server running)
test:
	@echo "Running Python provider SDK tests..."
	cd $(TESTS_DIR) && VERSION=1.0.0 uv sync
	VERSION=1.0.0 uv run --directory $(TESTS_DIR) python main.py

# Clean build artifacts
clean:
	rm -rf $(BINDINGS_DIR)/dist $(BINDINGS_DIR)/*.egg-info
	rm -rf $(SDK_DIR)/dist $(SDK_DIR)/*.egg-info
	rm -rf $(PROVIDER_DIR)/dist $(PROVIDER_DIR)/*.egg-info
	rm -rf $(DIST_DIR)
	find . -type d -name __pycache__ -exec rm -rf {} + 2>/dev/null || true
	find . -type d -name .pytest_cache -exec rm -rf {} + 2>/dev/null || true

# Collect all wheels into a single directory
collect-wheels: build
	@echo "Collecting all Python wheels..."
	mkdir -p $(DIST_DIR)
	cp $(BINDINGS_DIR)/dist/*.whl $(DIST_DIR)/
	cp $(SDK_DIR)/dist/*.whl $(DIST_DIR)/
	cp $(PROVIDER_DIR)/dist/*.whl $(DIST_DIR)/
	@echo "All wheels collected in $(DIST_DIR):"
	@ls -la $(DIST_DIR)/

# Publish to PyPI (requires TWINE_USERNAME and TWINE_PASSWORD)
publish: collect-wheels
	@echo "Publishing Python packages to PyPI..."
	twine upload $(DIST_DIR)/*.whl

# Setup native binaries for the bindings package
# This expects binaries to be in rust-binaries/ directory (relative to repo root)
setup-binaries:
	@echo "Setting up native binaries for Python bindings..."
	@if [ -d "../../rust-binaries" ]; then \
		for zip_file in ../../rust-binaries/*.zip; do \
			echo "Extracting $$zip_file"; \
			zip_basename=$$(basename "$$zip_file" .zip); \
			target_triple="$${zip_basename#superposition_core-}"; \
			mkdir -p /tmp/py_extract/$$target_triple; \
			unzip -o "$$zip_file" -d /tmp/py_extract/$$target_triple/; \
			find /tmp/py_extract/$$target_triple -name "libsuperposition_core.*" -o -name "superposition_core.*" | while read lib; do \
				filename=$$(basename "$$lib"); \
				extension="$${filename##*.}"; \
				file="libsuperposition_core-$$target_triple.$$extension"; \
				cp "$$lib" $(BINDINGS_DIR)/superposition_bindings/$$file; \
				echo "Copied $$file"; \
			done; \
			rm -rf /tmp/py_extract; \
		done; \
		echo "Native libraries in bindings package:"; \
		ls -la $(BINDINGS_DIR)/superposition_bindings/; \
	else \
		echo "No rust-binaries directory found. Skipping binary setup."; \
	fi

# Install development dependencies
install-deps:
	@echo "Installing development dependencies..."
	cd $(BINDINGS_DIR) && uv sync
	cd $(SDK_DIR) && uv sync
	cd $(PROVIDER_DIR) && uv sync
	cd $(TESTS_DIR) && VERSION=1.0.0 uv sync
