# =============================================================================
# CI-Specific Targets
# =============================================================================
# This file contains targets used by GitHub Actions workflows.
# These targets set up tools and run CI checks, using versions from common.mk.
# =============================================================================

# Ensure common.mk is included (for version variables)
ifndef RUST_VERSION
  $(error "common.mk must be included before ci.mk")
endif

.PHONY: ci-info \
	ci-setup-rust \
	ci-setup-rust-wasm \
	ci-setup-leptosfmt \
	ci-setup-cocogitto \
	ci-setup-cargo-edit \
	ci-setup-node \
	ci-setup-python \
	ci-setup-smithy \
	ci-setup-wasm-pack \
	ci-formatting \
	ci-test \
	ci-uniffi-bindings \
	ci-smithy-updates

# -----------------------------------------------------------------------------
# CI Information
# -----------------------------------------------------------------------------
ci-info:
	@echo "=== CI Environment ==="
	@echo "CI:                  $(CI)"
	@echo "RUST_VERSION:        $(RUST_VERSION)"
	@echo "NODE_VERSION:        $(NODE_VERSION)"
	@echo "PYTHON_VERSION:      $(PYTHON_VERSION)"
	@echo "SMITHY_VERSION:      $(SMITHY_VERSION)"
	@echo "LEPTOSFMT_VERSION:   $(LEPTOSFMT_VERSION)"

# -----------------------------------------------------------------------------
# Tool Setup Targets
# -----------------------------------------------------------------------------
# These targets install tools needed for CI. They're designed to be idempotent
# and can be used both locally and in CI.

# Install Rust toolchain (used by workflows via dtolnay/rust-toolchain action)
# This target is for local use - CI uses the action directly
ci-setup-rust:
	@echo "Setting up Rust $(RUST_VERSION)..."
	rustup install $(RUST_VERSION)
	rustup default $(RUST_VERSION)
	rustup component add rustfmt clippy

# Add WASM target
ci-setup-rust-wasm: ci-setup-rust
	@echo "Adding WASM target..."
	rustup target add $(RUST_TARGETS)

# Install leptosfmt
ci-setup-leptosfmt:
	@echo "Installing leptosfmt $(LEPTOSFMT_VERSION)..."
	@if ! command -v leptosfmt &> /dev/null || ! leptosfmt --version | grep -q "$(LEPTOSFMT_VERSION)"; then \
		cargo install leptosfmt --version $(LEPTOSFMT_VERSION) --locked; \
	else \
		echo "leptosfmt $(LEPTOSFMT_VERSION) already installed"; \
	fi

# Install cocogitto (for conventional commits)
ci-setup-cocogitto:
	@echo "Installing cocogitto $(COCOGITTO_VERSION)..."
	@if ! command -v cog &> /dev/null; then \
		cargo install cocogitto --version $(COCOGITTO_VERSION) --locked; \
	else \
		echo "cocogitto already installed"; \
	fi

# Install cargo-edit (for version management)
ci-setup-cargo-edit:
	@echo "Installing cargo-edit $(CARGO_EDIT_VERSION)..."
	@if ! command -v cargo-set-version &> /dev/null; then \
		cargo install cargo-edit --version $(CARGO_EDIT_VERSION) --locked; \
	else \
		echo "cargo-edit already installed"; \
	fi

# Install wasm-pack
ci-setup-wasm-pack:
	@echo "Installing wasm-pack..."
	@if ! command -v wasm-pack &> /dev/null; then \
		curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh; \
	else \
		echo "wasm-pack already installed"; \
	fi

# Install Smithy CLI
ci-setup-smithy:
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

# Setup Node.js (version management is handled by actions/setup-node in CI)
ci-setup-node:
	@echo "Expected Node version: $(NODE_VERSION)"
	@echo "Current Node version: $$(node --version 2>/dev/null || echo 'not installed')"

# Setup Python (version management is handled by actions/setup-python in CI)
ci-setup-python:
	@echo "Expected Python version: $(PYTHON_VERSION)"
	@echo "Current Python version: $$(python3 --version 2>/dev/null || echo 'not installed')"

# -----------------------------------------------------------------------------
# CI Composite Targets
# -----------------------------------------------------------------------------
# These targets are called directly by GitHub Actions workflows.

# Run formatting and linting checks (used by ci_check_pr.yaml formatting job)
ci-formatting: check
	@echo "Formatting and linting checks passed"

# Run full test suite (used by ci_check_pr.yaml test job)
ci-test: test
	@echo "Test suite completed"

# Generate and verify uniffi bindings (used by ci_check_pr.yaml binding-generation-check job)
ci-uniffi-bindings: uniffi-bindings
	@echo "UniFII bindings generated"

# Generate and verify Smithy SDKs (used by ci_check_pr.yaml smithy-sdk-generation-check job)
ci-smithy-updates: smithy-updates
	@echo "Smithy SDKs generated"

# -----------------------------------------------------------------------------
# CI Provider Test Targets
# -----------------------------------------------------------------------------
ci-test-js-provider: test-js-provider
	@echo "JavaScript provider tests completed"

ci-test-py-provider: test-py-provider
	@echo "Python provider tests completed"

ci-test-kotlin-provider: test-kotlin-provider
	@echo "Kotlin provider tests completed"

ci-test-rust-provider: test-rust-provider
	@echo "Rust provider tests completed"

# -----------------------------------------------------------------------------
# Version Output for CI
# -----------------------------------------------------------------------------
# These targets output version information that can be used in CI workflows

ci-rust-version:
	@echo "$(RUST_VERSION)"

ci-node-version:
	@echo "$(NODE_VERSION)"

ci-python-version:
	@echo "$(PYTHON_VERSION)"

ci-java-version:
	@echo "$(JAVA_VERSION)"

ci-smithy-version:
	@echo "$(SMITHY_VERSION)"

ci-leptosfmt-version:
	@echo "$(LEPTOSFMT_VERSION)"
