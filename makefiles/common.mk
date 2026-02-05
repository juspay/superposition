# =============================================================================
# Common Variables and Tool Versions
# =============================================================================
# This file serves as the SINGLE SOURCE OF TRUTH for tool versions and shared
# configuration used across the Makefile and CI workflows.
# =============================================================================

# -----------------------------------------------------------------------------
# Tool Versions (Update these to change versions across all CI/local builds)
# -----------------------------------------------------------------------------
RUST_VERSION := 1.90.0
RUST_TARGETS := wasm32-unknown-unknown
RUST_COMPONENTS := rustfmt,clippy

NODE_VERSION := 18.19.0
PYTHON_VERSION := 3.12
JAVA_VERSION := 17
SMITHY_VERSION := 1.55.0
LEPTOSFMT_VERSION := 0.1.33
COCOGITTO_VERSION := 6.2.0
CARGO_EDIT_VERSION := 0.13.6
YQ_VERSION := 4.44.2

# -----------------------------------------------------------------------------
# CI Environment Variables
# -----------------------------------------------------------------------------
# These are exported for CI builds to optimize compilation and improve reliability.
# See: https://matklad.github.io/2021/09/04/fast-rust-builds.html#ci-workflow

# Disable incremental compilation in CI (not useful for fresh builds)
export CARGO_INCREMENTAL ?= 0

# Allow more retries for network requests (reduces flaky CI failures)
export CARGO_NET_RETRY ?= 10
export RUSTUP_MAX_RETRIES ?= 10

# Use short backtraces in CI logs
export RUST_BACKTRACE ?= short

# Use cargo's sparse index protocol for faster dependency resolution
export CARGO_REGISTRIES_CRATES_IO_PROTOCOL ?= sparse

# -----------------------------------------------------------------------------
# AWS Mock Configuration (for local development and CI testing)
# -----------------------------------------------------------------------------
export AWS_ACCESS_KEY_ID ?= test
export AWS_SECRET_ACCESS_KEY ?= test
export AWS_SESSION_TOKEN ?= test
export AWS_REGION ?= ap-south-1

# -----------------------------------------------------------------------------
# Application Configuration
# -----------------------------------------------------------------------------
export APP_ENV ?= $(if $(CI),TEST,DEV)

# -----------------------------------------------------------------------------
# Project Paths
# -----------------------------------------------------------------------------
SMITHY_BUILD_SRC := smithy/output/source
SMITHY_CLIENT_DIR := clients/generated/smithy

# -----------------------------------------------------------------------------
# Shell Configuration
# -----------------------------------------------------------------------------
SHELL := /usr/bin/env bash

# -----------------------------------------------------------------------------
# Docker/Podman Detection
# -----------------------------------------------------------------------------
HAS_DOCKER := $(shell command -v docker > /dev/null; echo $$?)
HAS_PODMAN := $(shell command -v podman > /dev/null; echo $$?)

ifeq ($(HAS_DOCKER),0)
  DOCKER := docker
else ifeq ($(HAS_PODMAN),0)
  DOCKER := podman
  export PODMAN_COMPOSE_WARNING_LOGS = false
else
  $(warning "Neither docker nor podman found. Container commands will fail.")
  DOCKER := docker
endif

COMPOSE := $(DOCKER) compose

# -----------------------------------------------------------------------------
# OS Detection
# -----------------------------------------------------------------------------
OS := $(shell uname -s)
ifeq ($(OS),Darwin)
  LIB_EXTENSION := dylib
else ifeq ($(OS),Linux)
  LIB_EXTENSION := so
else ifneq (,$(findstring MINGW,$(OS)))
  LIB_EXTENSION := dll
else ifneq (,$(findstring CYGWIN,$(OS)))
  LIB_EXTENSION := dll
else
  $(warning Unsupported OS type: $(OS). Defaulting to .so extension.)
  LIB_EXTENSION := so
endif

# -----------------------------------------------------------------------------
# Container Helper Functions
# -----------------------------------------------------------------------------
define read-container-name
	yq -r '.services.$(1).container_name' docker-compose.yaml
endef

define check-container
	$(DOCKER) ps | grep $(1) 2>&1 > /dev/null; echo $$?
endef

# -----------------------------------------------------------------------------
# Cargo Configuration
# -----------------------------------------------------------------------------
IMAGE_NAME ?= context-aware-config
DOCKER_DNS ?= localhost
FEATURES ?= ssr
FE_FEATURES ?= hydrate
CARGO_FLAGS := --color always --no-default-features
EXCLUDE_PACKAGES := experimentation_client_integration_example superposition_sdk
FMT_EXCLUDE_PACKAGES_REGEX := $(shell echo "$(EXCLUDE_PACKAGES)" | sed "s/ /|/g")
LINT_FLAGS := --workspace --all-targets --all-features $(addprefix --exclude ,$(EXCLUDE_PACKAGES)) --no-deps
COMPONENT_NAME_FLAGS :=
CARGO_TARGET_DIR := $(shell cargo metadata --no-deps --format-version 1 2>/dev/null | jq -r .target_directory 2>/dev/null || echo "target")

# Get all workspace members and filter out excluded ones
define get_workspace_members
	cargo metadata --format-version 1 --no-deps | \
	jq -r '.workspace_members[]' | \
	awk -F '#' '{print $$1;}' | awk -F '/' '{print $$NF;}' | \
	grep -v -E '$(FMT_EXCLUDE_PACKAGES_REGEX)'
endef

WORKSPACE_MEMBERS := $(shell $(get_workspace_members) 2>/dev/null)
FMT_PACKAGE_FLAGS := $(addprefix -p ,$(WORKSPACE_MEMBERS))
LEPTOS_PACKAGES := $(addprefix crates/,$(WORKSPACE_MEMBERS))
WASM_PACK_MODE ?= --dev

# Export Smithy Maven repos
export SMITHY_MAVEN_REPOS = https://repo1.maven.org/maven2|https://sandbox.assets.juspay.in/smithy/m2

# -----------------------------------------------------------------------------
# Utility Targets
# -----------------------------------------------------------------------------
.PHONY: print-versions print-config

# Print all tool versions (useful for debugging)
print-versions:
	@echo "=== Tool Versions ==="
	@echo "RUST_VERSION:        $(RUST_VERSION)"
	@echo "RUST_TARGETS:        $(RUST_TARGETS)"
	@echo "NODE_VERSION:        $(NODE_VERSION)"
	@echo "PYTHON_VERSION:      $(PYTHON_VERSION)"
	@echo "JAVA_VERSION:        $(JAVA_VERSION)"
	@echo "SMITHY_VERSION:      $(SMITHY_VERSION)"
	@echo "LEPTOSFMT_VERSION:   $(LEPTOSFMT_VERSION)"
	@echo "COCOGITTO_VERSION:   $(COCOGITTO_VERSION)"
	@echo "CARGO_EDIT_VERSION:  $(CARGO_EDIT_VERSION)"

# Print configuration (useful for debugging)
print-config:
	@echo "=== Configuration ==="
	@echo "DOCKER:              $(DOCKER)"
	@echo "CARGO_TARGET_DIR:    $(CARGO_TARGET_DIR)"
	@echo "LIB_EXTENSION:       $(LIB_EXTENSION)"
	@echo "CI:                  $(CI)"
	@echo "APP_ENV:             $(APP_ENV)"
