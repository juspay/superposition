# =============================================================================
# Superposition - Root Makefile
# =============================================================================
# This is the main orchestration Makefile that:
# - Includes shared configuration from makefiles/common.mk
# - Includes CI targets from makefiles/ci.mk
# - Delegates to component-specific Makefiles where appropriate
# - Provides high-level build, test, and development targets
# =============================================================================

# Include shared configuration (versions, env vars, utilities)
include makefiles/common.mk

# Include CI-specific targets
include makefiles/ci.mk

# -----------------------------------------------------------------------------
# Container Management
# -----------------------------------------------------------------------------
DB_CONTAINER_NAME = $(shell $(call read-container-name,postgres))
DB_UP = $(shell $(call check-container,$(DB_CONTAINER_NAME)))
LSTACK_CONTAINER_NAME = $(shell $(call read-container-name,localstack))
LSTACK_UP = $(shell $(call check-container,$(LSTACK_CONTAINER_NAME)))

# -----------------------------------------------------------------------------
# Phony Targets
# -----------------------------------------------------------------------------
.PHONY: amend \
	amend-no-edit \
	backend \
	build \
	check \
	cleanup \
	clients \
	commit \
	db \
	db-init \
	default \
	env-file \
	fmt \
	frontend \
	get-password \
	grafana-local \
	kill \
	legacy_db_setup \
	leptosfmt \
	lint \
	lint-fix \
	local-docs-view \
	localstack \
	migration \
	node-dependencies \
	provider-template \
	run \
	run_legacy \
	schema-file \
	setup \
	setup-clients \
	smithy-api-docs \
	smithy-build \
	smithy-clean \
	smithy-clean-build \
	smithy-clients \
	smithy-updates \
	superposition \
	superposition-example \
	superposition_dev \
	superposition_legacy \
	tailwind \
	test \
	test-js-provider \
	test-kotlin-provider \
	test-py-provider \
	test-rust-provider \
	uniffi-bindings \
	validate-aws-connection \
	validate-psql-connection

# =============================================================================
# Environment & Setup
# =============================================================================

env-file:
	@if ! [ -e .env ]; then \
		echo ".env missing, copying .env.example as .env" && \
		cp .env.example .env; \
	fi

db:
ifndef CI
ifeq ($(DB_UP),1)
	$(COMPOSE) up -d postgres
endif
else
	@echo "Skipping postgres container-setup in CI."
endif
	@echo "Verifying postgres readiness..."
	@while ! pg_isready -h $(DOCKER_DNS) -p 5432; do sleep 0.5; done

localstack:
ifndef CI
ifeq ($(LSTACK_UP),1)
	$(COMPOSE) up -d localstack
endif
else
	@echo "Skipping localstack container-setup in CI."
endif
	@echo "Verifying localstack readiness..."
	@while ! aws --no-cli-pager --endpoint-url=http://$(DOCKER_DNS):4566 --region=ap-south-1 sts get-caller-identity; do \
		sleep 0.5; \
	done

db-init:
	set -a; . .env; set +a; psql "$$DATABASE_URL" -f superposition.sql

cleanup:
	-$(DOCKER) rm -f $$($(DOCKER) container ls --filter name=^superposition -a -q)
	-$(DOCKER) rmi -f $$($(DOCKER) images | grep $(DB_CONTAINER_NAME) | cut -f 10 -d " ")

migration: cleanup db
	diesel migration run --locked-schema --config-file=crates/superposition_types/src/database/diesel.toml
	$(COMPOSE) down

legacy_db_setup:
	grep 'DATABASE_URL=' .env | sed -e 's/DATABASE_URL=//' | xargs ./scripts/legacy-db-setup.sh

validate-aws-connection:
	aws --no-cli-pager --endpoint-url=http://$(DOCKER_DNS):4566 --region=ap-south-1 sts get-caller-identity

validate-psql-connection:
	pg_isready -h $(DOCKER_DNS) -p 5432

node-dependencies:
	npm ci

SETUP_DEPS = env-file db
ifdef CI
	SETUP_DEPS += db-init
endif
setup: $(SETUP_DEPS) node-dependencies setup-clients

# =============================================================================
# Client SDK Setup
# =============================================================================

# Setup JavaScript SDK (build and install test dependencies)
setup-clients:
	$(MAKE) -C clients/javascript build-sdk
	cd tests && bun install

# Build all clients (Smithy generation + setup)
clients: smithy-clients setup-clients

# =============================================================================
# Smithy SDK Generation (delegates to smithy/Makefile)
# =============================================================================

smithy-clean:
	$(MAKE) -C smithy clean

smithy-build:
	$(MAKE) -C smithy build

smithy-clean-build:
	$(MAKE) -C smithy clean-build

smithy-clients:
	$(MAKE) -C smithy clients

smithy-api-docs:
	$(MAKE) -C smithy api-docs

smithy-updates:
	$(MAKE) -C smithy updates

# =============================================================================
# Build Targets
# =============================================================================

kill:
	-@pkill -f $(CARGO_TARGET_DIR)/debug/superposition &

get-password:
	export DB_PASSWORD=`./docker-compose/localstack/get_db_password.sh` && echo $$DB_PASSWORD

superposition: CARGO_FLAGS += --features='$(FEATURES)'
superposition:
	cargo build $(CARGO_FLAGS) --bin superposition
	@cd $(CARGO_TARGET_DIR) && ln -s ../node_modules node_modules || true

superposition-example:
	cargo run --bin cac-demo-app

superposition_legacy: CARGO_FLAGS += --features='$(FEATURES)
superposition_legacy: CARGO_FLAGS += superposition_types/disable_db_data_validation
superposition_legacy: CARGO_FLAGS += context_aware_config/disable_db_data_validation
superposition_legacy: CARGO_FLAGS += experimentation_platform/disable_db_data_validation'
superposition_legacy:
	cargo build $(CARGO_FLAGS) --bin superposition

superposition_dev: CARGO_FLAGS += --features='$(FEATURES)'
superposition_dev:
	# export DB_PASSWORD=`./docker-compose/localstack/get_db_password.sh`
	cargo watch -x 'run $(CARGO_FLAGS) --bin superposition'

frontend:
	cd crates/frontend && \
		wasm-pack build --target web $(WASM_PACK_MODE) --no-default-features --features '$(FE_FEATURES)'
	cd crates/frontend && \
		npx tailwindcss -i ./styles/tailwind.css -o ./pkg/style.css
	-rm -rf $(CARGO_TARGET_DIR)/site
	mkdir -p $(CARGO_TARGET_DIR)/site/pkg
	mv crates/frontend/pkg $(CARGO_TARGET_DIR)/site/
	cp -a crates/frontend/assets/. $(CARGO_TARGET_DIR)/site/

backend: CARGO_FLAGS += --features='$(FEATURES)' --color always
backend:
	-rm -rf $(CARGO_TARGET_DIR)/node_modules
	npm --prefix ./crates/context_aware_config/ ci
	mv crates/context_aware_config/node_modules $(CARGO_TARGET_DIR)/
	cargo build $(CARGO_FLAGS)

build: frontend backend

tailwind:
	cd crates/frontend && npx tailwindcss -i ./styles/tailwind.css -o ./pkg/style.css --watch

# =============================================================================
# Run Targets
# =============================================================================

run: kill db frontend superposition
	@$(CARGO_TARGET_DIR)/debug/superposition

%_run: kill db frontend superposition
	@RUST_LOG=$* $(CARGO_TARGET_DIR)/debug/superposition

run_legacy: kill build db superposition_legacy
	@$(CARGO_TARGET_DIR)/debug/superposition

# =============================================================================
# Code Quality
# =============================================================================

leptosfmt:
	leptosfmt $(LEPTOS_FMT_FLAGS) $(LEPTOS_PACKAGES)

lint-fix: LINT_FLAGS += --fix --allow-dirty --allow-staged
lint-fix: COMPONENT_NAME_FLAGS = --fix
lint-fix: lint

check: FMT_FLAGS += --check
check: LEPTOS_FMT_FLAGS += --check
check: LINT_FLAGS += -- -Dwarnings
check: fmt leptosfmt lint

fmt:
	@echo "Running cargo fmt on packages: $(WORKSPACE_MEMBERS)"
	cargo fmt $(FMT_PACKAGE_FLAGS)

lint:
	cargo clippy $(LINT_FLAGS)
	@./scripts/check_component_names.sh $(COMPONENT_NAME_FLAGS)

commit: check
	git commit $(COMMIT_FLAGS)

amend: COMMIT_FLAGS += --amend
amend: commit

amend-no-edit: COMMIT_FLAGS += --no-edit
amend-no-edit: amend

# =============================================================================
# Testing
# =============================================================================

test: WASM_PACK_MODE=--profiling
test: setup frontend superposition
	cargo test
	@echo "Running superposition"
	@$(CARGO_TARGET_DIR)/debug/superposition &
	@echo "Awaiting superposition boot..."
## FIXME Curl doesn't retry.
	@curl	--silent --retry 10 \
				--connect-timeout 2 \
				--retry-all-errors \
				'http://localhost:8080/health' 2>&1 > /dev/null
	cd tests && bun test:clean
	-@pkill -f $(CARGO_TARGET_DIR)/debug/superposition &

# =============================================================================
# Provider Tests
# =============================================================================

provider-template: setup superposition
	@$(CARGO_TARGET_DIR)/debug/superposition &
	@echo "Awaiting superposition boot..."
	@curl	--silent --retry 10 \
				--connect-timeout 2 \
				--retry-all-errors \
				'http://localhost:8080/health' 2>&1 > /dev/null

test-js-provider: provider-template
	cd clients/javascript/provider-sdk-tests && npm ci
	bash ./scripts/setup_provider_binaries.sh js
	node clients/javascript/provider-sdk-tests/index.js
	-@pkill -f $(CARGO_TARGET_DIR)/debug/superposition

test-py-provider: provider-template
	bash ./scripts/setup_provider_binaries.sh py
	cd clients/python/provider-sdk-tests && VERSION=1.0.0 uv sync
	VERSION=1.0.0 uv run --directory clients/python/provider-sdk-tests python main.py
	-@pkill -f $(CARGO_TARGET_DIR)/debug/superposition

test-kotlin-provider: provider-template
	bash ./scripts/setup_provider_binaries.sh kotlin
	$(MAKE) -C clients/java provider-test
	-@pkill -f $(CARGO_TARGET_DIR)/debug/superposition

test-rust-provider: provider-template
	cargo test --package superposition_provider --test integration_test -- --nocapture --ignored
	-@pkill -f $(CARGO_TARGET_DIR)/debug/superposition

# =============================================================================
# UniFII Bindings
# =============================================================================

uniffi-bindings:
	cargo build --package superposition_core --lib --release
	cargo run --bin uniffi-bindgen generate --library $(CARGO_TARGET_DIR)/release/libsuperposition_core.$(LIB_EXTENSION) --language kotlin --out-dir clients/java/bindings/src/main/kotlin
	cargo run --bin uniffi-bindgen generate --library $(CARGO_TARGET_DIR)/release/libsuperposition_core.$(LIB_EXTENSION) --language python --out-dir clients/python/bindings/superposition_bindings
	git apply uniffi/patches/*.patch

# =============================================================================
# Documentation & Utilities
# =============================================================================

grafana-local:
	cd grafana && $(COMPOSE) up

local-docs-view: smithy-api-docs
	cd docs && npm start

default: dev-build frontend

schema-file:
	diesel migration run --config-file=crates/superposition_types/src/database/diesel.toml
	diesel print-schema > crates/superposition_types/src/database/schema.rs
	git apply crates/superposition_types/src/database/schema.patch
	diesel print-schema --schema superposition > crates/superposition_types/src/database/superposition_schema.rs
	git apply crates/superposition_types/src/database/superposition_schema.patch
	git apply crates/superposition_types/src/database/schema-timestamp-migration.patch
