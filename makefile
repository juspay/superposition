SMITHY_BUILD_SRC := smithy/output/source
SMITHY_CLIENT_DIR := clients/generated/smithy
IMAGE_NAME ?= context-aware-config
DOCKER_DNS ?= localhost
SHELL := /usr/bin/env bash
FEATURES ?= ssr
FE_FEATURES ?= hydrate
CARGO_FLAGS := --color always --no-default-features
EXCLUDE_PACKAGES := experimentation_client_integration_example superposition_sdk
FMT_EXCLUDE_PACKAGES_REGEX := $(shell echo "$(EXCLUDE_PACKAGES)" | sed "s/ /|/g")
LINT_FLAGS := --workspace --all-targets $(addprefix --exclude ,$(EXCLUDE_PACKAGES)) --no-deps
CARGO_TARGET_DIR := $(shell cargo metadata --no-deps --format-version 1 | jq -r .target_directory)

# Get all workspace members and filter out excluded ones
#
define get_workspace_members
	cargo metadata --format-version 1 --no-deps | \
	jq -r '.workspace_members[]' | \
	awk -F '#' '{print $$1;}' | awk -F '/' '{print $$NF;}' | \
	grep -v -E '$(FMT_EXCLUDE_PACKAGES_REGEX)'
endef

WORKSPACE_MEMBERS := $(shell $(get_workspace_members))

# Create the package flags for cargo fmt
FMT_PACKAGE_FLAGS := $(addprefix -p ,$(WORKSPACE_MEMBERS))
LEPTOS_PACKAGES := $(addprefix crates/,$(WORKSPACE_MEMBERS))
WASM_PACK_MODE ?= --dev
HAS_DOCKER := $(shell command -v docker > /dev/null; echo $$?)
HAS_PODMAN := $(shell command -v podman > /dev/null; echo $$?)
ifeq ($(HAS_DOCKER),0)
  DOCKER := docker
else ifeq ($(HAS_PODMAN),0)
  DOCKER := podman
  export PODMAN_COMPOSE_WARNING_LOGS = false
else
	$(error "Neither docker nor podman found, please install one of them.")
endif
COMPOSE := $(DOCKER) compose

define read-container-name
	yq -r '.services.$(1).container_name' docker-compose.yaml
endef
define check-container
	$(DOCKER) ps | grep $(1) 2>&1 > /dev/null; echo $$?
endef
DB_CONTAINER_NAME = $(shell $(call read-container-name,postgres))
DB_UP = $(shell $(call check-container,$(DB_CONTAINER_NAME)))
LSTACK_CONTAINER_NAME = $(shell $(call read-container-name,localstack))
LSTACK_UP = $(shell $(call check-container,$(LSTACK_CONTAINER_NAME)))
export SMITHY_MAVEN_REPOS = https://repo1.maven.org/maven2|https://sandbox.assets.juspay.in/smithy/m2
.PHONY:
	cac
	ci-test
	clients
	db-init
	grafana-local
	kill
	local-docs-view
	node-dependencies
	run
	schema-file
	setup
	setup-clients
	smithy-clean
	smithy-build
	smithy-clean-build
	smithy-api-docs
	smithy-updates
	validate-aws-connection
	validate-psql-connection
	uniffi-bindings
	test-js-provider
	test-py-provider
	test-kotlin-provider
	test-rust-provider

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

setup-clients:
	cd  clients/javascript/sdk && npm ci &&\
		npm run build:cjs &&\
		npm run build:types &&\
		npm run build:es
	cd tests && bun install

clients: smithy-clients setup-clients

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

superposition_jsonlogic: CARGO_FLAGS += --features='$(FEATURES) jsonlogic'
superposition_jsonlogic:
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

run: kill db frontend superposition
	@$(CARGO_TARGET_DIR)/debug/superposition

%_run: kill db frontend superposition
	@RUST_LOG=$* $(CARGO_TARGET_DIR)/debug/superposition

run_legacy: kill build db superposition_legacy
	@$(CARGO_TARGET_DIR)/debug/superposition

run_jsonlogic: FE_FEATURES += jsonlogic
run_jsonlogic: kill db frontend superposition_jsonlogic
	@$(CARGO_TARGET_DIR)/debug/superposition

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
	cd tests && bun test
	-@pkill -f $(CARGO_TARGET_DIR)/debug/superposition &

test_jsonlogic: WASM_PACK_MODE=--profiling
test_jsonlogic: FE_FEATURES += jsonlogic
test_jsonlogic: setup frontend superposition_jsonlogic
	cargo test --features=jsonlogic
	@echo "Running superposition"
	@$(CARGO_TARGET_DIR)/debug/superposition &
	@echo "Awaiting superposition boot..."
## FIXME Curl doesn't retry.
	@curl	--silent --retry 10 \
				--connect-timeout 2 \
				--retry-all-errors \
				'http://localhost:8080/health' 2>&1 > /dev/null
	cd tests && export JSONLOGIC_ENABLED=true && bun test
	-@pkill -f $(CARGO_TARGET_DIR)/debug/superposition &

## npm run test
## FIXME Broken as requires hardcoded 'org_id'. Current test setup doesn't create
## deterministic 'org_id'.
# cd $(SMITHY_CLIENT_DIR)/ts && npm ci && npm test

tailwind:
	cd crates/frontend && npx tailwindcss -i ./styles/tailwind.css -o ./pkg/style.css --watch

smithy-clean:
	rm -rf smithy/output

smithy-build:
	cd smithy && smithy build

smithy-clean-build: smithy-clean smithy-build

smithy-clients: smithy-build
## Moving the Java client like this as smithy publishes it as a java project.
## Probably want to use that to publish it ourselves in the future.
	rm -rf clients/java/sdk/src/main/java
	mkdir -p clients/java/sdk/src/main/java
	cp -r $(SMITHY_BUILD_SRC)/java-client-codegen/*\
				clients/java/sdk/src/main/java
	git restore clients/java/sdk/src/main/java/io/juspay/superposition/client/auth/BasicAuthIdentityResolver.java
	git restore clients/java/sdk/src/main/java/io/juspay/superposition/client/auth/BearerTokenIdentityResolver.java

	rm -rf clients/python/sdk
	mkdir -p clients/python/sdk
	cp -r $(SMITHY_BUILD_SRC)/python-client-codegen/*\
				clients/python/sdk
	git restore clients/python/sdk/superposition_sdk/auth_helpers.py
	git restore clients/python/sdk/AUTH_README.md

	rm -rf clients/haskell/sdk
	mkdir -p clients/haskell/sdk
	cp -r $(SMITHY_BUILD_SRC)/haskell-client-codegen/*\
				clients/haskell/sdk

	rm -rf clients/javascript/sdk
	mkdir -p clients/javascript/sdk
	git restore clients/javascript/sdk/README.md
	cp -r $(SMITHY_BUILD_SRC)/typescript-client-codegen/*\
				clients/javascript/sdk
	git restore clients/javascript/sdk/LICENSE

	rm -rf crates/superposition_sdk
	mkdir -p crates/superposition_sdk
	git restore crates/superposition_sdk/README.md
	git restore crates/superposition_sdk/CHANGELOG.md
	cp -r $(SMITHY_BUILD_SRC)/rust-client-codegen/*\
				crates/superposition_sdk

	@for d in $(SMITHY_BUILD_SRC)/*-client-codegen; do \
		[ -d "$$d" ] || continue; \
		[[ "$$d" =~ "java" || "$$d" =~ "haskell" || "$$d" =~ "python" || "$$d" =~ "typescript" || "$$d" =~ "rust" ]] && continue; \
		name=$$(basename "$$d" -client-codegen); \
		mkdir -p "$(SMITHY_CLIENT_DIR)/$$name"; \
		cp -r "$$d"/* "$(SMITHY_CLIENT_DIR)/$$name"; \
	done
	git restore clients/python/sdk/README.md
	git restore LICENSE
	git apply smithy/patches/*.patch

# API Documentation targets
smithy-api-docs: smithy-build
	rm -rf docs/docs/api
	mkdir -p docs/docs/api
	cp $(SMITHY_BUILD_SRC)/openapi/Superposition.openapi.json docs/docs/api/
	cd docs && npm ci && npm run openapi-docs

smithy-updates: smithy-clients smithy-api-docs

leptosfmt:
	leptosfmt $(LEPTOS_FMT_FLAGS) $(LEPTOS_PACKAGES)

# Note: Run make from the repository root for correct path exclusions!


lint-fix: LINT_FLAGS += --fix --allow-dirty --allow-staged
lint-fix: lint

check: FMT_FLAGS += --check
check: LEPTOS_FMT_FLAGS += --check
check: LINT_FLAGS += -- -Dwarnings
check: fmt leptosfmt lint


# Target to run cargo fmt on filtered packages
fmt:
	@echo "Running cargo fmt on packages: $(WORKSPACE_MEMBERS)"
	cargo fmt $(FMT_PACKAGE_FLAGS)

lint:
	cargo clippy $(LINT_FLAGS)


commit: check
	git commit $(COMMIT_FLAGS)

amend: COMMIT_FLAGS += --amend
amend: commit

amend-no-edit: COMMIT_FLAGS += --no-edit
amend-no-edit: amend

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
  $(error Unsupported OS type: $(OS))
endif
uniffi-bindings:
	cargo build --package superposition_core --lib --release
	cargo run --bin uniffi-bindgen generate --library $(CARGO_TARGET_DIR)/release/libsuperposition_core.$(LIB_EXTENSION) --language kotlin --out-dir clients/java/bindings/src/main/kotlin
	cargo run --bin uniffi-bindgen generate --library $(CARGO_TARGET_DIR)/release/libsuperposition_core.$(LIB_EXTENSION) --language python --out-dir clients/python/bindings/superposition_bindings
	git apply uniffi/patches/*.patch

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
	cd clients/java && ./gradlew :provider-sdk-tests:run
	-@pkill -f $(CARGO_TARGET_DIR)/debug/superposition

test-rust-provider: provider-template
	cargo test --package superposition_provider --test integration_test -- --nocapture --ignored
	-@pkill -f $(CARGO_TARGET_DIR)/debug/superposition
