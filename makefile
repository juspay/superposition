IMAGE_NAME ?= context-aware-config
DOCKER_DNS ?= localhost
TENANT ?= dev
SHELL := /usr/bin/env bash
FEATURES ?= ssr
CARGO_FLAGS := --color always --no-default-features
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

.PHONY:
	db-init
	setup
	kill
	run
	ci-test
	validate-aws-connection
	validate-psql-connection
	cac

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
	diesel migration run --locked-schema --config-file=crates/superposition_types/src/database/diesel.toml

cleanup:
	-$(DOCKER) rm -f $$($(DOCKER) container ls --filter name=^superposition -a -q)
	-$(DOCKER) rmi -f $$($(DOCKER) images | grep $(DB_CONTAINER_NAME) | cut -f 10 -d " ")

migration: cleanup db
	diesel migration run --locked-schema --config-file=crates/superposition_types/src/database/diesel.toml
	$(COMPOSE) down

legacy_db_setup:
	grep 'DATABASE_URL=' .env | sed -e 's/DATABASE_URL=//' | xargs ./scripts/legacy-db-setup.sh

tenant:
	grep 'DATABASE_URL=' .env | sed -e 's/DATABASE_URL=//' | xargs ./scripts/create-tenant.sh -t $(TENANT) -d

validate-aws-connection:
	aws --no-cli-pager --endpoint-url=http://$(DOCKER_DNS):4566 --region=ap-south-1 sts get-caller-identity

validate-psql-connection:
	pg_isready -h $(DOCKER_DNS) -p 5432


test-tenant: TENANT = 'test'
test-tenant: tenant

dev-tenant: TENANT = 'dev'
dev-tenant: tenant

SETUP_DEPS = env-file db localstack
ifdef CI
	SETUP_DEPS += test-tenant
endif
setup: $(SETUP_DEPS)
	npm ci

kill:
	-@pkill -f target/debug/superposition &

get-password:
	export DB_PASSWORD=`./docker-compose/localstack/get_db_password.sh` && echo $$DB_PASSWORD

superposition: CARGO_FLAGS += --features=$(FEATURES)
superposition:
	cargo build $(CARGO_FLAGS) --bin superposition

superposition-example:
	cargo run --bin cac-demo-app

superposition_legacy: CARGO_FLAGS += --features='$(FEATURES)
superposition_legacy: CARGO_FLAGS += superposition_types/disable_db_data_validation
superposition_legacy: CARGO_FLAGS += context_aware_config/disable_db_data_validation
superposition_legacy: CARGO_FLAGS += experimentation_platform/disable_db_data_validation'
superposition_legacy:
	cargo build $(CARGO_FLAGS) --bin superposition

superposition_dev: CARGO_FLAGS += --features=$(FEATURES)
superposition_dev:
	# export DB_PASSWORD=`./docker-compose/localstack/get_db_password.sh`
	cargo watch -x 'run $(CARGO_FLAGS) --bin superposition'


frontend:
	cd crates/frontend && \
		wasm-pack build --target web $(WASM_PACK_MODE) --no-default-features --features hydrate
	cd crates/frontend && \
		npx tailwindcss -i ./styles/tailwind.css -o ./pkg/style.css
	-rm -rf target/site
	mkdir -p target/site/pkg
	mv crates/frontend/pkg target/site/
	cp -a crates/frontend/assets/. target/site/

backend:
	-rm -rf target/node_modules
	npm --prefix ./crates/context_aware_config/ ci
	mv crates/context_aware_config/node_modules target/
	cargo build --color always

build: frontend backend

run: kill db localstack frontend superposition
	@./target/debug/superposition

run_legacy: kill build db localstack superposition_legacy
	@./target/debug/superposition

test: WASM_PACK_MODE=--profiling
test: setup frontend superposition
	cargo test
	@echo "Running superposition"
	@./target/debug/superposition &
	@echo "Awaiting superposition boot..."
	@timeout 20s bash -c \
		"while ! curl --silent 'http://localhost:8080/health' 2>&1 > /dev/null; do sleep 0.5; done"
	npm run test

tailwind:
	cd crates/frontend && npx tailwindcss -i ./styles/tailwind.css -o ./pkg/style.css --watch

default: dev-build
