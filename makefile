IMAGE_NAME ?= context-aware-config
DOCKER_DNS ?= localhost
TENANT ?= dev
SHELL := /usr/bin/env bash
FEATURES ?= ssr
HAS_DOCKER := $(shell command -v docker > /dev/null; echo $$?)
HAS_PODMAN := $(shell command -v podman > /dev/null; echo $$?)

ifeq ($(shell test -f .env; echo $$?),1)
$(shell cp .env.example .env)
endif

ifeq ($(HAS_DOCKER),0)
  DOCKER := docker
else ifeq ($(HAS_PODMAN),0)
  DOCKER := podman
else
	$(error "No docker or podman found, please install one of them.")
endif

COMPOSE := $(DOCKER) compose

.PHONY:
	db-init
	setup
	kill
	run
	ci-test
	validate-aws-connection
	validate-psql-connection
	cac

db:
ifndef CI
	$(COMPOSE) up -d postgres
else
	@echo "Skipping postgres setup in CI."
endif
	@echo "Verifying postgres readiness..."
	@while ! pg_isready -h $(DOCKER_DNS) -p 5432; do sleep 0.5; done

localstack:
ifndef CI
	$(COMPOSE) up -d localstack
else
	@echo "Skipping localstack setup in CI."
endif
	@echo "Verifying localstack readiness..."
	@while ! aws --no-cli-pager --endpoint-url=http://$(DOCKER_DNS):4566 --region=ap-south-1 sts get-caller-identity; do \
		sleep 0.5; \
	done

db-init:
	diesel migration run --locked-schema --config-file=crates/superposition_types/src/database/diesel.toml

cleanup:
	-$(DOCKER) rm -f $$($(DOCKER) container ls --filter name=^context-aware-config -a -q)
	-$(DOCKER) rmi -f $$($(DOCKER) images | grep context-aware-config-postgres | cut -f 10 -d " ")

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

SETUP_DEPS = db localstack
ifdef CI
	SETUP_DEPS += test-tenant
endif
setup: $(SETUP_DEPS)
	npm ci

kill:
	-pkill -f target/debug/superposition &

get-password:
	export DB_PASSWORD=`./docker-compose/localstack/get_db_password.sh` && echo $$DB_PASSWORD

superposition:
	cargo run --color always --bin superposition --no-default-features --features=$(FEATURES)

superposition-example:
	cargo run --bin cac-demo-app

superposition_legacy:
	cargo run --color always --bin superposition --no-default-features --features='ssr superposition_types/disable_db_data_validation context_aware_config/disable_db_data_validation experimentation_platform/disable_db_data_validation'

superposition_dev:
	# export DB_PASSWORD=`./docker-compose/localstack/get_db_password.sh`
	cargo watch -x 'run --color always --bin superposition --no-default-features --features=$(FEATURES)'


frontend:
	cd crates/frontend && \
		wasm-pack build --target web --dev --no-default-features --features hydrate
	cd crates/frontend && \
		npx tailwindcss -i ./styles/tailwind.css -o ./pkg/style.css
	-rm -rf target/site
	mkdir target/site && mkdir target/site/pkg
	mv crates/frontend/pkg target/site/
	cp -a crates/frontend/assets/. target/site/

backend:
	-rm -rf target/node_modules
	npm --prefix ./crates/context_aware_config/ ci
	mv crates/context_aware_config/node_modules target/
	cargo build --color always

build: frontend backend

RUN_DEPS =
ifdef CI
		RUN_DEPS += setup
endif
RUN_DEPS += kill frontend superposition
run: $(RUN_DEPS)

run_legacy: kill build db localstack superposition_legacy

test:
	cargo test
	npm ci
	@echo "Awaiting superposition boot..."
	@timeout 200s bash -c "while ! curl --silent 'http://localhost:8080/health' 2>&1 > /dev/null; do sleep 0.5; done"
	npm run test

tailwind:
	cd crates/frontend && npx tailwindcss -i ./styles/tailwind.css -o ./pkg/style.css --watch

default: dev-build
