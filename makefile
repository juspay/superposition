IMAGE_NAME ?= context-aware-config
DOCKER_DNS ?= localhost
TENANT ?= dev
SHELL := /usr/bin/env bash
FEATURES ?= ssr
COMPOSE_CMD = podman-compose
COMPOSE_UP = $(COMPOSE_CMD) up -d
COMPOSE_DOWN = $(COMPOSE_CMD) down
DB_UP := $(shell podman container ls | grep -q 'superposition_postgres' && echo 1 || echo 0)
LOCALSTACK_UP := $(shell podman container ls | grep -q 'superposition_localstack' && echo 1 || echo 0)
TIMEOUT ?= 30

.PHONY:
	db-init
	setup
	kill
	run
	ci-setup
	test
	cac

db-init:
	diesel migration run --locked-schema --config-file=crates/superposition_types/src/cac/diesel.toml
	-diesel migration run --locked-schema --config-file=crates/superposition_types/src/experimentation/diesel.toml

cleanup:
	-podman rm -f $$(podman container ls --filter name=^context-aware-config -a -q)
	-podman rmi -f $$(podman images | grep context-aware-config-postgres | cut -f 10 -d " ")

cac-migration: cleanup db
	cp .env.example .env
	diesel migration run --locked-schema --config-file=crates/superposition_types/src/cac/diesel.toml
	$(COMPOSE_DOWN)

	exp-migration: cleanup db
	cp .env.example .env
	--diesel migration run --locked-schema --config-file=crates/superposition_types/src/experimentation/diesel.toml
	$(COMPOSE_DOWN)

migration: cac-migration exp-migration

legacy_db_setup:
	grep 'DATABASE_URL=' .env | sed -e 's/DATABASE_URL=//' | xargs ./scripts/legacy-db-setup.sh

tenant:
	grep 'DATABASE_URL=' .env | sed -e 's/DATABASE_URL=//' | xargs ./scripts/create-tenant.sh -t $(TENANT) -d

db:
ifeq ($(DB_UP),0)
	@echo "Booting postgres..."
## Filtering some output to remove spurious error logs.
	@$(COMPOSE_UP) postgres 2>&1 | grep -v "already exists"
endif
	@podman wait --condition=healthy "superposition_postgres" > /dev/null
	@echo "DB Up!"

localstack:
ifeq ($(LOCALSTACK_UP),0)
	@echo "Booting localstack..."
## Filtering some output to remove spurious error logs.
	@$(COMPOSE_UP) localstack 2>&1 | grep -v "already exists"
endif
	@podman wait --condition=healthy "superposition_localstack" > /dev/null
	@echo "Localstack Up!"

infra: db localstack

env-setup: infra
	npm ci
	cp .env.example .env

test-tenant: TENANT = 'test'
test-tenant: tenant

dev-tenant: TENANT = 'dev'
dev-tenant: tenant

ci-setup: env-setup test-tenant run

setup: env-setup

kill:
	-pkill -f target/debug/superposition &

get-password:
	export DB_PASSWORD=`./podman-compose/localstack/get_db_password.sh` && echo $$DB_PASSWORD

superposition:
	cargo run --color always --bin superposition --no-default-features --features=$(FEATURES)

superposition-example:
	cargo run --bin cac-demo-app

superposition_legacy:
	cargo run --color always --bin superposition --no-default-features --features='ssr superposition_types/disable_db_data_validation context_aware_config/disable_db_data_validation experimentation_platform/disable_db_data_validation'

superposition_dev:
# export DB_PASSWORD=`./podman-compose/localstack/get_db_password.sh`
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

run: kill infra frontend superposition

run_legacy: kill infra build superposition_legacy

test:
	cargo test
	@timeout=$(TIMEOUT); \
  while ! nc -z localhost 8080 && [ $$timeout -gt 0 ]; do \
		sleep 1; \
		((timeout--)); \
  done; \
	if [ $$timeout -eq 0 ]; then \
		echo "Timedout waiting for service to recieve connections, is superposition running?" && exit 1; \
	fi
	npm run test

tailwind:
	cd crates/frontend && npx tailwindcss -i ./styles/tailwind.css -o ./pkg/style.css --watch

default: dev-build
