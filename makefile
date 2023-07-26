IMAGE_NAME ?= context-aware-config

SHELL := /usr/bin/env bash

setup:
	touch ./docker-compose/localstack/export_cyphers.sh
	docker-compose up -d
	cp .env.example .env
	sleep 10 #TODO move this sleep to aws cli list-keys command instead

db-init:
	diesel migration run --config-file=crates/context-aware-config/diesel.toml
	diesel migration run --config-file=crates/experimentation-platform/diesel.toml

build:
	cargo build --color always

ci-test:
	npm ci --loglevel=error
	-docker rm -f $$(docker container ls --filter name=^context-aware-config -q)
	make run 2>&1 | tee test_logs &
	while ! grep -q "starting in Actix" test_logs; do echo "waiting for bootup..." && sleep 2; done
	npm run test

ci-build:
	docker build -t $(IMAGE_NAME):$(VERSION) .

ci-push: registry-login
	docker tag $(IMAGE_NAME):$(VERSION) $(REGISTRY_HOST)/$(IMAGE_NAME):$(VERSION)
	docker push $(REGISTRY_HOST)/$(IMAGE_NAME):$(VERSION)

registry-login:
	aws ecr get-login-password --region $(REGION) | \
	docker login \
	    --username AWS \
	    --password-stdin $(REGISTRY_HOST)

cac:
	source ./docker-compose/localstack/export_cyphers.sh && \
		cargo run --package context-aware-config --color always

example:
	cargo run --package example

ls-packages:
	cargo run --package

kill:
	pkill -f target/debug/context-aware-config &

run: kill setup db-init build cac

default: build
