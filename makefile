IMAGE_NAME ?= context-aware-config

SHELL := /usr/bin/env bash

build:
	cargo build

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

run:
	pkill -f target/debug/context-aware-config &
	touch ./docker-compose/localstack/export_cyphers.sh
	docker-compose up -d postgres localstack
	cp .env.example .env
	#NOTE need to sleep here because locastack takes some time to internally
	#populate the kms keyId
	sleep 10 #TODO move this sleep to aws cli list-keys command instead
	cargo build --color always
	diesel migration run --config-file=crates/context-aware-config/diesel.toml
	source ./docker-compose/localstack/export_cyphers.sh && \
		cargo run --color always

default: build
