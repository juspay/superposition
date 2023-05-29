IMAGE_NAME ?= context-aware-config

SHELL := /bin/bash

build:
	cargo build

ci-test:
## Un-comment once agent has 'cargo' & 'libpq5'.
	#cargo build

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
	pkill -f context-aware-config &
	touch ./docker-compose/localstack/export_cyphers.sh
	cargo build --color always
	docker-compose up -d postgres localstack
	source ./docker-compose/localstack/export_cyphers.sh && \
		cargo run --color always

default: build
