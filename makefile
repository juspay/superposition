IMAGE_NAME ?= context-aware-config
DOCKER_DNS ?= localhost
SHELL := /usr/bin/env bash

.PHONY:
	db-init
	setup
	kill
	run
	ci-test
	ci-build
	ci-push
	registry-login
	validate-aws-connection
	validate-psql-connection
	cac

db-init:
	diesel migration run --config-file=crates/context-aware-config/diesel.toml
	diesel migration run --config-file=crates/experimentation-platform/diesel.toml

validate-aws-connection:
	aws --endpoint-url=http://$(DOCKER_DNS):4566 --region=ap-south-1 sts get-caller-identity

validate-psql-connection:
	pg_isready -h $(DOCKER_DNS) -p 5432

setup:
	docker-compose up -d postgres localstack
	cp .env.example .env
	sed -i 's/dockerdns/$(DOCKER_DNS)/g' ./.env
	while ! make validate-psql-connection validate-aws-connection; \
		do echo "waiting for postgres, localstack bootup"; \
		sleep 0.5; \
		done
	make db-init

kill:
	-pkill -f target/debug/context-aware-config &

cac:
	export DB_PASSWORD=`./docker-compose/localstack/get_db_password.sh`; \
	cargo run --color always --bin context-aware-config

run:
	-make kill
	cargo build --color always
	make setup
	make cac -e DOCKER_DNS=$(DOCKER_DNS)

ci-test:
	-docker rm -f $$(docker container ls --filter name=^context-aware-config -a -q)
	-docker rmi -f $$(docker images | grep context-aware-config-postgres | cut -f 10 -d " ")
	npm ci --loglevel=error
	make run -e DOCKER_DNS=$(DOCKER_DNS) 2>&1 | tee test_logs &
	while ! grep -q "starting in Actix" test_logs; \
		do echo "ci-test: waiting for bootup..." && sleep 4; \
		done
	npm run test

ci-build:
	docker build \
	    -t $(IMAGE_NAME):$(VERSION) \
			--build-arg "CONTEXT_AWARE_CONFIG_VERSION=${VERSION}" \
			--build-arg "SOURCE_COMMIT=${SOURCE_COMMIT}" \
			.

ci-push: registry-login
	docker tag $(IMAGE_NAME):$(VERSION) $(REGISTRY_HOST)/$(IMAGE_NAME):$(VERSION)
	docker push $(REGISTRY_HOST)/$(IMAGE_NAME):$(VERSION)

registry-login:
	aws ecr get-login-password --region $(REGION) | \
	docker login \
	    --username AWS \
	    --password-stdin $(REGISTRY_HOST)


default: dev-build
