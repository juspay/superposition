IMAGE_NAME ?= context-aware-config
DOCKER_DNS ?= localhost
TENANT ?= dev
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

cleanup:
	-docker rm -f $$(docker container ls --filter name=^context-aware-config -a -q)
	-docker rmi -f $$(docker images | grep context-aware-config-postgres | cut -f 10 -d " ")

db-init:
	diesel migration run --locked-schema --config-file=crates/context-aware-config/diesel.toml
	-diesel migration run --locked-schema --config-file=crates/experimentation-platform/diesel.toml

cac-migration: cleanup
	docker-compose up -d postgres
	cp .env.example .env
	sed -i 's/dockerdns/$(DOCKER_DNS)/g' ./.env
	while ! make validate-psql-connection; \
		do echo "waiting for postgres bootup"; \
		sleep 0.5; \
		done
	diesel migration run --config-file=crates/context-aware-config/diesel.toml
	docker-compose down

exp-migration: cleanup
	docker-compose up -d postgres
	cp .env.example .env
	sed -i 's/dockerdns/$(DOCKER_DNS)/g' ./.env
	while ! make validate-psql-connection; \
		do echo "waiting for postgres bootup"; \
		sleep 0.5; \
		done
	diesel migration run --config-file=crates/experimentation-platform/diesel.toml
	docker-compose down

migration: cac-migration exp-migration

legacy_db_setup:
	grep 'DATABASE_URL=' .env | sed -e 's/DATABASE_URL=//' | xargs ./scripts/legacy-db-setup.sh

tenant:
	grep 'DATABASE_URL=' .env | sed -e 's/DATABASE_URL=//' | xargs ./scripts/create-tenant.sh $(TENANT)

validate-aws-connection:
	aws --endpoint-url=http://$(DOCKER_DNS):4566 --region=ap-south-1 sts get-caller-identity

validate-psql-connection:
	pg_isready -h $(DOCKER_DNS) -p 5432


env-setup:
	docker-compose up -d postgres localstack
	cp .env.example .env
	sed -i 's/dockerdns/$(DOCKER_DNS)/g' ./.env
	while ! make validate-psql-connection validate-aws-connection; \
		do echo "waiting for postgres, localstack bootup"; \
		sleep 0.5; \
		done

test-tenant:
	make tenant TENANT='test'

dev-tenant:
	make tenant TENANT='dev'

ci-setup: env-setup test-tenant
	npm ci --loglevel=error
	make run -e DOCKER_DNS=$(DOCKER_DNS) 2>&1 | tee test_logs &
	while ! grep -q "starting in Actix" test_logs; \
		do echo "ci-test: waiting for bootup..." && sleep 4; \
		done
	# NOTE: `make db-init` finally starts a postgres container and runs all the migrations with locked-schema option
	# to prevent update of schema.rs for both cac and experimentation.
	# NOTE: The container spinned-up here is the actual container being used in development
	echo setup completed successfully!!!

setup: migration env-setup test-tenant dev-tenant
	# NOTE: `make migration` is being used to run the migrations for cac and experimentation in isolation,
	# otherwise the tables and types of cac and experimentation spill into each others schema.rs
	# NOTE: The container spinned up are stopped and removed after the work is done.

kill:
	-pkill -f target/debug/context-aware-config &

get-password:
	export DB_PASSWORD=`./docker-compose/localstack/get_db_password.sh` && echo $$DB_PASSWORD

cac:
	export DB_PASSWORD=`./docker-compose/localstack/get_db_password.sh`; \
	cargo run --color always --bin context-aware-config --no-default-features --features=ssr

frontend:
	cd crates/frontend && \
		wasm-pack build --target=web --debug --no-default-features --features=hydrate
	cd crates/frontend && \
		npx tailwindcss -i ./styles/tailwind.css -o ./pkg/style.css
	-rm -rf target/site
	mkdir target/site && mkdir target/site/pkg
	mv crates/frontend/pkg target/site/
	cp -a crates/frontend/assets/. target/site/

backend:
	cargo build --color always

build: frontend backend

run: kill build
	while ! make validate-psql-connection validate-aws-connection; \
		do echo "waiting for postgres, localstack bootup"; \
		sleep 0.5; \
		done
	cp .env.example .env
	sed -i 's/dockerdns/$(DOCKER_DNS)/g' ./.env
	make cac -e DOCKER_DNS=$(DOCKER_DNS)

ci-test: cleanup ci-setup
	cargo test
	npm run test
	rm test_cac.sql
	rm test_experimentation.sql

ci-build:
	docker buildx build --ssh default=$(SSH_AUTH_SOCK) \
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

tailwind:
	cd crates/frontend && npx tailwindcss -i ./styles/tailwind.css -o ./pkg/style.css --watch

default: dev-build