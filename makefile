IMAGE_NAME ?= context-aware-config

build:
	docker build -f Dockerfile -t $(IMAGE_NAME):$(packageVersion) .

default: build
