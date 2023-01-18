IMAGE_NAME ?= context-aware-config
packageVersion ?= 0.0.1

build:
	docker build -f Dockerfile -t $(IMAGE_NAME):$(packageVersion) .

default: build
