

.PHONY: test coverage run run-ex build-ex


test: test-cac test-expt

test-cac:
	zig test -I../../headers -lc  src/cac.zig ../../target/debug/libcac_client.so

test-expt:
	zig test -I../../headers -lc  src/expt.zig ../../target/debug/libexperimentation_client.so


run-ex: build-ex
	./zig-out/bin/cli

build-ex:
	zig build

