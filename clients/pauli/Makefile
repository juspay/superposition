

.PHONY: test coverage run


test: test-cac test-expt

test-cac:
	zig test -I../../headers -lc  src/cac.zig ../../target/debug/libcac_client.so

test-expt:
	zig test -I../../headers -lc  src/expt.zig ../../target/debug/libexperimentation_client.so
