

.PHONY: test coverage run


test: test-cac

test-cac:
	zig test -I../../headers -lc src/cac.zig ../../target/debug/libcac_client.so
