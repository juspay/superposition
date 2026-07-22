"""HttpDataSource distinguishes "nothing changed" from "the call failed".

The SDK's deserializer treats every status >= 300 as an error and collapses it into an
UnknownApiError carrying only a message. This used to be caught wholesale and reported as
FetchResponse.not_modified(), so expired credentials and 500s looked like a successful 304
and silently froze the config. These tests pin the difference.

Run standalone: python test_http_data_source.py
"""

import asyncio
import json
import threading
from http.server import BaseHTTPRequestHandler, HTTPServer

from superposition_provider.http_data_source import HttpDataSource
from superposition_provider.types import SuperpositionOptions, TokenAuth

# Set per-test; the handler replies with whatever is here.
STATUS = 200
BODY = {}


def _reply(self):
    # A 304 carries no body, by definition.
    if STATUS == 304:
        self.send_response(304)
        self.end_headers()
        return
    payload = json.dumps(BODY).encode()
    self.send_response(STATUS)
    self.send_header("Content-Type", "application/json")
    self.send_header("Content-Length", str(len(payload)))
    self.end_headers()
    self.wfile.write(payload)


class _Handler(BaseHTTPRequestHandler):
    # The SDK posts to /config; answer whatever verb it uses.
    do_GET = do_POST = do_PUT = do_DELETE = _reply

    def log_message(self, *args):
        pass  # keep the test output readable


def _serve():
    server = HTTPServer(("127.0.0.1", 0), _Handler)
    threading.Thread(target=server.serve_forever, daemon=True).start()
    return server


def _fetch_config(server):
    # The SDK builds an aiohttp session eagerly, so the source must be created on the loop.
    async def run():
        source = HttpDataSource(SuperpositionOptions(
            endpoint=f"http://127.0.0.1:{server.server_address[1]}",
            auth=TokenAuth("test-token"),
            org_id="test-org",
            workspace_id="test-workspace",
        ))
        return await source.fetch_config(None)

    return asyncio.run(run())


def test_a_304_is_not_modified():
    global STATUS
    server = _serve()
    try:
        STATUS = 304
        response = _fetch_config(server)
        assert response.is_not_modified(), "304 should be NotModified"
        assert response.get_data() is None
    finally:
        server.shutdown()


def test_an_auth_failure_is_not_reported_as_not_modified():
    global STATUS, BODY
    server = _serve()
    try:
        STATUS, BODY = 401, {"message": "Unauthorized"}
        try:
            response = _fetch_config(server)
        except Exception:
            return  # the only acceptable outcome
        raise AssertionError(
            f"401 surfaced as {response!r} instead of raising — a caller would treat "
            "expired credentials as 'config unchanged' and serve stale flags forever"
        )
    finally:
        STATUS, BODY = 200, {}
        server.shutdown()


def test_a_server_error_is_not_reported_as_not_modified():
    global STATUS, BODY
    server = _serve()
    try:
        STATUS, BODY = 500, {"message": "boom"}
        try:
            response = _fetch_config(server)
        except Exception:
            return
        raise AssertionError(f"500 surfaced as {response!r} instead of raising")
    finally:
        STATUS, BODY = 200, {}
        server.shutdown()


if __name__ == "__main__":
    cases = [v for k, v in sorted(globals().items()) if k.startswith("test_")]
    for case in cases:
        case()
        print(f"ok  {case.__name__}")
    print(f"\n{len(cases)} passed")
