import asyncio
import logging
import os
import sys

_PYTHON_DIR = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.path.insert(0, os.path.join(_PYTHON_DIR, "sdk"))
sys.path.insert(0, os.path.join(_PYTHON_DIR, "bindings"))

from openfeature.evaluation_context import EvaluationContext
from superposition_provider import LocalResolutionProvider, HttpDataSource
from superposition_provider.types import SuperpositionOptions, SseStrategy

logging.basicConfig(level=logging.INFO, format="%(asctime)s  %(message)s")
logging.getLogger("superposition_sdk").setLevel(logging.CRITICAL)
logging.getLogger("superposition_provider").setLevel(logging.WARNING)
log = logging.getLogger(__name__)


ENDPOINT  = os.environ.get("SUPERPOSITION_ENDPOINT", "http://localhost:8080")
TOKEN     = os.environ.get("SUPERPOSITION_TOKEN", "token")
ORG       = os.environ.get("SUPERPOSITION_ORG_ID", "localorg")
WORKSPACE = os.environ.get("SUPERPOSITION_WORKSPACE", "dev")


async def main():
    loop = asyncio.get_event_loop()
    _orig = loop.default_exception_handler
    loop.set_exception_handler(
        lambda l, ctx: None if ctx.get("message") == "Unclosed client session" else _orig(ctx)
    )

    options = SuperpositionOptions(endpoint=ENDPOINT, token=TOKEN, org_id=ORG, workspace_id=WORKSPACE)

    def on_config_change(before, after):
        for key, value in after.items():
            if before.get(key) != value:
                log.info(f"[UPDATE] {key}: {before.get(key)!r} -> {value!r}")

    provider = LocalResolutionProvider(
        primary_source=HttpDataSource(options),
        refresh_strategy=SseStrategy(superposition_options=options, reconnect_delay=5),
        on_config_change=on_config_change,
    )

    await provider.initialize(EvaluationContext())
    log.info(f"Initial config: {provider.resolve_all_features(EvaluationContext())}")
    log.info("Listening for SSE config changes (Ctrl-C to stop)")

    try:
        await asyncio.Event().wait()
    except KeyboardInterrupt:
        pass
    finally:
        await provider.shutdown()


if __name__ == "__main__":
    asyncio.run(main())
