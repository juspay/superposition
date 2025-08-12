## Superposition SDK

Superposition SDK is a Python client for the Superposition platform, designed to facilitate programmatic integration of all Superposition's API capabilities in Python applications. Read the complete documentation at [Superposition SDK Documentation](https://juspay.io/superposition/docs).

## Installation

Install the Superposition SDK using pip:

```bash
pip install superposition-sdk
```

## Initialization

```python
from superposition_sdk.client import Config, Superposition
client = Superposition(Config(endpoint_uri="http://localhost:8080"))
```

## Usage

The SDK provides commands for every API call that Superposition supports. Below is an example of how to use the SDK to list default configs.

```python
import asyncio
from superposition_sdk.client import Config, ListDefaultConfigsInput, Superposition
from pprint import pprint

async def list_configs():
    client = Superposition(Config(endpoint_uri="http://localhost:8080"))
    list_configs = ListDefaultConfigsInput(workspace_id="upi", org_id="orgid162145664241766405", all=True)
    response = await client.list_default_configs(list_configs)
    pprint(response)

asyncio.run(list_configs())
```
