# Superposition Provider - Python Examples

This directory contains Python examples demonstrating the Superposition Python provider library, showing various configuration loading and refreshing strategies.

## Examples

### 1. `local_file_example.py` - Load from Local File

**Purpose:** Load configuration from a local TOML file with on-demand refresh.

**Key Features:**

- File-based configuration loading
- On-demand refresh strategy (loads once, refreshes only when data becomes stale)
- Simple evaluation context with dimensions

**Usage:**

```bash
python local_file_example.py
```

**Output:**

- Prints resolved configuration
- Shows specific flag values (timeout, currency)

### 2. `local_file_watch_example.py` - Watch File for Changes

**Purpose:** Watch a local configuration file and automatically reload when it changes.

**Key Features:**

- File watching capability
- Automatic reload on file modification
- Periodic polling to display current values

**Usage:**

```bash
# Terminal 1: Run the example
python local_file_watch_example.py

# Terminal 2: Edit config.toml
# The running example will display updated values within 2 seconds
```

**Output:**

- Prints updated configuration every 2 seconds
- Shows when changes are detected

### 3. `local_http_example.py` - Load from HTTP with Polling

**Purpose:** Fetch configuration from Superposition API with periodic polling.

**Prerequisites:**

- Superposition server running at `http://localhost:8080`

**Key Features:**

- HTTP-based configuration fetching
- Polling refresh strategy (checks for updates every 30 seconds)
- Experiment variant resolution

**Usage:**

```bash
python local_http_example.py
```

**Output:**

- Prints resolved configuration from API
- Shows applicable experiment variants

### 4. `local_with_fallback_example.py` - Primary + Fallback Sources

**Purpose:** Resilient configuration with HTTP primary and local file fallback.

**Prerequisites:**

- Superposition server (optional) at `http://localhost:8080`
- `config.toml` file as fallback

**Key Features:**

- Primary data source (HTTP)
- Automatic fallback to local file if primary fails
- Polling strategy with 10-second interval
- Demonstrates resilience patterns

**Usage:**

```bash
python local_with_fallback_example.py
```

**Output:**

- Prints resolved configuration every 5 seconds
- Uses HTTP when available, falls back to file if needed

### 5. `polling_example.py` - Configurable Polling via Environment Variables

**Purpose:** Production-ready example with flexible environment variable configuration.

**Prerequisites:**

- Superposition server (optional) at `http://localhost:8080`

**Key Features:**

- All settings configurable via environment variables
- Comprehensive logging
- Tracks value changes and logs when they happen
- Suitable for long-running monitoring

**Configuration via Environment Variables:**

```bash
export SUPERPOSITION_ENDPOINT="http://localhost:8080"      # API endpoint
export SUPERPOSITION_TOKEN="token"                         # API token
export SUPERPOSITION_ORG_ID="localorg"                     # Organization ID
export SUPERPOSITION_WORKSPACE="dev"                       # Workspace ID
export POLL_INTERVAL="10"                                  # Poll frequency (seconds)
export PRINT_INTERVAL="5"                                  # Print frequency (seconds)
export CONFIG_KEY="max_connections"                        # Key to watch
```

**Usage:**

```bash
# With defaults
python polling_example.py

# With custom settings
POLL_INTERVAL=5 PRINT_INTERVAL=3 CONFIG_KEY=timeout python polling_example.py

# Full custom configuration
export SUPERPOSITION_ENDPOINT="http://my-server:8080"
export SUPERPOSITION_ORG_ID="myorg"
export SUPERPOSITION_WORKSPACE="production"
python polling_example.py
```

**Output:**

- Logs configuration changes with timestamps
- Shows which values have been updated
- Tracks polling cycles

## Sample Configuration (config.toml)

The `config.toml` file contains sample feature flag configuration:

```toml
[default-configs]
timeout = { value = 30, schema = { type = "integer" } }
currency = { value = "Rupee", schema = { type = "string", enum = [...] } }
price = { value = 10000, schema = { type = "integer", minimum = 0 } }

[dimensions]
os = { position = 1, schema = { type = "string" } }
city = { position = 2, schema = { type = "string" } }

[[overrides]]
_context_ = { os = "linux" }
timeout = 45

[[overrides]]
_context_ = { city = "Boston" }
currency = "Dollar"

[[overrides]]
_context_ = { city = "Berlin" }
currency = "Euro"
```

## Running the Examples

### Setup

1. Install dependencies:

```bash
pip install superposition-sdk openfeature
```

2. For HTTP examples, ensure Superposition server is running:

```bash
# Local development server
docker run -p 8080:8080 superposition:latest
```

### Run Individual Examples

```bash
# Local file example
python local_file_example.py

# File watching example
python local_file_watch_example.py

# HTTP polling example
python local_http_example.py

# Fallback example
python local_with_fallback_example.py

# Environment-based polling
POLL_INTERVAL=5 python polling_example.py
```

## Refresh Strategies

The examples demonstrate different refresh strategies:

| Strategy     | Use Case                          | Example                                       |
| ------------ | --------------------------------- | --------------------------------------------- |
| **OnDemand** | Load once, refresh when stale     | `local_file_example.py`                       |
| **Watch**    | React to file changes immediately | `local_file_watch_example.py`                 |
| **Polling**  | Regular interval checks           | `local_http_example.py`, `polling_example.py` |
| **Manual**   | Caller-driven refresh             | (Custom implementation)                       |

## Evaluation Contexts

Examples show how to create evaluation contexts with:

- **Targeting Key:** Identifies the user/entity
- **Attributes:** Dimensional data for flag evaluation

```python
context = EvaluationContext(
    targeting_key="user-1234",
    attributes={
        "os": "linux",
        "city": "Boston",
        "source": "mobile_app",
    },
)
```

## Error Handling

All examples implement proper error handling:

- Graceful shutdown with `Ctrl-C`
- Connection error resilience
- Logging for debugging

## Next Steps

- Explore different refresh strategies for your use case
- Combine with OpenFeature client for standardized flag evaluation
- Implement custom data sources for specialized backends
- Monitor performance metrics from provider statistics

## Troubleshooting

**Provider won't initialize:**

- Check that required dependencies are installed
- For HTTP examples, verify Superposition server is reachable

**Config not updating:**

- Verify file is being modified (watch example)
- Check polling interval is not too long
- Review logs for error messages

**Flags not resolved:**

- Ensure evaluation context has required dimensions
- Check config file syntax for TOML errors
