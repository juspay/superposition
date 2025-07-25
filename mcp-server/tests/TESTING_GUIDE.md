# Superposition MCP Server Testing Guide

This guide provides comprehensive information for testing the Superposition MCP (Model Context Protocol) server implementation.

## Quick Start

```bash
# Install test dependencies
pip install -r tests/requirements.txt

# Check test environment
./tests/test_runner.py --check

# Run all tests
./tests/test_runner.py --all

# Run specific test categories
./tests/test_runner.py --unit --cac        # CAC unit tests
./tests/test_runner.py --integration       # Integration tests
./tests/test_runner.py --error            # Error handling tests
```

## Test Architecture

The test suite is organized into several layers, each testing different aspects of the MCP server:

### 1. Unit Tests
Test individual tool implementations in isolation using mocked dependencies.

**CAC Tools (`test_cac_tools.py`)**
- Configuration management (get, set, delete, list)
- Context management (create, get, update, delete, list)
- Input validation and error handling

**Experimentation Tools (`test_experimentation_tools.py`)**
- Experiment lifecycle (create, get, update, conclude)
- Experiment control (ramp, pause, resume)
- Experiment groups (create, get, list)
- Variant validation and weight distribution

### 2. Integration Tests
Test complete MCP server functionality with tool registration and protocol compliance.

**Server Integration (`test_mcp_server.py`)**
- Server startup and shutdown
- Tool discovery and registration
- End-to-end workflows
- Concurrent request handling
- Protocol compliance verification

### 3. Error Handling Tests
Test various failure scenarios and edge cases.

**Error Scenarios (`test_error_handling.py`)**
- Network failures and timeouts
- Authentication and authorization errors
- Input validation failures
- Rate limiting scenarios
- Unicode and special character handling
- Large payload handling

## Test Infrastructure

### Mock Framework

The test suite includes a comprehensive mock framework that simulates the Superposition SDK and MCP server:

**MockSuperpositionClient**
```python
# Create a mock client with configurable failure modes
client = MockSuperpositionClient(should_fail=False)

# Configure for specific error scenarios
client.set_failure_mode(True, "network")  # Simulate network failures
client.set_failure_mode(True, "auth")     # Simulate auth failures
client.set_failure_mode(True, "validation")  # Simulate validation errors
```

**MockMCPServer**
```python
# Create and configure a mock MCP server
server = MockMCPServer()
server.register_tool('get_config', get_config_handler)
await server.start()

# Test tool calls
result = await server.call_tool('get_config', {'key': 'test'})
```

### Test Data Generation

The mock framework includes utilities for generating test data:

```python
# Generate mock configuration data
config_data = MockDataGenerator.generate_config_data(
    key='feature_flag',
    value={'enabled': True, 'rollout': 50}
)

# Generate mock experiment data
experiment_data = MockDataGenerator.generate_experiment_data(
    name='ab_test'
)
```

## Testing Strategies

### 1. Tool Validation Testing

Each tool is tested for:
- **Happy Path**: Normal operation with valid inputs
- **Error Scenarios**: Various failure modes and error conditions
- **Input Validation**: Schema validation and edge cases
- **Output Format**: Consistent response structure

Example:
```python
@pytest.mark.asyncio
async def test_get_config_success(self, mock_client):
    """Test successful configuration retrieval."""
    # Setup
    mock_client.configs["test:feature"] = {"enabled": True}
    
    # Execute
    result = await get_config_tool({
        'key': 'feature',
        'context': {'user_id': 'test_user'}
    })
    
    # Verify
    assert result['status'] == 'success'
    assert result['data']['value'] == {"enabled": True}
```

### 2. End-to-End Workflow Testing

Integration tests verify complete workflows:

```python
@pytest.mark.asyncio
async def test_config_lifecycle(self, configured_server):
    """Test complete configuration lifecycle."""
    # Create config
    create_result = await configured_server.call_tool('set_config', {
        'key': 'test_feature',
        'value': {'enabled': True}
    })
    assert create_result['status'] == 'success'
    
    # Retrieve config
    get_result = await configured_server.call_tool('get_config', {
        'key': 'test_feature'
    })
    assert get_result['status'] == 'success'
    
    # Delete config
    delete_result = await configured_server.call_tool('delete_config', {
        'key': 'test_feature'
    })
    assert delete_result['status'] == 'success'
```

### 3. Error Simulation Testing

Tests verify proper error handling:

```python
@pytest.mark.asyncio
async def test_network_failure_handling(self, failing_client):
    """Test handling of network failures."""
    failing_client.set_failure_mode(True, "network")
    
    result = await network_sensitive_tool({'key': 'test'})
    
    assert result['status'] == 'error'
    assert result['error'] == 'NetworkError'
    assert 'retry_after' in result
```

### 4. Concurrent Testing

Tests verify concurrent request handling:

```python
@pytest.mark.asyncio
async def test_concurrent_operations(self, configured_server):
    """Test concurrent tool execution."""
    tasks = [
        configured_server.call_tool('get_config', {'key': f'test_{i}'})
        for i in range(10)
    ]
    
    results = await asyncio.gather(*tasks)
    
    # Verify all succeeded
    for result in results:
        assert result['status'] == 'success'
```

## Test Execution

### Running Tests

**All tests with coverage:**
```bash
./tests/test_runner.py --all
```

**Specific test categories:**
```bash
./tests/test_runner.py --unit                    # All unit tests
./tests/test_runner.py --unit --cac              # CAC unit tests only
./tests/test_runner.py --unit --experimentation  # Experimentation unit tests only
./tests/test_runner.py --integration             # Integration tests
./tests/test_runner.py --error                   # Error handling tests
```

**Fast tests (exclude slow operations):**
```bash
./tests/test_runner.py --fast
```

**Specific test file or function:**
```bash
./tests/test_runner.py --specific tests/unit/test_cac_tools.py
./tests/test_runner.py --specific tests/unit/test_cac_tools.py::TestCACTools::test_get_config_success
```

### Coverage Analysis

Generate detailed coverage reports:
```bash
./tests/test_runner.py --coverage
```

This generates:
- HTML report: `htmlcov/index.html`
- Terminal summary with missing lines
- XML report for CI integration

Coverage requirements:
- Minimum 80% overall coverage (enforced)
- 100% coverage for critical paths (error handling, validation)

## Test Markers

Tests are categorized using pytest markers:

```python
@pytest.mark.unit
@pytest.mark.cac
@pytest.mark.asyncio
async def test_get_config_tool_success(self):
    """Test successful config retrieval."""
    pass
```

Available markers:
- `unit` - Unit tests
- `integration` - Integration tests
- `cac` - Context-Aware Configuration tests
- `experimentation` - Experimentation platform tests
- `error_handling` - Error scenario tests
- `validation` - Input validation tests
- `performance` - Performance-related tests
- `slow` - Tests that take >1 second
- `network` - Tests requiring network connectivity
- `mcp` - MCP protocol compliance tests

## Development Workflow

### Adding New Tests

1. **Identify test category** (unit vs integration)
2. **Choose appropriate test file** based on functionality
3. **Follow naming conventions**:
   ```python
   def test_tool_name_scenario_description(self, fixtures):
       """Clear description of what is being tested."""
   ```

4. **Use appropriate markers**:
   ```python
   @pytest.mark.unit
   @pytest.mark.cac
   @pytest.mark.asyncio
   async def test_new_functionality(self):
   ```

5. **Follow AAA pattern**:
   ```python
   # Arrange - set up test data and mocks
   mock_client.configs["test:key"] = {"value": "test"}
   
   # Act - execute the functionality being tested
   result = await tool_function(arguments)
   
   # Assert - verify the results
   assert result['status'] == 'success'
   assert result['data']['key'] == 'test'
   ```

### Test Data Management

Use fixtures and factories for consistent test data:

```python
@pytest.fixture
def sample_experiment(self):
    """Provide sample experiment data."""
    return {
        'experiment_name': 'test_experiment',
        'variants': [
            {'id': 'control', 'weight': 50},
            {'id': 'treatment', 'weight': 50}
        ],
        'overrides': {
            'control': {'feature': False},
            'treatment': {'feature': True}
        }
    }
```

### Mock Configuration

Configure mocks appropriately for different scenarios:

```python
@pytest.fixture
def failing_network_client(self):
    """Client that simulates network failures."""
    client = MockSuperpositionClient()
    client.set_failure_mode(True, "network")
    return client

@pytest.fixture
def rate_limited_client(self):
    """Client that simulates rate limiting."""
    # Custom configuration for rate limiting tests
    pass
```

## Performance Testing

### Basic Performance Validation

Tests include basic performance validation:

```python
@pytest.mark.performance
@pytest.mark.asyncio
async def test_response_time(self, configured_server):
    """Verify reasonable response times."""
    start_time = time.time()
    
    result = await configured_server.call_tool('get_config', {
        'key': 'perf_test'
    })
    
    response_time = time.time() - start_time
    assert response_time < 1.0  # Should respond within 1 second
```

### Concurrent Load Testing

```python
@pytest.mark.performance
@pytest.mark.asyncio
async def test_concurrent_load(self, configured_server):
    """Test handling of concurrent requests."""
    tasks = []
    for i in range(100):
        task = configured_server.call_tool('get_config', {
            'key': f'load_test_{i}'
        })
        tasks.append(task)
    
    start_time = time.time()
    results = await asyncio.gather(*tasks)
    total_time = time.time() - start_time
    
    # Verify all succeeded
    assert all(r['status'] == 'success' for r in results)
    
    # Verify reasonable throughput
    throughput = len(results) / total_time
    assert throughput > 10  # At least 10 requests/second
```

## Debugging Tests

### Verbose Output

Run tests with maximum verbosity:
```bash
pytest -vvv --tb=long --capture=no
```

### Debugging Specific Failures

1. **Isolate the failing test**:
   ```bash
   ./tests/test_runner.py --specific path/to/failing_test.py::test_function
   ```

2. **Add debug output**:
   ```python
   import logging
   logging.basicConfig(level=logging.DEBUG)
   
   # Add debug prints
   print(f"Debug: {result}")
   ```

3. **Use pytest debugging**:
   ```bash
   pytest --pdb  # Drop into debugger on failure
   ```

### Mock State Issues

Common issues and solutions:

1. **State leakage between tests**:
   ```python
   @pytest.fixture(autouse=True)
   def reset_mocks(self, mock_client):
       yield
       mock_client.reset()  # Clean up after each test
   ```

2. **Async context issues**:
   ```python
   @pytest.mark.asyncio
   async def test_async_function(self):
       # Ensure all async operations use await
       result = await async_function()
   ```

## CI/CD Integration

### GitHub Actions Example

```yaml
name: Test MCP Server
on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        python-version: [3.9, 3.10, 3.11]
    
    steps:
    - uses: actions/checkout@v3
    
    - name: Set up Python
      uses: actions/setup-python@v4
      with:
        python-version: ${{ matrix.python-version }}
    
    - name: Install dependencies
      run: |
        pip install -r tests/requirements.txt
    
    - name: Check test environment
      run: |
        cd mcp-server && ./tests/test_runner.py --check
    
    - name: Run tests
      run: |
        cd mcp-server && ./tests/test_runner.py --all
    
    - name: Upload coverage
      uses: codecov/codecov-action@v3
      with:
        file: ./mcp-server/coverage.xml
```

### Docker Testing

```dockerfile
FROM python:3.11-slim

WORKDIR /app
COPY tests/requirements.txt .
RUN pip install -r requirements.txt

COPY . .
RUN ./tests/test_runner.py --check && ./tests/test_runner.py --all
```

## Best Practices Summary

### Test Structure
- Use descriptive test names that explain the scenario
- Follow AAA pattern (Arrange, Act, Assert)
- Group related tests in classes
- Use appropriate markers for categorization

### Async Testing
- Always use `@pytest.mark.asyncio` for async tests
- Properly await all async operations
- Handle timeouts and cancellation appropriately

### Mock Management
- Reset mock state between tests
- Configure failure modes explicitly
- Verify mock interactions when relevant

### Error Testing
- Test both positive and negative scenarios
- Verify error response formats
- Test edge cases and boundary conditions

### Performance
- Include basic performance validation
- Test concurrent request handling
- Monitor resource usage during tests

### Maintenance
- Keep tests up to date with implementation changes
- Regularly review and refactor test code
- Monitor coverage and add tests for uncovered code

This comprehensive testing strategy ensures the MCP server implementation is robust, reliable, and ready for production use.