# Superposition MCP Server Test Suite

This directory contains comprehensive tests for the Superposition MCP (Model Context Protocol) server implementation.

## Overview

The test suite is designed to validate all aspects of the MCP server implementation, including:

- **Unit Tests**: Individual tool implementations for CAC and Experimentation
- **Integration Tests**: Full MCP server functionality and protocol compliance
- **Error Handling**: Various failure scenarios and edge cases
- **Validation**: Input validation and data integrity
- **Performance**: Basic performance characteristics

## Test Structure

```
tests/
├── conftest.py              # Shared fixtures and configuration
├── pytest.ini              # Pytest configuration
├── requirements.txt         # Test dependencies
├── README.md               # This file
├── unit/                   # Unit tests
│   ├── test_cac_tools.py           # CAC tool tests
│   ├── test_experimentation_tools.py # Experimentation tool tests
│   └── test_error_handling.py      # Error handling tests
├── integration/            # Integration tests
│   └── test_mcp_server.py          # Full server integration tests
└── mocks/                  # Mock implementations
    ├── __init__.py
    └── sdk_mock.py                 # Mock Superposition SDK
```

## Running Tests

### Prerequisites

1. Install test dependencies:
```bash
pip install -r tests/requirements.txt
```

2. Ensure the Superposition SDK is available in your environment.

### Basic Test Execution

Run all tests:
```bash
pytest
```

Run specific test categories:
```bash
# Unit tests only
pytest -m unit

# Integration tests only
pytest -m integration

# CAC-specific tests
pytest -m cac

# Experimentation-specific tests
pytest -m experimentation

# Error handling tests
pytest -m error_handling
```

Run tests with coverage:
```bash
pytest --cov=src --cov-report=html
```

### Advanced Test Options

Run tests with verbose output:
```bash
pytest -v
```

Run tests in parallel (requires pytest-xdist):
```bash
pytest -n auto
```

Run only fast tests (exclude slow tests):
```bash
pytest -m "not slow"
```

Run tests with specific timeout:
```bash
pytest --timeout=60
```

## Test Categories

### Unit Tests

#### CAC Tools (`test_cac_tools.py`)
Tests for Context-Aware Configuration tools:
- `get_config` - Configuration retrieval
- `set_config` - Configuration creation/update
- `delete_config` - Configuration deletion
- `list_configs` - Configuration listing
- `create_context` - Context creation
- `get_context` - Context retrieval
- `update_context` - Context modification
- `delete_context` - Context deletion
- `list_contexts` - Context listing

#### Experimentation Tools (`test_experimentation_tools.py`)
Tests for Experimentation platform tools:
- `create_experiment` - Experiment creation
- `get_experiment` - Experiment retrieval
- `list_experiments` - Experiment listing
- `update_experiment` - Experiment modification
- `ramp_experiment` - Traffic ramping
- `pause_experiment` - Experiment pausing
- `resume_experiment` - Experiment resuming
- `conclude_experiment` - Experiment conclusion
- `create_experiment_group` - Group creation
- `get_experiment_group` - Group retrieval
- `list_experiment_groups` - Group listing

#### Error Handling (`test_error_handling.py`)
Tests for error scenarios:
- Network failures and timeouts
- Authentication and authorization errors
- Input validation failures
- Server-side errors
- Rate limiting scenarios
- Edge cases and boundary conditions

### Integration Tests

#### MCP Server (`test_mcp_server.py`)
Tests for complete server functionality:
- Server startup and tool registration
- Tool discovery and listing
- End-to-end workflows
- Concurrent request handling
- Protocol compliance
- Error propagation
- Performance characteristics

## Mock Infrastructure

### SDK Mock (`mocks/sdk_mock.py`)
Comprehensive mock implementation of the Superposition SDK:
- `MockSuperpositionClient` - Mock client with configurable failure modes
- `MockMCPServer` - Mock MCP server for testing
- `MockDataGenerator` - Test data generation utilities

### Failure Simulation
The mock client supports various failure modes:
- `network` - Connection timeouts and network errors
- `auth` - Authentication failures
- `validation` - Input validation errors
- `server` - Internal server errors

## Test Configuration

### Fixtures (`conftest.py`)
Shared test fixtures include:
- `mock_superposition_client` - Pre-configured mock client
- `mock_config` - Default configuration settings
- `sample_*_data` - Sample data for different entity types
- `test_data_factory` - Factory for generating test data
- `async_test_utils` - Utilities for async testing

### Environment Setup
Tests automatically configure a clean test environment with:
- Isolated mock data stores
- Configurable failure modes
- Consistent test data
- Proper async handling

## Writing New Tests

### Adding Unit Tests

1. Create test functions following the pattern:
```python
@pytest.mark.asyncio
async def test_tool_name_scenario(self, mock_client, tool_base):
    """Test description."""
    # Arrange - set up test data
    # Act - call the tool
    # Assert - verify results
```

2. Use appropriate markers:
```python
@pytest.mark.unit
@pytest.mark.cac  # or @pytest.mark.experimentation
```

### Adding Integration Tests

1. Use the `configured_server` fixture for full server testing
2. Test complete workflows rather than individual operations
3. Verify MCP protocol compliance

### Testing Error Scenarios

1. Configure mock clients with appropriate failure modes
2. Test both tool-level and server-level error handling
3. Verify error response formats match expected structure

## Best Practices

### Test Organization
- Group related tests in classes
- Use descriptive test names that explain the scenario
- Include both positive and negative test cases
- Test edge cases and boundary conditions

### Async Testing
- Always use `@pytest.mark.asyncio` for async test functions
- Use `await` for all async operations
- Handle timeouts appropriately

### Mock Usage
- Reset mock state between tests using fixtures
- Configure failure modes explicitly for error testing
- Verify mock call counts and arguments when relevant

### Assertions
- Use specific assertions that clearly indicate what failed
- Include helpful error messages in assertions
- Test both success and error response formats

## Performance Testing

Basic performance tests are included to ensure:
- Response times are reasonable (< 1 second for simple operations)
- Concurrent requests are handled properly
- Memory usage doesn't grow unbounded during testing

For comprehensive performance testing, consider using dedicated tools like:
- `pytest-benchmark` for microbenchmarks
- `locust` for load testing
- `py-spy` for profiling

## CI/CD Integration

The test suite is designed to work in CI/CD environments:

### GitHub Actions Example
```yaml
- name: Run tests
  run: |
    pip install -r tests/requirements.txt
    pytest --cov=src --cov-report=xml
    
- name: Upload coverage
  uses: codecov/codecov-action@v3
  with:
    file: coverage.xml
```

### Test Parallelization
For faster CI runs, use pytest-xdist:
```bash
pip install pytest-xdist
pytest -n auto
```

## Troubleshooting

### Common Issues

1. **Import Errors**: Ensure the Superposition SDK is properly installed
2. **Async Errors**: Check that all async functions use `@pytest.mark.asyncio`
3. **Mock State**: Use fixture scope appropriately to avoid state leakage
4. **Timeouts**: Increase timeout values for slow operations

### Debug Mode
Run tests with maximum verbosity:
```bash
pytest -vvv --tb=long --capture=no
```

### Test Data Issues
Reset test environment:
```python
# In test function
mock_client.reset()
mock_server.reset()
```

## Contributing

When adding new tests:

1. Follow existing patterns and conventions
2. Include both unit and integration tests for new features
3. Add appropriate markers and documentation
4. Ensure tests are deterministic and don't depend on external state
5. Update this README if adding new test categories or significant changes

## Coverage Requirements

The test suite aims for:
- Minimum 80% code coverage (enforced by pytest configuration)
- 100% coverage of critical paths (error handling, validation)
- Comprehensive testing of all public APIs

Generate coverage reports:
```bash
pytest --cov=src --cov-report=html
open htmlcov/index.html  # View detailed coverage report
```