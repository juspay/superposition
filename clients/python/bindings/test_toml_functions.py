#!/usr/bin/env python3
"""
Test script for TOML parsing functions in superposition_bindings

This script demonstrates the usage of:
- ffi_parse_toml_config: Parse TOML configuration into structured format
"""

import json
import sys
from pathlib import Path

# Import the generated bindings
from superposition_bindings.superposition_client import ffi_parse_toml_config

# Sample TOML configuration - ride-sharing pricing example
EXAMPLE_TOML = """
[default-configs]
per_km_rate = { "value" = 20.0, "schema" = { "type" = "number" } }
surge_factor = { "value" = 0.0, "schema" = { "type" = "number" } }

[dimensions]
city = { position = 1, schema = { "type" = "string", "enum" = ["Bangalore", "Delhi"] } }
vehicle_type = { position = 2, schema = { "type" = "string", "enum" = [ "auto", "cab", "bike", ] } }
hour_of_day = { position = 3, schema = { "type" = "integer", "minimum" = 0, "maximum" = 23 }}

[[overrides]]
_context_ = { vehicle_type = "cab" }
per_km_rate = 25.0

[[overrides]]
_context_ = { vehicle_type = "bike" }
per_km_rate = 15.0

[[overrides]]
_context_ = { city = "Bangalore", vehicle_type = "cab" }
per_km_rate = 22.0

[[overrides]]
_context_ = { city = "Delhi", vehicle_type = "cab", hour_of_day = 18 }
surge_factor = 5.0

[[overrides]]
_context_ = { city = "Delhi", vehicle_type = "cab", hour_of_day = 6 }
surge_factor = 5.0
"""


def print_section_header(title):
    """Print a formatted section header"""
    print(f"\n{'=' * 70}")
    print(f"  {title}")
    print(f"{'=' * 70}")


def test_parse_toml_config():
    """Test the ffi_parse_toml_config function"""
    print_section_header("TEST 1: Parse TOML Configuration")

    try:
        result = ffi_parse_toml_config(EXAMPLE_TOML)

        print("\n✓ Successfully parsed TOML configuration!\n")

        # Display default config
        print("Default Configuration:")
        print("-" * 50)
        for key, value in result.default_configs.items():
            # value is a JSON string, parse it for display
            parsed_value = json.loads(value)
            print(f"  {key}: {parsed_value}")

        # Display contexts (now directly available as typed objects)
        print("\nContexts:")
        print("-" * 50)
        for i, context in enumerate(result.contexts, 1):
            print(f"  Context {i}:")
            print(f"    Override ID: {context.id}")
            print(f"    Priority: {context.priority}")

        # Display overrides
        print("\nOverrides:")
        print("-" * 50)
        overrides = result.overrides
        print(f"  Total overrides: {len(overrides)}")
        for override_id in list(overrides.keys())[:3]:
            print(f"  {override_id}")

        # Display dimensions
        print("\nDimensions:")
        print("-" * 50)
        for dim_name, dim_info in result.dimensions.items():
            print(f"  {dim_name}:")
            print(f"    Position: {dim_info.position}")

        return True

    except Exception as e:
        print(f"\n✗ Error parsing TOML: {e}")
        import traceback

        traceback.print_exc()
        return False


def test_with_external_file():
    """Test parsing a TOML file from the examples directory"""
    print_section_header("TEST 2: Parse External TOML File")

    # Try to find the example TOML file
    example_file = (
        Path(__file__).parent.parent.parent.parent
        / "examples"
        / "superposition_config_file_examples"
        / "example.toml"
    )

    if not example_file.exists():
        print(f"\n⚠ Example file not found at: {example_file}")
        print("  Skipping external file test.")
        return None

    print(f"\nReading TOML from: {example_file}")

    try:
        toml_content = example_file.read_text()
        result = ffi_parse_toml_config(toml_content)

        print(f"\n✓ Successfully parsed external TOML file!")
        print(f"\nParsed configuration summary:")
        print(f"  - Default config keys: {len(result.default_configs)}")
        print(f"  - Contexts: {len(result.contexts)}")
        print(f"  - Overrides: {len(result.overrides)}")
        print(f"  - Dimensions: {len(result.dimensions)}")

        return True

    except Exception as e:
        print(f"\n✗ Error parsing external file: {e}")
        import traceback

        traceback.print_exc()
        return False


def test_error_handling():
    """Test error handling with invalid TOML"""
    print_section_header("TEST 3: Error Handling")

    invalid_toml_cases = [
        {"name": "Invalid TOML syntax", "toml": "[invalid toml content ][["},
        {
            "name": "Missing required section",
            "toml": '[dimensions]\ncity = { position = 1, schema = { "type" = "string" } }',
        },
        {
            "name": "Missing position in dimension",
            "toml": '[default-configs]\nkey1 = { value = 10, schema = { type = "integer" } }\n\n[dimensions]\ncity = { schema = { "type" = "string" } }\n\n[[overrides]]\n_context_= {city="bangalore"}\nkey1 = 20',
        },
    ]

    for i, case in enumerate(invalid_toml_cases, 1):
        print(f"\nTest {i}: {case['name']}")
        print("-" * 50)

        try:
            result = ffi_parse_toml_config(case["toml"])
            print(f"✗ Expected error but parsing succeeded!")
        except Exception as e:
            print(f"✓ Correctly caught error: {type(e).__name__}")
            print(f"  Message: {str(e)[:100]}")


def main():
    """Run all tests"""
    print("\n" + "=" * 70)
    print("  SUPERPOSITION TOML PARSING - PYTHON BINDING TESTS")
    print("=" * 70)

    results = []

    # Run tests
    results.append(("Parse TOML", test_parse_toml_config()))
    results.append(("External File", test_with_external_file()))

    # Error handling test (doesn't return pass/fail)
    test_error_handling()

    # Summary
    print_section_header("TEST SUMMARY")

    passed = sum(1 for _, result in results if result is True)
    total = sum(1 for _, result in results if result is not None)

    for test_name, result in results:
        if result is True:
            print(f"  ✓ {test_name}")
        elif result is False:
            print(f"  ✗ {test_name}")
        else:
            print(f"  - {test_name} (skipped)")

    print(f"\n  Total: {passed}/{total} tests passed")
    print("=" * 70)

    return 0 if passed == total else 1


if __name__ == "__main__":
    sys.exit(main())
