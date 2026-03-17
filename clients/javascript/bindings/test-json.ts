import { NativeResolver } from './native-resolver.js';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// Sample JSON configuration - ride-sharing pricing example
const EXAMPLE_JSON = `
{
  "default-configs": {
    "per_km_rate": { "value": 20.0, "schema": { "type": "number" } },
    "surge_factor": { "value": 0.0, "schema": { "type": "number" } }
  },
  "dimensions": {
    "city": { "position": 1, "schema": { "type": "string", "enum": ["Bangalore", "Delhi"] } },
    "vehicle_type": { "position": 2, "schema": { "type": "string", "enum": ["auto", "cab", "bike"] } },
    "hour_of_day": { "position": 3, "schema": { "type": "integer", "minimum": 0, "maximum": 23 } }
  },
  "overrides": [
    { "_context_": { "vehicle_type": "cab" }, "per_km_rate": 25.0 },
    { "_context_": { "vehicle_type": "bike" }, "per_km_rate": 15.0 },
    { "_context_": { "city": "Bangalore", "vehicle_type": "cab" }, "per_km_rate": 22.0 },
    { "_context_": { "city": "Delhi", "vehicle_type": "cab", "hour_of_day": 18 }, "surge_factor": 5.0 },
    { "_context_": { "city": "Delhi", "vehicle_type": "cab", "hour_of_day": 6 }, "surge_factor": 5.0 }
  ]
}
`;

function printSectionHeader(title: string): void {
  console.log('\n' + '='.repeat(70));
  console.log(`  ${title}`);
  console.log('='.repeat(70));
}

function testParseJsonConfig(): boolean {
  printSectionHeader('TEST 1: Parse JSON Configuration');

  try {
    const resolver = new NativeResolver();
    const result = resolver.parseJsonConfig(EXAMPLE_JSON);

    console.log('\n✓ Successfully parsed JSON configuration!\n');

    // Display default config
    console.log('Default Configuration:');
    console.log('-'.repeat(50));
    Object.entries(result.default_configs).forEach(([key, value]) => {
      console.log(`  ${key}: ${value}`);
    });

    // Access parsed objects directly
    const contexts = result.contexts;
    const overrides = result.overrides;
    const dimensions = result.dimensions;

    // Display contexts
    console.log('\nContexts:');
    console.log('-'.repeat(50));
    contexts.forEach((context: any, i: number) => {
      console.log(`  Context ${i + 1}:`);
      console.log(`    Condition: ${JSON.stringify(context.condition)}`);
      console.log(`    Override ID: ${context.id || 'N/A'}`);
      console.log(`    Priority: ${context.priority || 'N/A'}`);
    });

    // Display overrides
    console.log('\nOverrides:');
    console.log('-'.repeat(50));
    console.log(`  Total overrides: ${Object.keys(overrides).length}`);
    Object.entries(overrides).slice(0, 3).forEach(([id, data]) => {
      console.log(`  ${id}: ${JSON.stringify(data).substring(0, 100)}...`);
    });

    // Display dimensions
    console.log('\nDimensions:');
    console.log('-'.repeat(50));
    Object.entries(dimensions).forEach(([dimName, dimInfo]: [string, any]) => {
      console.log(`  ${dimName}:`);
      console.log(`    Schema: ${JSON.stringify(dimInfo.schema)}`);
      console.log(`    Position: ${dimInfo.position || 'N/A'}`);
    });

    return true;
  } catch (error: any) {
    console.log(`\n✗ Error parsing JSON: ${error.message}`);
    console.error(error.stack);
    return false;
  }
}

function testWithExternalFile(): boolean | null {
  printSectionHeader('TEST 2: Parse External JSON File');

  // Try to find the example JSON file
  const exampleFile = path.join(__dirname, '..', '..', '..', '..', 'examples', 'superposition_config_file_examples', 'example.json');

  if (!fs.existsSync(exampleFile)) {
    console.log(`\n⚠ Example file not found at: ${exampleFile}`);
    console.log('  Skipping external file test.');
    return null;
  }

  console.log(`\nReading JSON from: ${exampleFile}`);

  try {
    const resolver = new NativeResolver();
    const jsonContent = fs.readFileSync(exampleFile, 'utf8');
    const result = resolver.parseJsonConfig(jsonContent);

    console.log('\n✓ Successfully parsed external JSON file!');
    console.log('\nParsed configuration summary:');
    console.log(`  - Default config keys: ${Object.keys(result.default_configs).length}`);
    console.log(`  - Contexts: ${result.contexts.length}`);
    console.log(`  - Overrides: ${Object.keys(result.overrides).length}`);
    console.log(`  - Dimensions: ${Object.keys(result.dimensions).length}`);

    return true;
  } catch (error: any) {
    console.log(`\n✗ Error parsing external file: ${error.message}`);
    console.error(error.stack);
    return false;
  }
}

function testErrorHandling(): void {
  printSectionHeader('TEST 3: Error Handling');

  const resolver = new NativeResolver();

  const invalidJsonCases = [
    {
      name: 'Invalid JSON syntax',
      json: '{ invalid json content }'
    },
    {
      name: 'Missing required section',
      json: '{"dimensions": {"city": {"position": 1, "schema": {"type": "string"}}}}'
    },
    {
      name: 'Empty object',
      json: '{}'
    }
  ];

  invalidJsonCases.forEach((testCase, i) => {
    console.log(`\nTest ${i + 1}: ${testCase.name}`);
    console.log('-'.repeat(50));

    try {
      resolver.parseJsonConfig(testCase.json);
      console.log('✗ Expected error but parsing succeeded!');
    } catch (error: any) {
      console.log(`✓ Correctly caught error: ${error.constructor.name}`);
      console.log(`  Message: ${error.message.substring(0, 100)}`);
    }
  });
}

function testTomlJsonEquivalence(): boolean {
  printSectionHeader('TEST 4: TOML/JSON Equivalence');

  try {
    const resolver = new NativeResolver();

    const tomlConfig = `
[default-configs]
per_km_rate = { value = 20.0, schema = { type = "number" } }
surge_factor = { value = 0.0, schema = { type = "number" } }

[dimensions]
city = { position = 1, schema = { type = "string", enum = ["Bangalore", "Delhi"] } }
vehicle_type = { position = 2, schema = { type = "string", enum = ["auto", "cab", "bike"] } }

[[overrides]]
_context_ = { vehicle_type = "cab" }
per_km_rate = 25.0

[[overrides]]
_context_ = { vehicle_type = "bike" }
per_km_rate = 15.0
`;

    const jsonConfig = `
{
  "default-configs": {
    "per_km_rate": { "value": 20.0, "schema": { "type": "number" } },
    "surge_factor": { "value": 0.0, "schema": { "type": "number" } }
  },
  "dimensions": {
    "city": { "position": 1, "schema": { "type": "string", "enum": ["Bangalore", "Delhi"] } },
    "vehicle_type": { "position": 2, "schema": { "type": "string", "enum": ["auto", "cab", "bike"] } }
  },
  "overrides": [
    { "_context_": { "vehicle_type": "cab" }, "per_km_rate": 25.0 },
    { "_context_": { "vehicle_type": "bike" }, "per_km_rate": 15.0 }
  ]
}
`;

    const tomlResult = resolver.parseTomlConfig(tomlConfig);
    const jsonResult = resolver.parseJsonConfig(jsonConfig);

    // Both should parse successfully with same structure
    const tomlKeys = Object.keys(tomlResult.default_configs).length;
    const jsonKeys = Object.keys(jsonResult.default_configs).length;

    if (tomlKeys !== jsonKeys) {
      console.log(`\n✗ default_configs mismatch: TOML=${tomlKeys}, JSON=${jsonKeys}`);
      return false;
    }

    console.log('\n✓ Both TOML and JSON produce equivalent configs');
    return true;
  } catch (error: any) {
    console.log(`\n✗ Equivalence test failed: ${error.message}`);
    console.error(error.stack);
    return false;
  }
}

function main(): number {
  console.log('\n' + '='.repeat(70));
  console.log('  SUPERPOSITION JSON PARSING - JAVASCRIPT/TYPESCRIPT BINDING TESTS');
  console.log('='.repeat(70));

  const results: [string, boolean | null][] = [];

  // Run tests
  results.push(['Parse JSON', testParseJsonConfig()]);
  results.push(['External File', testWithExternalFile()]);

  // Error handling test (doesn't return pass/fail)
  testErrorHandling();

  results.push(['TOML/JSON Equivalence', testTomlJsonEquivalence()]);

  // Summary
  printSectionHeader('TEST SUMMARY');

  const passed = results.filter(([_, result]) => result === true).length;
  const total = results.filter(([_, result]) => result !== null).length;

  results.forEach(([testName, result]) => {
    if (result === true) {
      console.log(`  ✓ ${testName}`);
    } else if (result === false) {
      console.log(`  ✗ ${testName}`);
    } else {
      console.log(`  - ${testName} (skipped)`);
    }
  });

  console.log(`\n  Total: ${passed}/${total} tests passed`);
  console.log('='.repeat(70));

  return passed === total ? 0 : 1;
}

// Run main and exit with appropriate code
process.exit(main());
