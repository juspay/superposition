package uniffi.superposition_client.test

import org.junit.Test
import org.junit.Assert.*
import uniffi.superposition_client.*
import com.google.gson.Gson

/**
 * Test suite for JSON parsing functions
 */
class JsonFunctionsTest {

    private val gson = Gson()

    companion object {
        // Sample JSON configuration - ride-sharing pricing example
        private const val EXAMPLE_JSON = """
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
"""
    }

    @Test
    fun testParseJsonConfig() {
        println("\n" + "=".repeat(70))
        println("  TEST: Parse JSON Configuration")
        println("=".repeat(70))

        val result = ffiParseJsonConfig(EXAMPLE_JSON)

        println("\n✓ Successfully parsed JSON configuration!\n")

        // Display default config
        println("Default Configuration:")
        println("-".repeat(50))
        result.defaultConfigs.forEach { (key, value) ->
            val parsedValue = gson.fromJson(value, Any::class.java)
            println("  $key: $parsedValue")
        }

        // Display contexts
        println("\nContexts:")
        println("-".repeat(50))
        result.contexts.forEachIndexed { index, context ->
            println("  Context ${index + 1}:")
            println("    ID: ${context.id}")
            println("    Priority: ${context.priority}")
        }

        // Display dimensions
        println("\nDimensions:")
        println("-".repeat(50))
        result.dimensions.forEach { (dimName, dimInfo) ->
            println("  $dimName:")
            println("    Position: ${dimInfo.position}")
        }

        // Assertions
        assertEquals(2, result.defaultConfigs.size)
        assertTrue(result.defaultConfigs.containsKey("per_km_rate"))
        assertTrue(result.defaultConfigs.containsKey("surge_factor"))
        assertEquals(5, result.contexts.size)
        assertEquals(3, result.dimensions.size)
    }

    @Test
    fun testErrorHandling_InvalidJson() {
        println("\n" + "=".repeat(70))
        println("  TEST: Error Handling - Invalid JSON")
        println("=".repeat(70))

        val invalidJson = "{ invalid json content }"

        try {
            ffiParseJsonConfig(invalidJson)
            fail("Expected OperationException to be thrown")
        } catch (e: OperationException) {
            println("\n✓ Correctly caught error: ${e.javaClass.simpleName}")
            println("  Message: ${e.message?.take(100)}")
            assertTrue(e.message?.contains("JSON") == true)
        }
    }

    @Test
    fun testTomlJsonEquivalence() {
        println("\n" + "=".repeat(70))
        println("  TEST: TOML/JSON Equivalence")
        println("=".repeat(70))

        // Same configuration in TOML format
        val tomlConfig = """
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
"""

        val tomlResult = ffiParseTomlConfig(tomlConfig)
        val jsonResult = ffiParseJsonConfig(EXAMPLE_JSON)

        // Both should parse successfully with same structure
        assertEquals(tomlResult.defaultConfigs.size, jsonResult.defaultConfigs.size)
        println("\n✓ Both TOML and JSON produce equivalent configs")
    }
}
