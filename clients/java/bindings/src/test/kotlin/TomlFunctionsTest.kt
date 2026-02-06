package uniffi.superposition_client.test

import org.junit.Test
import org.junit.Assert.*
import uniffi.superposition_client.*
import com.google.gson.Gson
import com.google.gson.reflect.TypeToken

/**
 * Test suite for TOML parsing functions
 *
 * This demonstrates the usage of:
 * - ffiParseTomlConfig: Parse TOML configuration into structured format
 */
class TomlFunctionsTest {

    private val gson = Gson()

    companion object {
        // Sample TOML configuration - ride-sharing pricing example
        private const val EXAMPLE_TOML = """
[default-config]
per_km_rate = { "value" = 20.0, "schema" = { "type" = "number" } }
surge_factor = { "value" = 0.0, "schema" = { "type" = "number" } }

[dimensions]
city = { position = 1, schema = { "type" = "string", "enum" = ["Bangalore", "Delhi"] } }
vehicle_type = { position = 2, schema = { "type" = "string", "enum" = [ "auto", "cab", "bike", ] } }
hour_of_day = { position = 3, schema = { "type" = "integer", "minimum" = 0, "maximum" = 23 }}

[[context]]
_condition_ = { vehicle_type = "cab" }
per_km_rate = 25.0

[[context]]
_condition_ = { vehicle_type = "bike" }
per_km_rate = 15.0

[[context]]
_condition_ = { city = "Bangalore", vehicle_type = "cab" }
per_km_rate = 22.0

[[context]]
_condition_ = { city = "Delhi", vehicle_type = "cab", hour_of_day = 18 }
surge_factor = 5.0

[[context]]
_condition_ = { city = "Delhi", vehicle_type = "cab", hour_of_day = 6 }
surge_factor = 5.0
"""
    }

    @Test
    fun testParseTomlConfig() {
        println("\n" + "=".repeat(70))
        println("  TEST: Parse TOML Configuration")
        println("=".repeat(70))

        val result = ffiParseTomlConfig(EXAMPLE_TOML)

        println("\n✓ Successfully parsed TOML configuration!\n")

        // Display default config
        println("Default Configuration:")
        println("-".repeat(50))
        result.defaultConfigs.forEach { (key, value) ->
            // value is a JSON string, parse it for display
            val parsedValue = gson.fromJson(value, Any::class.java)
            println("  $key: $parsedValue")
        }

        // Display contexts (now directly available as typed objects)
        println("\nContexts:")
        println("-".repeat(50))
        result.contexts.forEachIndexed { index, context ->
            println("  Context ${index + 1}:")
            println("    ID: ${context.id}")
            println("    Priority: ${context.priority}")
        }

        // Display overrides
        println("\nOverrides:")
        println("-".repeat(50))
        println("  Total overrides: ${result.overrides.size}")

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
    fun testErrorHandling_InvalidToml() {
        println("\n" + "=".repeat(70))
        println("  TEST: Error Handling - Invalid TOML")
        println("=".repeat(70))

        val invalidToml = "[invalid toml content ][["

        try {
            ffiParseTomlConfig(invalidToml)
            fail("Expected OperationException to be thrown")
        } catch (e: OperationException) {
            println("\n✓ Correctly caught error: ${e.javaClass.simpleName}")
            println("  Message: ${e.message?.take(100)}")
            assertTrue(e.message?.contains("TOML") == true)
        }
    }

    @Test
    fun testErrorHandling_MissingSection() {
        println("\n" + "=".repeat(70))
        println("  TEST: Error Handling - Missing Required Section")
        println("=".repeat(70))

        val invalidToml = """
[dimensions]
city = { position = 1, schema = { "type" = "string" } }
"""

        try {
            ffiParseTomlConfig(invalidToml)
            fail("Expected OperationException to be thrown")
        } catch (e: OperationException) {
            println("\n✓ Correctly caught error: ${e.javaClass.simpleName}")
            println("  Message: ${e.message?.take(100)}")
            assertTrue(e.message?.contains("default-config") == true)
        }
    }
}
