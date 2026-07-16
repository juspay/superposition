package io.juspay.superposition.openfeature.traits

import com.google.gson.GsonBuilder
import io.juspay.superposition.openfeature.error.SuperpositionError
import java.math.BigDecimal

/**
 * Decodes the JSON-encoded values that flag evaluation hands back.
 *
 * Deliberately not on [AllFeatureProvider]: decoding is what the bool/int/float/struct extractors
 * are defined against, and that contract is shared with the Rust and Python clients. Exposing it on
 * the interface would let an implementor silently change what a flag evaluates to in Java alone.
 */
internal object JsonValues {

    /**
     * Parses flag values, preserving each number's JSON literal type.
     *
     * Gson's stock `Object` binding maps every number to `Double`, which erases the difference
     * between `10` and `10.0` and leaves `resolveInt` no way to reject a fractional value except by
     * truncating it. The only thing wrong with that binding is the number policy, so that is the
     * only thing replaced here; the rest (objects to Map, arrays to List, booleans, strings, nulls)
     * already produces exactly the types `Value.objectToValue` accepts.
     *
     * `Integer` rather than `Long` is deliberate: `objectToValue` has no `Long` case, and
     * OpenFeature's `Value` has no integer type wider than `int` to widen to. A literal beyond int
     * range therefore degrades to a `Double` — lossy for very large integers, but `resolveInt` must
     * reject those either way.
     */
    private val gson = GsonBuilder()
        .setObjectToNumberStrategy { reader ->
            val number = BigDecimal(reader.nextString())
            // scale() <= 0 covers both 10 (scale 0) and 1e2 (scale -2); 10.0 has scale 1.
            if (number.scale() <= 0) {
                runCatching { number.intValueExact() }.getOrElse { number.toDouble() }
            } else {
                number.toDouble()
            }
        }
        .create()

    /**
     * Parse a JSON string into plain Java values: null, Boolean, String, Integer, Double, List or
     * Map.
     *
     * `@Throws` is load-bearing. Kotlin has no checked exceptions, so without it the Java callers
     * in [AllFeatureProvider] would not be forced to handle a malformed flag value — which is the
     * exact bug this method was written to fix.
     *
     * @throws SuperpositionError if the value is not well-formed JSON
     */
    @JvmStatic
    @Throws(SuperpositionError::class)
    fun parse(jsonString: String): Any? = try {
        gson.fromJson(jsonString, Any::class.java)
    } catch (e: RuntimeException) {
        throw SuperpositionError.serializationError(
            "Failed to parse flag value as JSON: $jsonString",
            e,
        )
    }
}
