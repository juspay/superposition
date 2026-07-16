package io.juspay.superposition.openfeature

import com.google.gson.Gson
import com.google.gson.JsonElement
import com.google.gson.JsonParser
import com.google.gson.JsonSyntaxException
import io.juspay.superposition.model.ContextPartial
import io.juspay.superposition.model.DimensionType as SdkDimensionType
import io.juspay.superposition.model.GetConfigOutput
import io.juspay.superposition.model.GetExperimentConfigOutput
import software.amazon.smithy.java.core.serde.document.Document
import uniffi.superposition_client.ExperimentConfig
import uniffi.superposition_client.OperationException
import uniffi.superposition_client.ffiParseConfigFileWithFilters
import uniffi.superposition_types.Config
import uniffi.superposition_types.Context
import uniffi.superposition_types.DimensionInfo
import uniffi.superposition_types.DimensionType

/**
 * Java-callable wrappers around UniFFI top-level functions and conversion helpers.
 *
 * Kotlin top-level functions in the generated bindings are not easily callable from Java
 * without referencing the auto-generated `*Kt` class. This object exposes them as
 * `@JvmStatic` methods to keep Java call sites clean.
 *
 * `internal` on purpose. This is the quarantine layer between the Smithy SDK's types and the
 * UniFFI-generated ones — every signature here traffics in `Config`, `ExperimentConfig` and
 * `Document`, none of which belong in a contract consumers compile against. It exists for
 * `FileDataSource`, `HttpDataSource` and `SuperpositionAPIProvider` inside this module and
 * nothing else; [EvaluationArgs], which does the same job, is already internal.
 *
 * Note this is not a blanket rule against exposing UniFFI types: [ConfigData] and [ExperimentData]
 * deliberately carry `Config` and `ExperimentConfig`, because that *is* the domain model — one
 * Rust definition shared by the Rust, Python and Java clients alike. Callers read those. What must
 * not leak is the machinery for *building* them.
 */
internal object FfiUtils {

    private val gson = Gson()

    /**
     * Parses a config file and applies dimension and prefix filters in a single FFI call.
     *
     * @param format either `json` or `toml`
     * @param dimensionData dimension values as JSON-encoded strings; null or empty skips dimension filtering
     * @param prefix config key prefixes; null or empty skips prefix filtering
     */
    @JvmStatic
    @Throws(OperationException::class)
    fun parseConfigFileWithFilters(
        fileContent: String,
        format: String,
        dimensionData: Map<String, String>?,
        prefix: List<String>?
    ): Config = ffiParseConfigFileWithFilters(
        fileContent,
        format,
        dimensionData?.takeIf { it.isNotEmpty() },
        prefix?.takeIf { it.isNotEmpty() }
    )

    /**
     * Converts a Smithy-generated GetConfigOutput to a UniFFI Config.
     * Used by HttpDataSource and other SDK operations to transform API responses.
     */
    @JvmStatic
    fun fromGetConfigOutput(output: GetConfigOutput): Config {
        return Config(
            contexts = output.contexts().map { toFfiContext(it) },
            overrides = output.overrides().mapValues { (_, docs) -> serializeDocumentValues(docs) },
            defaultConfigs = serializeDocumentValues(output.defaultConfigs()),
            dimensions = output.dimensions().mapValues { (_, dimInfo) -> toFfiDimensionInfo(dimInfo) }
        )
    }

    /**
     * Converts a Smithy-generated GetExperimentConfigOutput to a UniFFI ExperimentConfig,
     * so cached experiments can be handed to the evaluation FFI as-is.
     */
    @JvmStatic
    fun fromGetExperimentConfigOutput(output: GetExperimentConfigOutput): ExperimentConfig {
        return ExperimentConfig(
            experiments = output.experiments().map { EvaluationArgs.Helpers.toFfiExperiment(it) },
            experimentGroups = output.experimentGroups().map { EvaluationArgs.Helpers.toFfiExperimentGroup(it) }
        )
    }

    /**
     * Converts a server-resolved config Document into the JSON-encoded value map the providers
     * hand back. A response that is not an object is wrapped under `_value`, mirroring the Rust
     * provider.
     */
    @JvmStatic
    fun resolvedConfigToJsonMap(config: Document): Map<String, String> {
        val resolved = config.asObject()
        if (resolved !is Map<*, *>) {
            return mapOf("_value" to gson.toJson(resolved))
        }
        return resolved.entries.associate { (key, value) ->
            key.toString() to gson.toJson(if (value is Document) value.asObject() else value)
        }
    }

    /**
     * Converts a context map of JSON-encoded values into the Documents the SDK expects,
     * preserving numbers, booleans and nested structures. A value that is not valid JSON
     * is passed through as a plain string.
     */
    @JvmStatic
    fun contextToDocuments(context: Map<String, String>): Map<String, Document> {
        return context.mapValues { (_, json) -> toDocument(json) }
    }

    // ============= Private conversion helpers =============

    private fun toDocument(json: String): Document {
        val element = try {
            JsonParser.parseString(json)
        } catch (_: JsonSyntaxException) {
            return Document.of(json)
        }
        return toDocument(element)
    }

    private fun toDocument(element: JsonElement): Document = when {
        element.isJsonObject ->
            Document.of(element.asJsonObject.entrySet().associate { (k, v) -> k to toDocument(v) })
        element.isJsonArray -> Document.of(element.asJsonArray.map { toDocument(it) })
        element.isJsonPrimitive -> {
            val primitive = element.asJsonPrimitive
            when {
                primitive.isBoolean -> Document.of(primitive.asBoolean)
                primitive.isNumber -> Document.ofNumber(primitive.asNumber)
                else -> Document.of(primitive.asString)
            }
        }
        // JSON null has no Document representation; the empty string keeps the key present.
        else -> Document.of("")
    }

    private fun serializeDocument(d: Document): String {
        val obj = d.asObject()
        return gson.toJson(obj)
    }

    private fun serializeDocumentValues(m: MutableMap<String, Document>): Map<String, String> {
        return m.mapValues { (_, doc) -> serializeDocument(doc) }
    }

    private fun toFfiContext(cp: ContextPartial): Context {
        val cond = serializeDocumentValues(cp.condition())
        return Context(
            id = cp.id(),
            condition = cond,
            priority = cp.priority(),
            weight = cp.weight(),
            overrideWithKeys = cp.overrideWithKeys()
        )
    }

    private fun toFfiDimensionInfo(dim: io.juspay.superposition.model.DimensionInfo): DimensionInfo {
        return DimensionInfo(
            schema = serializeDocumentValues(dim.schemaMember()),
            position = dim.position(),
            dimensionType = toFfiDimensionType(dim.dimensionType()),
            dependencyGraph = dim.dependencyGraph(),
            valueComputeFunctionName = dim.valueComputeFunctionName(),
            description = ""
        )
    }

    private fun toFfiDimensionType(dimType: SdkDimensionType): DimensionType {
        return when (dimType.type()) {
            SdkDimensionType.Type.localCohort -> DimensionType.LocalCohort(dimType.getValue<String>())
            SdkDimensionType.Type.remoteCohort -> DimensionType.RemoteCohort(dimType.getValue<String>())
            else -> DimensionType.Regular
        }
    }
}
