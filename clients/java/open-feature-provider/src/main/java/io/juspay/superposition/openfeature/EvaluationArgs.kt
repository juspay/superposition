package io.juspay.superposition.openfeature

import com.google.gson.Gson
import dev.openfeature.sdk.EvaluationContext
import dev.openfeature.sdk.Value
import io.juspay.superposition.model.ContextPartial
import io.juspay.superposition.model.ExperimentResponse
import io.juspay.superposition.model.GetConfigOutput
import software.amazon.smithy.java.core.serde.document.Document
import uniffi.superposition_client.*
import uniffi.superposition_types.Context
import uniffi.superposition_types.Variant
import uniffi.superposition_types.VariantType

internal class EvaluationArgs {
    // Values are serialized json values.
    val defaultConfig: Map<String, String>
    val contexts: List<Context>
    // Values in the 2nd Map are serialized json values.
    val overrides: Map<String, Map<String, String>>

    @Throws(OperationException::class)
    fun evaluate(queryContext: EvaluationContext, eargs: ExperimentationArgs?): MutableMap<String, String> {
        val query = toQueryData(queryContext)
        return ffiEvalConfig(
            defaultConfig,
            contexts,
            overrides,
            query,
            MergeStrategy.MERGE,
            null,
            eargs
        ).toMutableMap()
    }

    constructor(output: GetConfigOutput) {
        defaultConfig = serializeDocumentValues(output.defaultConfigs())
        contexts = output.contexts().map { toFfiContext(it) }
        overrides = output.overrides().mapValues { serializeDocumentValues(it.value) }
    }

    constructor(config: SuperpositionConfig) {
        defaultConfig = config.defaultConfig.mapValues { valueToJsonString(toSerializable(it.value)) }
        contexts = config.contexts.map { toFfiContext(it) }
        overrides = config.overrides.mapValues {
            it.value.mapValues { e -> valueToJsonString(toSerializable(e.value)) }
        }
    }

    internal object Helpers {
        private fun toFfiVariant(v: io.juspay.superposition.model.Variant): Variant {
            var vtype = VariantType.CONTROL
            if (v.variantType().type() == io.juspay.superposition.model.VariantType.Type.EXPERIMENTAL) {
                vtype = VariantType.EXPERIMENTAL
            }
            return Variant(
                v.id(),
                vtype,
                v.contextId(),
                v.overrideId(),
                serializeDocumentValues(v.overrides().asStringMap())
            )
        }

        @JvmStatic
        fun toFfiExperiment(er: ExperimentResponse): FfiExperiment {
            val variants = er.variants().map { toFfiVariant(it) }
            return FfiExperiment(
                er.id(),
                er.trafficPercentage().toUByte(),
                variants,
                serializeDocumentValues(er.context())
            )
        }
    }

    companion object {
        private val gson = Gson()

        private fun valueToJsonString(value: Any): String {
            return gson.toJson(value)
        }

        private fun toQueryData(eContext: EvaluationContext): Map<String, String> {
            val m = eContext.asObjectMap()
            if (m == null) {
                return HashMap()
            }
            return m.mapValues { valueToJsonString(it.value) }
        }

        private fun serializeDocument(d: Document): String {
            return valueToJsonString(d.asObject())
        }

        private fun serializeDocumentValues(m: MutableMap<String, Document>): Map<String, String> {
            return m.mapValues { serializeDocument(it.value) }
        }

        private fun toFfiContext(cp: ContextPartial): Context {
            val cond = serializeDocumentValues(cp.condition())
            return Context(
                cp.id(),
                cond,
                cp.priority(),
                cp.weight(),
                cp.overrideWithKeys()
            )
        }

        private fun toFfiContext(ctx: SuperpositionConfig.Context): Context {
            val cond = ctx.condition.mapValues { valueToJsonString(it.value) }
            return Context(
                ctx.id,
                cond,
                ctx.priority,
                ctx.weight,
                ctx.overrideWithKeys
            )
        }

        private fun toSerializable(v: Value): Any {
            if (v.isStructure) {
                val struct = v.asStructure()
                return struct.asMap().mapValues { toSerializable(it.value) }
            }

            if (v.isList) {
                return v.asList().map { toSerializable(it) }
            }

            if (v.isInstant) {
                return v.asInstant().toEpochMilli()
            }

            return v.asObject()
        }

        internal fun getApplicableVariants(ectx: EvaluationContext, experimentationArgs: ExperimentationArgs): List<String> {
            val qdata = toQueryData(ectx)
            return ffiGetApplicableVariants(experimentationArgs, qdata, null)
        }
    }
}
