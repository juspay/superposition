package io.superposition.openfeature;

import dev.openfeature.sdk.EvaluationContext;
import dev.openfeature.sdk.Value;
import io.juspay.superposition.model.ContextPartial;
import io.juspay.superposition.model.GetConfigOutput;
import com.google.gson.Gson;
import software.amazon.smithy.java.core.serde.document.Document;
import uniffi.superposition_client.MergeStrategy;
import uniffi.superposition_client.OperationException;
import uniffi.superposition_client.Superposition_clientKt;
import uniffi.superposition_types.Context;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

class EvaluationArgs {
    private static final Gson gson = new Gson();

    private static String valueToJsonString(Object value) {
        return gson.toJson(value);
    }

    private static Map<String, String> toQueryData(EvaluationContext eContext) {
        var m = eContext.asObjectMap();
        if (m == null) {
            return new HashMap();
        }
        return m.entrySet().stream()
            .collect(Collectors.toMap(
                Map.Entry::getKey,
                entry -> valueToJsonString(entry.getValue())
            ));
    }

    private static Object documentToObject(Document d) {
        return d.asObject();
    }

    private static String serializeDocument(Document d) {
        return valueToJsonString(documentToObject(d));
    }

    private static Map<String, String> serializeDocumentValues(Map<String, Document> m) {
        return m.entrySet().stream().collect(
            Collectors.toMap(
                Map.Entry::getKey,
                entry -> serializeDocument(entry.getValue())
            )
        );
    }

    private static Context toFfiContext(ContextPartial cp) {
        var cond = serializeDocumentValues(cp.condition());
        return new Context(
            cp.id(),
            cond,
            cp.priority(),
            cp.weight(),
            cp.overrideWithKeys()
        );
    }

    private static Context toFfiContext(SuperpositionConfig.Context ctx) {
        var cond = ctx.condition.entrySet().stream().collect(Collectors.toMap(
            Map.Entry::getKey,
            entry -> valueToJsonString(entry.getValue())
        ));
        return new Context(
            ctx.id,
            cond,
            ctx.priority,
            ctx.weight,
            ctx.overrideWithKeys
        );
    }

    private static Object toSerializable(Value v) {
        if (v.isStructure()) {
            var struct = v.asStructure();
            return struct.asMap().entrySet().stream().collect(Collectors.toMap(
                Map.Entry::getKey,
                entry -> toSerializable(entry.getValue())
            ));
        }

        if (v.isList()) {
            var list = v.asList();
            return list.stream().map(EvaluationArgs::toSerializable).collect(Collectors.toList());
        }

        if (v.isInstant()) {
            // REVIEW This might be incorrect.
            return v.asInstant().toEpochMilli();
        }

        return v.asObject();
    }

    // Values are serialized json values.
    final Map<String, String> defaultConfig;
    final List<Context> contexts;
    // Values in the 2nd Map are serialized json values.
    final Map<String, Map<String, String>> overrides;

    Map<String, String> evaluate(EvaluationContext queryContext) throws OperationException {
        var query = toQueryData(queryContext);
        return Superposition_clientKt.ffiEvalConfig(
            defaultConfig,
            contexts,
            overrides,
            query,
            MergeStrategy.MERGE,
            null
        );
    }

    EvaluationArgs(GetConfigOutput output) {
        defaultConfig = serializeDocumentValues(output.defaultConfigs());
        contexts = output.contexts().stream()
            .map(EvaluationArgs::toFfiContext)
            .collect(Collectors.toList());
        overrides = output.overrides().entrySet().stream().collect(
            Collectors.toMap(
                Map.Entry::getKey,
                entry -> serializeDocumentValues(entry.getValue())
            )
        );
    }

    EvaluationArgs(SuperpositionConfig config) {
        defaultConfig = config.defaultConfig
            .entrySet()
            .stream()
            .collect(Collectors.toMap(
                Map.Entry::getKey,
                entry -> valueToJsonString(toSerializable(entry.getValue()))));
        contexts = config.contexts.stream()
            .map(EvaluationArgs::toFfiContext)
            .collect(Collectors.toList());
        overrides = config.overrides.entrySet().stream().collect(Collectors.toMap(
            Map.Entry::getKey,
            entry -> entry.getValue().entrySet().stream().collect(Collectors.toMap(
                Map.Entry::getKey,
                _entry -> valueToJsonString(toSerializable(_entry.getValue()))
            ))
        ));
    }
}
