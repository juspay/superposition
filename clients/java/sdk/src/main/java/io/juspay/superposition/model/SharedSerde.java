
package io.juspay.superposition.model;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiConsumer;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.java.core.serde.MapSerializer;
import software.amazon.smithy.java.core.serde.ShapeDeserializer;
import software.amazon.smithy.java.core.serde.ShapeSerializer;
import software.amazon.smithy.java.core.serde.document.Document;


/**
 * Defines shared serialization and deserialization methods for map and list shapes.
 */
final class SharedSerde {

    static final class WorkspaceListSerializer implements BiConsumer<List<WorkspaceResponse>, ShapeSerializer> {
        static final WorkspaceListSerializer INSTANCE = new WorkspaceListSerializer();

        @Override
        public void accept(List<WorkspaceResponse> values, ShapeSerializer serializer) {
            for (var value : values) {
                serializer.writeStruct(SharedSchemas.WORKSPACE_LIST.listMember(), value);
            }
        }
    }

    static List<WorkspaceResponse> deserializeWorkspaceList(Schema schema, ShapeDeserializer deserializer) {
        var size = deserializer.containerSize();
        List<WorkspaceResponse> result = size == -1 ? new ArrayList<>() : new ArrayList<>(size);
        deserializer.readList(schema, result, WorkspaceList$MemberDeserializer.INSTANCE);
        return result;
    }

    private static final class WorkspaceList$MemberDeserializer implements ShapeDeserializer.ListMemberConsumer<List<WorkspaceResponse>> {
        static final WorkspaceList$MemberDeserializer INSTANCE = new WorkspaceList$MemberDeserializer();

        @Override
        public void accept(List<WorkspaceResponse> state, ShapeDeserializer deserializer) {
            if (deserializer.isNull()) {

                return;
            }
            state.add(WorkspaceResponse.builder().deserializeMember(deserializer, SharedSchemas.WORKSPACE_LIST.listMember()).build());
        }
    }

    static final class WebhookListSerializer implements BiConsumer<List<WebhookResponse>, ShapeSerializer> {
        static final WebhookListSerializer INSTANCE = new WebhookListSerializer();

        @Override
        public void accept(List<WebhookResponse> values, ShapeSerializer serializer) {
            for (var value : values) {
                serializer.writeStruct(SharedSchemas.WEBHOOK_LIST.listMember(), value);
            }
        }
    }

    static List<WebhookResponse> deserializeWebhookList(Schema schema, ShapeDeserializer deserializer) {
        var size = deserializer.containerSize();
        List<WebhookResponse> result = size == -1 ? new ArrayList<>() : new ArrayList<>(size);
        deserializer.readList(schema, result, WebhookList$MemberDeserializer.INSTANCE);
        return result;
    }

    private static final class WebhookList$MemberDeserializer implements ShapeDeserializer.ListMemberConsumer<List<WebhookResponse>> {
        static final WebhookList$MemberDeserializer INSTANCE = new WebhookList$MemberDeserializer();

        @Override
        public void accept(List<WebhookResponse> state, ShapeDeserializer deserializer) {
            if (deserializer.isNull()) {

                return;
            }
            state.add(WebhookResponse.builder().deserializeMember(deserializer, SharedSchemas.WEBHOOK_LIST.listMember()).build());
        }
    }

    static final class OrganisationListSerializer implements BiConsumer<List<OrganisationResponse>, ShapeSerializer> {
        static final OrganisationListSerializer INSTANCE = new OrganisationListSerializer();

        @Override
        public void accept(List<OrganisationResponse> values, ShapeSerializer serializer) {
            for (var value : values) {
                serializer.writeStruct(SharedSchemas.ORGANISATION_LIST.listMember(), value);
            }
        }
    }

    static List<OrganisationResponse> deserializeOrganisationList(Schema schema, ShapeDeserializer deserializer) {
        var size = deserializer.containerSize();
        List<OrganisationResponse> result = size == -1 ? new ArrayList<>() : new ArrayList<>(size);
        deserializer.readList(schema, result, OrganisationList$MemberDeserializer.INSTANCE);
        return result;
    }

    private static final class OrganisationList$MemberDeserializer implements ShapeDeserializer.ListMemberConsumer<List<OrganisationResponse>> {
        static final OrganisationList$MemberDeserializer INSTANCE = new OrganisationList$MemberDeserializer();

        @Override
        public void accept(List<OrganisationResponse> state, ShapeDeserializer deserializer) {
            if (deserializer.isNull()) {

                return;
            }
            state.add(OrganisationResponse.builder().deserializeMember(deserializer, SharedSchemas.ORGANISATION_LIST.listMember()).build());
        }
    }

    static final class TypeTemplatesListSerializer implements BiConsumer<List<TypeTemplatesResponse>, ShapeSerializer> {
        static final TypeTemplatesListSerializer INSTANCE = new TypeTemplatesListSerializer();

        @Override
        public void accept(List<TypeTemplatesResponse> values, ShapeSerializer serializer) {
            for (var value : values) {
                serializer.writeStruct(SharedSchemas.TYPE_TEMPLATES_LIST.listMember(), value);
            }
        }
    }

    static List<TypeTemplatesResponse> deserializeTypeTemplatesList(Schema schema, ShapeDeserializer deserializer) {
        var size = deserializer.containerSize();
        List<TypeTemplatesResponse> result = size == -1 ? new ArrayList<>() : new ArrayList<>(size);
        deserializer.readList(schema, result, TypeTemplatesList$MemberDeserializer.INSTANCE);
        return result;
    }

    private static final class TypeTemplatesList$MemberDeserializer implements ShapeDeserializer.ListMemberConsumer<List<TypeTemplatesResponse>> {
        static final TypeTemplatesList$MemberDeserializer INSTANCE = new TypeTemplatesList$MemberDeserializer();

        @Override
        public void accept(List<TypeTemplatesResponse> state, ShapeDeserializer deserializer) {
            if (deserializer.isNull()) {

                return;
            }
            state.add(TypeTemplatesResponse.builder().deserializeMember(deserializer, SharedSchemas.TYPE_TEMPLATES_LIST.listMember()).build());
        }
    }

    static final class FunctionListResponseSerializer implements BiConsumer<List<FunctionResponse>, ShapeSerializer> {
        static final FunctionListResponseSerializer INSTANCE = new FunctionListResponseSerializer();

        @Override
        public void accept(List<FunctionResponse> values, ShapeSerializer serializer) {
            for (var value : values) {
                serializer.writeStruct(SharedSchemas.FUNCTION_LIST_RESPONSE.listMember(), value);
            }
        }
    }

    static List<FunctionResponse> deserializeFunctionListResponse(Schema schema, ShapeDeserializer deserializer) {
        var size = deserializer.containerSize();
        List<FunctionResponse> result = size == -1 ? new ArrayList<>() : new ArrayList<>(size);
        deserializer.readList(schema, result, FunctionListResponse$MemberDeserializer.INSTANCE);
        return result;
    }

    private static final class FunctionListResponse$MemberDeserializer implements ShapeDeserializer.ListMemberConsumer<List<FunctionResponse>> {
        static final FunctionListResponse$MemberDeserializer INSTANCE = new FunctionListResponse$MemberDeserializer();

        @Override
        public void accept(List<FunctionResponse> state, ShapeDeserializer deserializer) {
            if (deserializer.isNull()) {

                return;
            }
            state.add(FunctionResponse.builder().deserializeMember(deserializer, SharedSchemas.FUNCTION_LIST_RESPONSE.listMember()).build());
        }
    }

    static final class ListVariantUpdateRequestSerializer implements BiConsumer<List<VariantUpdateRequest>, ShapeSerializer> {
        static final ListVariantUpdateRequestSerializer INSTANCE = new ListVariantUpdateRequestSerializer();

        @Override
        public void accept(List<VariantUpdateRequest> values, ShapeSerializer serializer) {
            for (var value : values) {
                serializer.writeStruct(SharedSchemas.LIST_VARIANT_UPDATE_REQUEST.listMember(), value);
            }
        }
    }

    static List<VariantUpdateRequest> deserializeListVariantUpdateRequest(Schema schema, ShapeDeserializer deserializer) {
        var size = deserializer.containerSize();
        List<VariantUpdateRequest> result = size == -1 ? new ArrayList<>() : new ArrayList<>(size);
        deserializer.readList(schema, result, ListVariantUpdateRequest$MemberDeserializer.INSTANCE);
        return result;
    }

    private static final class ListVariantUpdateRequest$MemberDeserializer implements ShapeDeserializer.ListMemberConsumer<List<VariantUpdateRequest>> {
        static final ListVariantUpdateRequest$MemberDeserializer INSTANCE = new ListVariantUpdateRequest$MemberDeserializer();

        @Override
        public void accept(List<VariantUpdateRequest> state, ShapeDeserializer deserializer) {
            if (deserializer.isNull()) {

                return;
            }
            state.add(VariantUpdateRequest.builder().deserializeMember(deserializer, SharedSchemas.LIST_VARIANT_UPDATE_REQUEST.listMember()).build());
        }
    }

    static final class ExperimentListSerializer implements BiConsumer<List<ExperimentResponse>, ShapeSerializer> {
        static final ExperimentListSerializer INSTANCE = new ExperimentListSerializer();

        @Override
        public void accept(List<ExperimentResponse> values, ShapeSerializer serializer) {
            for (var value : values) {
                serializer.writeStruct(SharedSchemas.EXPERIMENT_LIST.listMember(), value);
            }
        }
    }

    static List<ExperimentResponse> deserializeExperimentList(Schema schema, ShapeDeserializer deserializer) {
        var size = deserializer.containerSize();
        List<ExperimentResponse> result = size == -1 ? new ArrayList<>() : new ArrayList<>(size);
        deserializer.readList(schema, result, ExperimentList$MemberDeserializer.INSTANCE);
        return result;
    }

    private static final class ExperimentList$MemberDeserializer implements ShapeDeserializer.ListMemberConsumer<List<ExperimentResponse>> {
        static final ExperimentList$MemberDeserializer INSTANCE = new ExperimentList$MemberDeserializer();

        @Override
        public void accept(List<ExperimentResponse> state, ShapeDeserializer deserializer) {
            if (deserializer.isNull()) {

                return;
            }
            state.add(ExperimentResponse.builder().deserializeMember(deserializer, SharedSchemas.EXPERIMENT_LIST.listMember()).build());
        }
    }

    static final class ExperimentGroupListSerializer implements BiConsumer<List<ExperimentGroupResponse>, ShapeSerializer> {
        static final ExperimentGroupListSerializer INSTANCE = new ExperimentGroupListSerializer();

        @Override
        public void accept(List<ExperimentGroupResponse> values, ShapeSerializer serializer) {
            for (var value : values) {
                serializer.writeStruct(SharedSchemas.EXPERIMENT_GROUP_LIST.listMember(), value);
            }
        }
    }

    static List<ExperimentGroupResponse> deserializeExperimentGroupList(Schema schema, ShapeDeserializer deserializer) {
        var size = deserializer.containerSize();
        List<ExperimentGroupResponse> result = size == -1 ? new ArrayList<>() : new ArrayList<>(size);
        deserializer.readList(schema, result, ExperimentGroupList$MemberDeserializer.INSTANCE);
        return result;
    }

    private static final class ExperimentGroupList$MemberDeserializer implements ShapeDeserializer.ListMemberConsumer<List<ExperimentGroupResponse>> {
        static final ExperimentGroupList$MemberDeserializer INSTANCE = new ExperimentGroupList$MemberDeserializer();

        @Override
        public void accept(List<ExperimentGroupResponse> state, ShapeDeserializer deserializer) {
            if (deserializer.isNull()) {

                return;
            }
            state.add(ExperimentGroupResponse.builder().deserializeMember(deserializer, SharedSchemas.EXPERIMENT_GROUP_LIST.listMember()).build());
        }
    }

    static final class DimensionExtListSerializer implements BiConsumer<List<DimensionExt>, ShapeSerializer> {
        static final DimensionExtListSerializer INSTANCE = new DimensionExtListSerializer();

        @Override
        public void accept(List<DimensionExt> values, ShapeSerializer serializer) {
            for (var value : values) {
                serializer.writeStruct(SharedSchemas.DIMENSION_EXT_LIST.listMember(), value);
            }
        }
    }

    static List<DimensionExt> deserializeDimensionExtList(Schema schema, ShapeDeserializer deserializer) {
        var size = deserializer.containerSize();
        List<DimensionExt> result = size == -1 ? new ArrayList<>() : new ArrayList<>(size);
        deserializer.readList(schema, result, DimensionExtList$MemberDeserializer.INSTANCE);
        return result;
    }

    private static final class DimensionExtList$MemberDeserializer implements ShapeDeserializer.ListMemberConsumer<List<DimensionExt>> {
        static final DimensionExtList$MemberDeserializer INSTANCE = new DimensionExtList$MemberDeserializer();

        @Override
        public void accept(List<DimensionExt> state, ShapeDeserializer deserializer) {
            if (deserializer.isNull()) {

                return;
            }
            state.add(DimensionExt.builder().deserializeMember(deserializer, SharedSchemas.DIMENSION_EXT_LIST.listMember()).build());
        }
    }

    static final class ListDefaultConfigOutSerializer implements BiConsumer<List<DefaultConfigFull>, ShapeSerializer> {
        static final ListDefaultConfigOutSerializer INSTANCE = new ListDefaultConfigOutSerializer();

        @Override
        public void accept(List<DefaultConfigFull> values, ShapeSerializer serializer) {
            for (var value : values) {
                serializer.writeStruct(SharedSchemas.LIST_DEFAULT_CONFIG_OUT.listMember(), value);
            }
        }
    }

    static List<DefaultConfigFull> deserializeListDefaultConfigOut(Schema schema, ShapeDeserializer deserializer) {
        var size = deserializer.containerSize();
        List<DefaultConfigFull> result = size == -1 ? new ArrayList<>() : new ArrayList<>(size);
        deserializer.readList(schema, result, ListDefaultConfigOut$MemberDeserializer.INSTANCE);
        return result;
    }

    private static final class ListDefaultConfigOut$MemberDeserializer implements ShapeDeserializer.ListMemberConsumer<List<DefaultConfigFull>> {
        static final ListDefaultConfigOut$MemberDeserializer INSTANCE = new ListDefaultConfigOut$MemberDeserializer();

        @Override
        public void accept(List<DefaultConfigFull> state, ShapeDeserializer deserializer) {
            if (deserializer.isNull()) {

                return;
            }
            state.add(DefaultConfigFull.builder().deserializeMember(deserializer, SharedSchemas.LIST_DEFAULT_CONFIG_OUT.listMember()).build());
        }
    }

    static final class ListMandatoryDimensionsSerializer implements BiConsumer<List<String>, ShapeSerializer> {
        static final ListMandatoryDimensionsSerializer INSTANCE = new ListMandatoryDimensionsSerializer();

        @Override
        public void accept(List<String> values, ShapeSerializer serializer) {
            for (var value : values) {
                serializer.writeString(SharedSchemas.LIST_MANDATORY_DIMENSIONS.listMember(), value);
            }
        }
    }

    static List<String> deserializeListMandatoryDimensions(Schema schema, ShapeDeserializer deserializer) {
        var size = deserializer.containerSize();
        List<String> result = size == -1 ? new ArrayList<>() : new ArrayList<>(size);
        deserializer.readList(schema, result, ListMandatoryDimensions$MemberDeserializer.INSTANCE);
        return result;
    }

    private static final class ListMandatoryDimensions$MemberDeserializer implements ShapeDeserializer.ListMemberConsumer<List<String>> {
        static final ListMandatoryDimensions$MemberDeserializer INSTANCE = new ListMandatoryDimensions$MemberDeserializer();

        @Override
        public void accept(List<String> state, ShapeDeserializer deserializer) {
            if (deserializer.isNull()) {

                return;
            }
            state.add(deserializer.readString(SharedSchemas.LIST_MANDATORY_DIMENSIONS.listMember()));
        }
    }

    static final class EventsSerializer implements BiConsumer<List<String>, ShapeSerializer> {
        static final EventsSerializer INSTANCE = new EventsSerializer();

        @Override
        public void accept(List<String> values, ShapeSerializer serializer) {
            for (var value : values) {
                serializer.writeString(SharedSchemas.EVENTS.listMember(), value);
            }
        }
    }

    static List<String> deserializeEvents(Schema schema, ShapeDeserializer deserializer) {
        var size = deserializer.containerSize();
        List<String> result = size == -1 ? new ArrayList<>() : new ArrayList<>(size);
        deserializer.readList(schema, result, Events$MemberDeserializer.INSTANCE);
        return result;
    }

    private static final class Events$MemberDeserializer implements ShapeDeserializer.ListMemberConsumer<List<String>> {
        static final Events$MemberDeserializer INSTANCE = new Events$MemberDeserializer();

        @Override
        public void accept(List<String> state, ShapeDeserializer deserializer) {
            if (deserializer.isNull()) {

                return;
            }
            state.add(deserializer.readString(SharedSchemas.EVENTS.listMember()));
        }
    }

    static final class WeightRecomputeResponsesSerializer implements BiConsumer<List<WeightRecomputeResponse>, ShapeSerializer> {
        static final WeightRecomputeResponsesSerializer INSTANCE = new WeightRecomputeResponsesSerializer();

        @Override
        public void accept(List<WeightRecomputeResponse> values, ShapeSerializer serializer) {
            for (var value : values) {
                serializer.writeStruct(SharedSchemas.WEIGHT_RECOMPUTE_RESPONSES.listMember(), value);
            }
        }
    }

    static List<WeightRecomputeResponse> deserializeWeightRecomputeResponses(Schema schema, ShapeDeserializer deserializer) {
        var size = deserializer.containerSize();
        List<WeightRecomputeResponse> result = size == -1 ? new ArrayList<>() : new ArrayList<>(size);
        deserializer.readList(schema, result, WeightRecomputeResponses$MemberDeserializer.INSTANCE);
        return result;
    }

    private static final class WeightRecomputeResponses$MemberDeserializer implements ShapeDeserializer.ListMemberConsumer<List<WeightRecomputeResponse>> {
        static final WeightRecomputeResponses$MemberDeserializer INSTANCE = new WeightRecomputeResponses$MemberDeserializer();

        @Override
        public void accept(List<WeightRecomputeResponse> state, ShapeDeserializer deserializer) {
            if (deserializer.isNull()) {

                return;
            }
            state.add(WeightRecomputeResponse.builder().deserializeMember(deserializer, SharedSchemas.WEIGHT_RECOMPUTE_RESPONSES.listMember()).build());
        }
    }

    static final class ListContextOutSerializer implements BiConsumer<List<ContextResponse>, ShapeSerializer> {
        static final ListContextOutSerializer INSTANCE = new ListContextOutSerializer();

        @Override
        public void accept(List<ContextResponse> values, ShapeSerializer serializer) {
            for (var value : values) {
                serializer.writeStruct(SharedSchemas.LIST_CONTEXT_OUT.listMember(), value);
            }
        }
    }

    static List<ContextResponse> deserializeListContextOut(Schema schema, ShapeDeserializer deserializer) {
        var size = deserializer.containerSize();
        List<ContextResponse> result = size == -1 ? new ArrayList<>() : new ArrayList<>(size);
        deserializer.readList(schema, result, ListContextOut$MemberDeserializer.INSTANCE);
        return result;
    }

    private static final class ListContextOut$MemberDeserializer implements ShapeDeserializer.ListMemberConsumer<List<ContextResponse>> {
        static final ListContextOut$MemberDeserializer INSTANCE = new ListContextOut$MemberDeserializer();

        @Override
        public void accept(List<ContextResponse> state, ShapeDeserializer deserializer) {
            if (deserializer.isNull()) {

                return;
            }
            state.add(ContextResponse.builder().deserializeMember(deserializer, SharedSchemas.LIST_CONTEXT_OUT.listMember()).build());
        }
    }

    static final class ListVersionsOutSerializer implements BiConsumer<List<ListVersionsMember>, ShapeSerializer> {
        static final ListVersionsOutSerializer INSTANCE = new ListVersionsOutSerializer();

        @Override
        public void accept(List<ListVersionsMember> values, ShapeSerializer serializer) {
            for (var value : values) {
                serializer.writeStruct(SharedSchemas.LIST_VERSIONS_OUT.listMember(), value);
            }
        }
    }

    static List<ListVersionsMember> deserializeListVersionsOut(Schema schema, ShapeDeserializer deserializer) {
        var size = deserializer.containerSize();
        List<ListVersionsMember> result = size == -1 ? new ArrayList<>() : new ArrayList<>(size);
        deserializer.readList(schema, result, ListVersionsOut$MemberDeserializer.INSTANCE);
        return result;
    }

    private static final class ListVersionsOut$MemberDeserializer implements ShapeDeserializer.ListMemberConsumer<List<ListVersionsMember>> {
        static final ListVersionsOut$MemberDeserializer INSTANCE = new ListVersionsOut$MemberDeserializer();

        @Override
        public void accept(List<ListVersionsMember> state, ShapeDeserializer deserializer) {
            if (deserializer.isNull()) {

                return;
            }
            state.add(ListVersionsMember.builder().deserializeMember(deserializer, SharedSchemas.LIST_VERSIONS_OUT.listMember()).build());
        }
    }

    static final class OverridesMapSerializer implements BiConsumer<Map<String, Map<String, Document>>, MapSerializer> {
        static final OverridesMapSerializer INSTANCE = new OverridesMapSerializer();

        @Override
        public void accept(Map<String, Map<String, Document>> values, MapSerializer serializer) {
            for (var valueEntry : values.entrySet()) {
                serializer.writeEntry(
                    SharedSchemas.OVERRIDES_MAP.mapKeyMember(),
                    valueEntry.getKey(),
                    valueEntry.getValue(),
                    OverridesMap$ValueSerializer.INSTANCE
                );
            }
        }
    }

    private static final class OverridesMap$ValueSerializer implements BiConsumer<Map<String, Document>, ShapeSerializer> {
        private static final OverridesMap$ValueSerializer INSTANCE = new OverridesMap$ValueSerializer();

        @Override
        public void accept(Map<String, Document> values, ShapeSerializer serializer) {
            serializer.writeMap(SharedSchemas.OVERRIDES_MAP.mapValueMember(), values, values.size(), SharedSerde.OverridesSerializer.INSTANCE);
        }
    }

    static Map<String, Map<String, Document>> deserializeOverridesMap(Schema schema, ShapeDeserializer deserializer) {
        var size = deserializer.containerSize();
        Map<String, Map<String, Document>> result = size == -1 ? new LinkedHashMap<>() : new LinkedHashMap<>(size);
        deserializer.readStringMap(schema, result, OverridesMap$ValueDeserializer.INSTANCE);
        return result;
    }

    private static final class OverridesMap$ValueDeserializer implements ShapeDeserializer.MapMemberConsumer<String, Map<String, Map<String, Document>>> {
        static final OverridesMap$ValueDeserializer INSTANCE = new OverridesMap$ValueDeserializer();

        @Override
        public void accept(Map<String, Map<String, Document>> state, String key, ShapeDeserializer deserializer) {
            if (deserializer.isNull()) {
                deserializer.readNull();
                return;
            }
            state.put(key, SharedSerde.deserializeOverrides(SharedSchemas.OVERRIDES_MAP.mapValueMember(), deserializer));
        }
    }

    static final class ObjectShapeSerializer implements BiConsumer<Map<String, Document>, MapSerializer> {
        static final ObjectShapeSerializer INSTANCE = new ObjectShapeSerializer();

        @Override
        public void accept(Map<String, Document> values, MapSerializer serializer) {
            for (var valueEntry : values.entrySet()) {
                serializer.writeEntry(
                    SharedSchemas.OBJECT.mapKeyMember(),
                    valueEntry.getKey(),
                    valueEntry.getValue(),
                    ObjectShape$ValueSerializer.INSTANCE
                );
            }
        }
    }

    private static final class ObjectShape$ValueSerializer implements BiConsumer<Document, ShapeSerializer> {
        private static final ObjectShape$ValueSerializer INSTANCE = new ObjectShape$ValueSerializer();

        @Override
        public void accept(Document values, ShapeSerializer serializer) {
            serializer.writeDocument(SharedSchemas.OBJECT.mapValueMember(), values);
        }
    }

    static Map<String, Document> deserializeObjectShape(Schema schema, ShapeDeserializer deserializer) {
        var size = deserializer.containerSize();
        Map<String, Document> result = size == -1 ? new LinkedHashMap<>() : new LinkedHashMap<>(size);
        deserializer.readStringMap(schema, result, ObjectShape$ValueDeserializer.INSTANCE);
        return result;
    }

    private static final class ObjectShape$ValueDeserializer implements ShapeDeserializer.MapMemberConsumer<String, Map<String, Document>> {
        static final ObjectShape$ValueDeserializer INSTANCE = new ObjectShape$ValueDeserializer();

        @Override
        public void accept(Map<String, Document> state, String key, ShapeDeserializer deserializer) {
            if (deserializer.isNull()) {
                deserializer.readNull();
                return;
            }
            state.put(key, deserializer.readDocument());
        }
    }

    static final class ContextListSerializer implements BiConsumer<List<ContextPartial>, ShapeSerializer> {
        static final ContextListSerializer INSTANCE = new ContextListSerializer();

        @Override
        public void accept(List<ContextPartial> values, ShapeSerializer serializer) {
            for (var value : values) {
                serializer.writeStruct(SharedSchemas.CONTEXT_LIST.listMember(), value);
            }
        }
    }

    static List<ContextPartial> deserializeContextList(Schema schema, ShapeDeserializer deserializer) {
        var size = deserializer.containerSize();
        List<ContextPartial> result = size == -1 ? new ArrayList<>() : new ArrayList<>(size);
        deserializer.readList(schema, result, ContextList$MemberDeserializer.INSTANCE);
        return result;
    }

    private static final class ContextList$MemberDeserializer implements ShapeDeserializer.ListMemberConsumer<List<ContextPartial>> {
        static final ContextList$MemberDeserializer INSTANCE = new ContextList$MemberDeserializer();

        @Override
        public void accept(List<ContextPartial> state, ShapeDeserializer deserializer) {
            if (deserializer.isNull()) {

                return;
            }
            state.add(ContextPartial.builder().deserializeMember(deserializer, SharedSchemas.CONTEXT_LIST.listMember()).build());
        }
    }

    static final class OverrideWithKeysSerializer implements BiConsumer<List<String>, ShapeSerializer> {
        static final OverrideWithKeysSerializer INSTANCE = new OverrideWithKeysSerializer();

        @Override
        public void accept(List<String> values, ShapeSerializer serializer) {
            for (var value : values) {
                serializer.writeString(SharedSchemas.OVERRIDE_WITH_KEYS.listMember(), value);
            }
        }
    }

    static List<String> deserializeOverrideWithKeys(Schema schema, ShapeDeserializer deserializer) {
        var size = deserializer.containerSize();
        List<String> result = size == -1 ? new ArrayList<>() : new ArrayList<>(size);
        deserializer.readList(schema, result, OverrideWithKeys$MemberDeserializer.INSTANCE);
        return result;
    }

    private static final class OverrideWithKeys$MemberDeserializer implements ShapeDeserializer.ListMemberConsumer<List<String>> {
        static final OverrideWithKeys$MemberDeserializer INSTANCE = new OverrideWithKeys$MemberDeserializer();

        @Override
        public void accept(List<String> state, ShapeDeserializer deserializer) {
            if (deserializer.isNull()) {

                return;
            }
            state.add(deserializer.readString(SharedSchemas.OVERRIDE_WITH_KEYS.listMember()));
        }
    }

    static final class ContextMapSerializer implements BiConsumer<Map<String, Document>, MapSerializer> {
        static final ContextMapSerializer INSTANCE = new ContextMapSerializer();

        @Override
        public void accept(Map<String, Document> values, MapSerializer serializer) {
            for (var valueEntry : values.entrySet()) {
                serializer.writeEntry(
                    SharedSchemas.CONTEXT_MAP.mapKeyMember(),
                    valueEntry.getKey(),
                    valueEntry.getValue(),
                    ContextMap$ValueSerializer.INSTANCE
                );
            }
        }
    }

    private static final class ContextMap$ValueSerializer implements BiConsumer<Document, ShapeSerializer> {
        private static final ContextMap$ValueSerializer INSTANCE = new ContextMap$ValueSerializer();

        @Override
        public void accept(Document values, ShapeSerializer serializer) {
            serializer.writeDocument(SharedSchemas.CONTEXT_MAP.mapValueMember(), values);
        }
    }

    static Map<String, Document> deserializeContextMap(Schema schema, ShapeDeserializer deserializer) {
        var size = deserializer.containerSize();
        Map<String, Document> result = size == -1 ? new LinkedHashMap<>() : new LinkedHashMap<>(size);
        deserializer.readStringMap(schema, result, ContextMap$ValueDeserializer.INSTANCE);
        return result;
    }

    private static final class ContextMap$ValueDeserializer implements ShapeDeserializer.MapMemberConsumer<String, Map<String, Document>> {
        static final ContextMap$ValueDeserializer INSTANCE = new ContextMap$ValueDeserializer();

        @Override
        public void accept(Map<String, Document> state, String key, ShapeDeserializer deserializer) {
            if (deserializer.isNull()) {
                deserializer.readNull();
                return;
            }
            state.put(key, deserializer.readDocument());
        }
    }

    static final class ListOverrideKeysSerializer implements BiConsumer<List<String>, ShapeSerializer> {
        static final ListOverrideKeysSerializer INSTANCE = new ListOverrideKeysSerializer();

        @Override
        public void accept(List<String> values, ShapeSerializer serializer) {
            for (var value : values) {
                serializer.writeString(SharedSchemas.LIST_OVERRIDE_KEYS.listMember(), value);
            }
        }
    }

    static List<String> deserializeListOverrideKeys(Schema schema, ShapeDeserializer deserializer) {
        var size = deserializer.containerSize();
        List<String> result = size == -1 ? new ArrayList<>() : new ArrayList<>(size);
        deserializer.readList(schema, result, ListOverrideKeys$MemberDeserializer.INSTANCE);
        return result;
    }

    private static final class ListOverrideKeys$MemberDeserializer implements ShapeDeserializer.ListMemberConsumer<List<String>> {
        static final ListOverrideKeys$MemberDeserializer INSTANCE = new ListOverrideKeys$MemberDeserializer();

        @Override
        public void accept(List<String> state, ShapeDeserializer deserializer) {
            if (deserializer.isNull()) {

                return;
            }
            state.add(deserializer.readString(SharedSchemas.LIST_OVERRIDE_KEYS.listMember()));
        }
    }

    static final class BulkOperationOutListSerializer implements BiConsumer<List<ContextActionOut>, ShapeSerializer> {
        static final BulkOperationOutListSerializer INSTANCE = new BulkOperationOutListSerializer();

        @Override
        public void accept(List<ContextActionOut> values, ShapeSerializer serializer) {
            for (var value : values) {
                serializer.writeStruct(SharedSchemas.BULK_OPERATION_OUT_LIST.listMember(), value);
            }
        }
    }

    static List<ContextActionOut> deserializeBulkOperationOutList(Schema schema, ShapeDeserializer deserializer) {
        var size = deserializer.containerSize();
        List<ContextActionOut> result = size == -1 ? new ArrayList<>() : new ArrayList<>(size);
        deserializer.readList(schema, result, BulkOperationOutList$MemberDeserializer.INSTANCE);
        return result;
    }

    private static final class BulkOperationOutList$MemberDeserializer implements ShapeDeserializer.ListMemberConsumer<List<ContextActionOut>> {
        static final BulkOperationOutList$MemberDeserializer INSTANCE = new BulkOperationOutList$MemberDeserializer();

        @Override
        public void accept(List<ContextActionOut> state, ShapeDeserializer deserializer) {
            if (deserializer.isNull()) {

                return;
            }
            state.add(ContextActionOut.builder().deserializeMember(deserializer, SharedSchemas.BULK_OPERATION_OUT_LIST.listMember()).build());
        }
    }

    static final class BulkOperationListSerializer implements BiConsumer<List<ContextAction>, ShapeSerializer> {
        static final BulkOperationListSerializer INSTANCE = new BulkOperationListSerializer();

        @Override
        public void accept(List<ContextAction> values, ShapeSerializer serializer) {
            for (var value : values) {
                serializer.writeStruct(SharedSchemas.BULK_OPERATION_LIST.listMember(), value);
            }
        }
    }

    static List<ContextAction> deserializeBulkOperationList(Schema schema, ShapeDeserializer deserializer) {
        var size = deserializer.containerSize();
        List<ContextAction> result = size == -1 ? new ArrayList<>() : new ArrayList<>(size);
        deserializer.readList(schema, result, BulkOperationList$MemberDeserializer.INSTANCE);
        return result;
    }

    private static final class BulkOperationList$MemberDeserializer implements ShapeDeserializer.ListMemberConsumer<List<ContextAction>> {
        static final BulkOperationList$MemberDeserializer INSTANCE = new BulkOperationList$MemberDeserializer();

        @Override
        public void accept(List<ContextAction> state, ShapeDeserializer deserializer) {
            if (deserializer.isNull()) {

                return;
            }
            state.add(ContextAction.builder().deserializeMember(deserializer, SharedSchemas.BULK_OPERATION_LIST.listMember()).build());
        }
    }

    static final class OverridesSerializer implements BiConsumer<Map<String, Document>, MapSerializer> {
        static final OverridesSerializer INSTANCE = new OverridesSerializer();

        @Override
        public void accept(Map<String, Document> values, MapSerializer serializer) {
            for (var valueEntry : values.entrySet()) {
                serializer.writeEntry(
                    SharedSchemas.OVERRIDES.mapKeyMember(),
                    valueEntry.getKey(),
                    valueEntry.getValue(),
                    Overrides$ValueSerializer.INSTANCE
                );
            }
        }
    }

    private static final class Overrides$ValueSerializer implements BiConsumer<Document, ShapeSerializer> {
        private static final Overrides$ValueSerializer INSTANCE = new Overrides$ValueSerializer();

        @Override
        public void accept(Document values, ShapeSerializer serializer) {
            serializer.writeDocument(SharedSchemas.OVERRIDES.mapValueMember(), values);
        }
    }

    static Map<String, Document> deserializeOverrides(Schema schema, ShapeDeserializer deserializer) {
        var size = deserializer.containerSize();
        Map<String, Document> result = size == -1 ? new LinkedHashMap<>() : new LinkedHashMap<>(size);
        deserializer.readStringMap(schema, result, Overrides$ValueDeserializer.INSTANCE);
        return result;
    }

    private static final class Overrides$ValueDeserializer implements ShapeDeserializer.MapMemberConsumer<String, Map<String, Document>> {
        static final Overrides$ValueDeserializer INSTANCE = new Overrides$ValueDeserializer();

        @Override
        public void accept(Map<String, Document> state, String key, ShapeDeserializer deserializer) {
            if (deserializer.isNull()) {
                deserializer.readNull();
                return;
            }
            state.put(key, deserializer.readDocument());
        }
    }

    static final class AuditLogListSerializer implements BiConsumer<List<AuditLogFull>, ShapeSerializer> {
        static final AuditLogListSerializer INSTANCE = new AuditLogListSerializer();

        @Override
        public void accept(List<AuditLogFull> values, ShapeSerializer serializer) {
            for (var value : values) {
                serializer.writeStruct(SharedSchemas.AUDIT_LOG_LIST.listMember(), value);
            }
        }
    }

    static List<AuditLogFull> deserializeAuditLogList(Schema schema, ShapeDeserializer deserializer) {
        var size = deserializer.containerSize();
        List<AuditLogFull> result = size == -1 ? new ArrayList<>() : new ArrayList<>(size);
        deserializer.readList(schema, result, AuditLogList$MemberDeserializer.INSTANCE);
        return result;
    }

    private static final class AuditLogList$MemberDeserializer implements ShapeDeserializer.ListMemberConsumer<List<AuditLogFull>> {
        static final AuditLogList$MemberDeserializer INSTANCE = new AuditLogList$MemberDeserializer();

        @Override
        public void accept(List<AuditLogFull> state, ShapeDeserializer deserializer) {
            if (deserializer.isNull()) {

                return;
            }
            state.add(AuditLogFull.builder().deserializeMember(deserializer, SharedSchemas.AUDIT_LOG_LIST.listMember()).build());
        }
    }

    static final class ListVariantSerializer implements BiConsumer<List<Variant>, ShapeSerializer> {
        static final ListVariantSerializer INSTANCE = new ListVariantSerializer();

        @Override
        public void accept(List<Variant> values, ShapeSerializer serializer) {
            for (var value : values) {
                serializer.writeStruct(SharedSchemas.LIST_VARIANT.listMember(), value);
            }
        }
    }

    static List<Variant> deserializeListVariant(Schema schema, ShapeDeserializer deserializer) {
        var size = deserializer.containerSize();
        List<Variant> result = size == -1 ? new ArrayList<>() : new ArrayList<>(size);
        deserializer.readList(schema, result, ListVariant$MemberDeserializer.INSTANCE);
        return result;
    }

    private static final class ListVariant$MemberDeserializer implements ShapeDeserializer.ListMemberConsumer<List<Variant>> {
        static final ListVariant$MemberDeserializer INSTANCE = new ListVariant$MemberDeserializer();

        @Override
        public void accept(List<Variant> state, ShapeDeserializer deserializer) {
            if (deserializer.isNull()) {

                return;
            }
            state.add(Variant.builder().deserializeMember(deserializer, SharedSchemas.LIST_VARIANT.listMember()).build());
        }
    }

    static final class ConditionSerializer implements BiConsumer<Map<String, Document>, MapSerializer> {
        static final ConditionSerializer INSTANCE = new ConditionSerializer();

        @Override
        public void accept(Map<String, Document> values, MapSerializer serializer) {
            for (var valueEntry : values.entrySet()) {
                serializer.writeEntry(
                    SharedSchemas.CONDITION.mapKeyMember(),
                    valueEntry.getKey(),
                    valueEntry.getValue(),
                    Condition$ValueSerializer.INSTANCE
                );
            }
        }
    }

    private static final class Condition$ValueSerializer implements BiConsumer<Document, ShapeSerializer> {
        private static final Condition$ValueSerializer INSTANCE = new Condition$ValueSerializer();

        @Override
        public void accept(Document values, ShapeSerializer serializer) {
            serializer.writeDocument(SharedSchemas.CONDITION.mapValueMember(), values);
        }
    }

    static Map<String, Document> deserializeCondition(Schema schema, ShapeDeserializer deserializer) {
        var size = deserializer.containerSize();
        Map<String, Document> result = size == -1 ? new LinkedHashMap<>() : new LinkedHashMap<>(size);
        deserializer.readStringMap(schema, result, Condition$ValueDeserializer.INSTANCE);
        return result;
    }

    private static final class Condition$ValueDeserializer implements ShapeDeserializer.MapMemberConsumer<String, Map<String, Document>> {
        static final Condition$ValueDeserializer INSTANCE = new Condition$ValueDeserializer();

        @Override
        public void accept(Map<String, Document> state, String key, ShapeDeserializer deserializer) {
            if (deserializer.isNull()) {
                deserializer.readNull();
                return;
            }
            state.put(key, deserializer.readDocument());
        }
    }

    static final class BucketsSerializer implements BiConsumer<List<Bucket>, ShapeSerializer> {
        static final BucketsSerializer INSTANCE = new BucketsSerializer();

        @Override
        public void accept(List<Bucket> values, ShapeSerializer serializer) {
            for (var value : values) {
                if (value == null) {
                    serializer.writeNull(SharedSchemas.BUCKETS.listMember());
                    continue;
                }
                serializer.writeStruct(SharedSchemas.BUCKETS.listMember(), value);
            }
        }
    }

    static List<Bucket> deserializeBuckets(Schema schema, ShapeDeserializer deserializer) {
        var size = deserializer.containerSize();
        List<Bucket> result = size == -1 ? new ArrayList<>() : new ArrayList<>(size);
        deserializer.readList(schema, result, Buckets$MemberDeserializer.INSTANCE);
        return result;
    }

    private static final class Buckets$MemberDeserializer implements ShapeDeserializer.ListMemberConsumer<List<Bucket>> {
        static final Buckets$MemberDeserializer INSTANCE = new Buckets$MemberDeserializer();

        @Override
        public void accept(List<Bucket> state, ShapeDeserializer deserializer) {
            if (deserializer.isNull()) {
                state.add(deserializer.readNull());
                return;
            }
            state.add(Bucket.builder().deserializeMember(deserializer, SharedSchemas.BUCKETS.listMember()).build());
        }
    }

    static final class StringListSerializer implements BiConsumer<List<String>, ShapeSerializer> {
        static final StringListSerializer INSTANCE = new StringListSerializer();

        @Override
        public void accept(List<String> values, ShapeSerializer serializer) {
            for (var value : values) {
                serializer.writeString(SharedSchemas.STRING_LIST.listMember(), value);
            }
        }
    }

    static List<String> deserializeStringList(Schema schema, ShapeDeserializer deserializer) {
        var size = deserializer.containerSize();
        List<String> result = size == -1 ? new ArrayList<>() : new ArrayList<>(size);
        deserializer.readList(schema, result, StringList$MemberDeserializer.INSTANCE);
        return result;
    }

    private static final class StringList$MemberDeserializer implements ShapeDeserializer.ListMemberConsumer<List<String>> {
        static final StringList$MemberDeserializer INSTANCE = new StringList$MemberDeserializer();

        @Override
        public void accept(List<String> state, ShapeDeserializer deserializer) {
            if (deserializer.isNull()) {

                return;
            }
            state.add(deserializer.readString(SharedSchemas.STRING_LIST.listMember()));
        }
    }

    private SharedSerde() {}
}

