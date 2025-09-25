
package io.juspay.superposition.model;

import java.time.Instant;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import software.amazon.smithy.java.core.schema.PreludeSchemas;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.java.core.schema.SchemaUtils;
import software.amazon.smithy.java.core.schema.SerializableStruct;
import software.amazon.smithy.java.core.schema.ShapeBuilder;
import software.amazon.smithy.java.core.serde.ShapeDeserializer;
import software.amazon.smithy.java.core.serde.ShapeSerializer;
import software.amazon.smithy.java.core.serde.ToStringSerializer;
import software.amazon.smithy.java.core.serde.document.Document;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.HttpHeaderTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class GetConfigOutput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#GetConfigOutput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("contexts", SharedSchemas.CONTEXT_LIST)
        .putMember("overrides", SharedSchemas.OVERRIDES_MAP)
        .putMember("default_configs", SharedSchemas.OBJECT)
        .putMember("dimensions", SharedSchemas.DIMENSION_DATA)
        .putMember("version", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-config-version"))
        .putMember("last_modified", SharedSchemas.DATE_TIME,
                new HttpHeaderTrait("last-modified"))
        .putMember("audit_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-audit-id"))
        .build();

    private static final Schema $SCHEMA_CONTEXTS = $SCHEMA.member("contexts");
    private static final Schema $SCHEMA_OVERRIDES = $SCHEMA.member("overrides");
    private static final Schema $SCHEMA_DEFAULT_CONFIGS = $SCHEMA.member("default_configs");
    private static final Schema $SCHEMA_DIMENSIONS = $SCHEMA.member("dimensions");
    private static final Schema $SCHEMA_VERSION = $SCHEMA.member("version");
    private static final Schema $SCHEMA_LAST_MODIFIED = $SCHEMA.member("last_modified");
    private static final Schema $SCHEMA_AUDIT_ID = $SCHEMA.member("audit_id");

    private final transient List<ContextPartial> contexts;
    private final transient Map<String, Map<String, Document>> overrides;
    private final transient Map<String, Document> defaultConfigs;
    private final transient Map<String, Document> dimensions;
    private final transient String version;
    private final transient Instant lastModified;
    private final transient String auditId;

    private GetConfigOutput(Builder builder) {
        this.contexts = builder.contexts == null ? null : Collections.unmodifiableList(builder.contexts);
        this.overrides = builder.overrides == null ? null : Collections.unmodifiableMap(builder.overrides);
        this.defaultConfigs = builder.defaultConfigs == null ? null : Collections.unmodifiableMap(builder.defaultConfigs);
        this.dimensions = builder.dimensions == null ? null : Collections.unmodifiableMap(builder.dimensions);
        this.version = builder.version;
        this.lastModified = builder.lastModified;
        this.auditId = builder.auditId;
    }

    public List<ContextPartial> contexts() {
        if (contexts == null) {
            return Collections.emptyList();
        }
        return contexts;
    }

    public boolean hasContexts() {
        return contexts != null;
    }

    public Map<String, Map<String, Document>> overrides() {
        if (overrides == null) {
            return Collections.emptyMap();
        }
        return overrides;
    }

    public boolean hasOverrides() {
        return overrides != null;
    }

    public Map<String, Document> defaultConfigs() {
        if (defaultConfigs == null) {
            return Collections.emptyMap();
        }
        return defaultConfigs;
    }

    public boolean hasDefaultConfigs() {
        return defaultConfigs != null;
    }

    public Map<String, Document> dimensions() {
        if (dimensions == null) {
            return Collections.emptyMap();
        }
        return dimensions;
    }

    public boolean hasDimensions() {
        return dimensions != null;
    }

    public String version() {
        return version;
    }

    public Instant lastModified() {
        return lastModified;
    }

    public String auditId() {
        return auditId;
    }

    @Override
    public String toString() {
        return ToStringSerializer.serialize(this);
    }

    @Override
    public boolean equals(Object other) {
        if (other == this) {
            return true;
        }
        if (other == null || getClass() != other.getClass()) {
            return false;
        }
        GetConfigOutput that = (GetConfigOutput) other;
        return Objects.equals(this.contexts, that.contexts)
               && Objects.equals(this.overrides, that.overrides)
               && Objects.equals(this.defaultConfigs, that.defaultConfigs)
               && Objects.equals(this.dimensions, that.dimensions)
               && Objects.equals(this.version, that.version)
               && Objects.equals(this.lastModified, that.lastModified)
               && Objects.equals(this.auditId, that.auditId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(contexts, overrides, defaultConfigs, dimensions, version, lastModified, auditId);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        if (contexts != null) {
            serializer.writeList($SCHEMA_CONTEXTS, contexts, contexts.size(), SharedSerde.ContextListSerializer.INSTANCE);
        }
        if (overrides != null) {
            serializer.writeMap($SCHEMA_OVERRIDES, overrides, overrides.size(), SharedSerde.OverridesMapSerializer.INSTANCE);
        }
        if (defaultConfigs != null) {
            serializer.writeMap($SCHEMA_DEFAULT_CONFIGS, defaultConfigs, defaultConfigs.size(), SharedSerde.ObjectShapeSerializer.INSTANCE);
        }
        if (dimensions != null) {
            serializer.writeMap($SCHEMA_DIMENSIONS, dimensions, dimensions.size(), SharedSerde.DimensionDataSerializer.INSTANCE);
        }
        if (version != null) {
            serializer.writeString($SCHEMA_VERSION, version);
        }
        if (lastModified != null) {
            serializer.writeTimestamp($SCHEMA_LAST_MODIFIED, lastModified);
        }
        if (auditId != null) {
            serializer.writeString($SCHEMA_AUDIT_ID, auditId);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONTEXTS, member, contexts);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_OVERRIDES, member, overrides);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_DEFAULT_CONFIGS, member, defaultConfigs);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_DIMENSIONS, member, dimensions);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_VERSION, member, version);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED, member, lastModified);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_AUDIT_ID, member, auditId);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link GetConfigOutput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.contexts(this.contexts);
        builder.overrides(this.overrides);
        builder.defaultConfigs(this.defaultConfigs);
        builder.dimensions(this.dimensions);
        builder.version(this.version);
        builder.lastModified(this.lastModified);
        builder.auditId(this.auditId);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link GetConfigOutput}.
     */
    public static final class Builder implements ShapeBuilder<GetConfigOutput> {
        private List<ContextPartial> contexts;
        private Map<String, Map<String, Document>> overrides;
        private Map<String, Document> defaultConfigs;
        private Map<String, Document> dimensions;
        private String version;
        private Instant lastModified;
        private String auditId;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * @return this builder.
         */
        public Builder contexts(List<ContextPartial> contexts) {
            this.contexts = contexts;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder overrides(Map<String, Map<String, Document>> overrides) {
            this.overrides = overrides;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder defaultConfigs(Map<String, Document> defaultConfigs) {
            this.defaultConfigs = defaultConfigs;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder dimensions(Map<String, Document> dimensions) {
            this.dimensions = dimensions;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder version(String version) {
            this.version = version;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder lastModified(Instant lastModified) {
            this.lastModified = lastModified;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder auditId(String auditId) {
            this.auditId = auditId;
            return this;
        }

        @Override
        public GetConfigOutput build() {
            return new GetConfigOutput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> contexts((List<ContextPartial>) SchemaUtils.validateSameMember($SCHEMA_CONTEXTS, member, value));
                case 1 -> overrides((Map<String, Map<String, Document>>) SchemaUtils.validateSameMember($SCHEMA_OVERRIDES, member, value));
                case 2 -> defaultConfigs((Map<String, Document>) SchemaUtils.validateSameMember($SCHEMA_DEFAULT_CONFIGS, member, value));
                case 3 -> dimensions((Map<String, Document>) SchemaUtils.validateSameMember($SCHEMA_DIMENSIONS, member, value));
                case 4 -> version((String) SchemaUtils.validateSameMember($SCHEMA_VERSION, member, value));
                case 5 -> lastModified((Instant) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED, member, value));
                case 6 -> auditId((String) SchemaUtils.validateSameMember($SCHEMA_AUDIT_ID, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public Builder deserialize(ShapeDeserializer decoder) {
            decoder.readStruct($SCHEMA, this, $InnerDeserializer.INSTANCE);
            return this;
        }

        @Override
        public Builder deserializeMember(ShapeDeserializer decoder, Schema schema) {
            decoder.readStruct(schema.assertMemberTargetIs($SCHEMA), this, $InnerDeserializer.INSTANCE);
            return this;
        }

        private static final class $InnerDeserializer implements ShapeDeserializer.StructMemberConsumer<Builder> {
            private static final $InnerDeserializer INSTANCE = new $InnerDeserializer();

            @Override
            public void accept(Builder builder, Schema member, ShapeDeserializer de) {
                switch (member.memberIndex()) {
                    case 0 -> builder.contexts(SharedSerde.deserializeContextList(member, de));
                    case 1 -> builder.overrides(SharedSerde.deserializeOverridesMap(member, de));
                    case 2 -> builder.defaultConfigs(SharedSerde.deserializeObjectShape(member, de));
                    case 3 -> builder.dimensions(SharedSerde.deserializeDimensionData(member, de));
                    case 4 -> builder.version(de.readString(member));
                    case 5 -> builder.lastModified(de.readTimestamp(member));
                    case 6 -> builder.auditId(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

