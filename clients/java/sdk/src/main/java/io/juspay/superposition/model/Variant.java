
package io.juspay.superposition.model;

import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import software.amazon.smithy.java.core.schema.PreludeSchemas;
import software.amazon.smithy.java.core.schema.PresenceTracker;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.java.core.schema.SchemaUtils;
import software.amazon.smithy.java.core.schema.SerializableStruct;
import software.amazon.smithy.java.core.schema.ShapeBuilder;
import software.amazon.smithy.java.core.serde.ShapeDeserializer;
import software.amazon.smithy.java.core.serde.ShapeSerializer;
import software.amazon.smithy.java.core.serde.ToStringSerializer;
import software.amazon.smithy.java.core.serde.document.Document;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class Variant implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#Variant");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("id", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("variant_type", VariantType.$SCHEMA,
                new RequiredTrait())
        .putMember("context_id", PreludeSchemas.STRING)
        .putMember("override_id", PreludeSchemas.STRING)
        .putMember("overrides", SharedSchemas.OVERRIDES,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_ID = $SCHEMA.member("id");
    private static final Schema $SCHEMA_VARIANT_TYPE = $SCHEMA.member("variant_type");
    private static final Schema $SCHEMA_CONTEXT_ID = $SCHEMA.member("context_id");
    private static final Schema $SCHEMA_OVERRIDE_ID = $SCHEMA.member("override_id");
    private static final Schema $SCHEMA_OVERRIDES = $SCHEMA.member("overrides");

    private final transient String id;
    private final transient VariantType variantType;
    private final transient String contextId;
    private final transient String overrideId;
    private final transient Map<String, Document> overrides;

    private Variant(Builder builder) {
        this.id = builder.id;
        this.variantType = builder.variantType;
        this.contextId = builder.contextId;
        this.overrideId = builder.overrideId;
        this.overrides = Collections.unmodifiableMap(builder.overrides);
    }

    public String id() {
        return id;
    }

    public VariantType variantType() {
        return variantType;
    }

    public String contextId() {
        return contextId;
    }

    public String overrideId() {
        return overrideId;
    }

    public Map<String, Document> overrides() {
        return overrides;
    }

    public boolean hasOverrides() {
        return true;
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
        Variant that = (Variant) other;
        return Objects.equals(this.id, that.id)
               && Objects.equals(this.variantType, that.variantType)
               && Objects.equals(this.contextId, that.contextId)
               && Objects.equals(this.overrideId, that.overrideId)
               && Objects.equals(this.overrides, that.overrides);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, variantType, contextId, overrideId, overrides);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_ID, id);
        serializer.writeString($SCHEMA_VARIANT_TYPE, variantType.value());
        if (contextId != null) {
            serializer.writeString($SCHEMA_CONTEXT_ID, contextId);
        }
        if (overrideId != null) {
            serializer.writeString($SCHEMA_OVERRIDE_ID, overrideId);
        }
        serializer.writeMap($SCHEMA_OVERRIDES, overrides, overrides.size(), SharedSerde.OverridesSerializer.INSTANCE);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_ID, member, id);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_VARIANT_TYPE, member, variantType);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_OVERRIDES, member, overrides);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONTEXT_ID, member, contextId);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_OVERRIDE_ID, member, overrideId);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link Variant}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.id(this.id);
        builder.variantType(this.variantType);
        builder.contextId(this.contextId);
        builder.overrideId(this.overrideId);
        builder.overrides(this.overrides);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link Variant}.
     */
    public static final class Builder implements ShapeBuilder<Variant> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String id;
        private VariantType variantType;
        private String contextId;
        private String overrideId;
        private Map<String, Document> overrides;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder id(String id) {
            this.id = Objects.requireNonNull(id, "id cannot be null");
            tracker.setMember($SCHEMA_ID);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder variantType(VariantType variantType) {
            this.variantType = Objects.requireNonNull(variantType, "variantType cannot be null");
            tracker.setMember($SCHEMA_VARIANT_TYPE);
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder contextId(String contextId) {
            this.contextId = contextId;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder overrideId(String overrideId) {
            this.overrideId = overrideId;
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder overrides(Map<String, Document> overrides) {
            this.overrides = Objects.requireNonNull(overrides, "overrides cannot be null");
            tracker.setMember($SCHEMA_OVERRIDES);
            return this;
        }

        @Override
        public Variant build() {
            tracker.validate();
            return new Variant(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> id((String) SchemaUtils.validateSameMember($SCHEMA_ID, member, value));
                case 1 -> variantType((VariantType) SchemaUtils.validateSameMember($SCHEMA_VARIANT_TYPE, member, value));
                case 2 -> overrides((Map<String, Document>) SchemaUtils.validateSameMember($SCHEMA_OVERRIDES, member, value));
                case 3 -> contextId((String) SchemaUtils.validateSameMember($SCHEMA_CONTEXT_ID, member, value));
                case 4 -> overrideId((String) SchemaUtils.validateSameMember($SCHEMA_OVERRIDE_ID, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<Variant> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_ID)) {
                id("");
            }
            if (!tracker.checkMember($SCHEMA_VARIANT_TYPE)) {
                variantType(VariantType.unknown(""));
            }
            if (!tracker.checkMember($SCHEMA_OVERRIDES)) {
                overrides(Collections.emptyMap());
            }
            return this;
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
                    case 0 -> builder.id(de.readString(member));
                    case 1 -> builder.variantType(VariantType.builder().deserializeMember(de, member).build());
                    case 2 -> builder.overrides(SharedSerde.deserializeOverrides(member, de));
                    case 3 -> builder.contextId(de.readString(member));
                    case 4 -> builder.overrideId(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

