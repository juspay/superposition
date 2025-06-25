
package io.juspay.superposition.model;

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
public final class VariantUpdateRequest implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#VariantUpdateRequest");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("id", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("overrides", PreludeSchemas.DOCUMENT,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_ID = $SCHEMA.member("id");
    private static final Schema $SCHEMA_OVERRIDES = $SCHEMA.member("overrides");

    private final transient String id;
    private final transient Document overrides;

    private VariantUpdateRequest(Builder builder) {
        this.id = builder.id;
        this.overrides = builder.overrides;
    }

    public String id() {
        return id;
    }

    public Document overrides() {
        return overrides;
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
        VariantUpdateRequest that = (VariantUpdateRequest) other;
        return Objects.equals(this.id, that.id)
               && Objects.equals(this.overrides, that.overrides);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, overrides);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_ID, id);
        serializer.writeDocument($SCHEMA_OVERRIDES, overrides);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_ID, member, id);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_OVERRIDES, member, overrides);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link VariantUpdateRequest}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.id(this.id);
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
     * Builder for {@link VariantUpdateRequest}.
     */
    public static final class Builder implements ShapeBuilder<VariantUpdateRequest> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String id;
        private Document overrides;

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
        public Builder overrides(Document overrides) {
            this.overrides = Objects.requireNonNull(overrides, "overrides cannot be null");
            tracker.setMember($SCHEMA_OVERRIDES);
            return this;
        }

        @Override
        public VariantUpdateRequest build() {
            tracker.validate();
            return new VariantUpdateRequest(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> id((String) SchemaUtils.validateSameMember($SCHEMA_ID, member, value));
                case 1 -> overrides((Document) SchemaUtils.validateSameMember($SCHEMA_OVERRIDES, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<VariantUpdateRequest> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_ID)) {
                id("");
            }
            if (!tracker.checkMember($SCHEMA_OVERRIDES)) {
                tracker.setMember($SCHEMA_OVERRIDES);
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
                    case 1 -> builder.overrides(de.readDocument());
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

