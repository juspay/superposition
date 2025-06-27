
package io.juspay.superposition.model;

import java.util.Objects;
import software.amazon.smithy.java.core.schema.PreludeSchemas;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.java.core.schema.SchemaUtils;
import software.amazon.smithy.java.core.schema.SerializableStruct;
import software.amazon.smithy.java.core.schema.ShapeBuilder;
import software.amazon.smithy.java.core.serde.ShapeDeserializer;
import software.amazon.smithy.java.core.serde.ShapeSerializer;
import software.amazon.smithy.java.core.serde.ToStringSerializer;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class Bucket implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#Bucket");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("experiment_id", PreludeSchemas.STRING)
        .putMember("variant", PreludeSchemas.STRING)
        .build();

    private static final Schema $SCHEMA_EXPERIMENT_ID = $SCHEMA.member("experiment_id");
    private static final Schema $SCHEMA_VARIANT = $SCHEMA.member("variant");

    private final transient String experimentId;
    private final transient String variant;

    private Bucket(Builder builder) {
        this.experimentId = builder.experimentId;
        this.variant = builder.variant;
    }

    public String experimentId() {
        return experimentId;
    }

    public String variant() {
        return variant;
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
        Bucket that = (Bucket) other;
        return Objects.equals(this.experimentId, that.experimentId)
               && Objects.equals(this.variant, that.variant);
    }

    @Override
    public int hashCode() {
        return Objects.hash(experimentId, variant);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        if (experimentId != null) {
            serializer.writeString($SCHEMA_EXPERIMENT_ID, experimentId);
        }
        if (variant != null) {
            serializer.writeString($SCHEMA_VARIANT, variant);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_EXPERIMENT_ID, member, experimentId);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_VARIANT, member, variant);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link Bucket}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.experimentId(this.experimentId);
        builder.variant(this.variant);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link Bucket}.
     */
    public static final class Builder implements ShapeBuilder<Bucket> {
        private String experimentId;
        private String variant;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * @return this builder.
         */
        public Builder experimentId(String experimentId) {
            this.experimentId = experimentId;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder variant(String variant) {
            this.variant = variant;
            return this;
        }

        @Override
        public Bucket build() {
            return new Bucket(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> experimentId((String) SchemaUtils.validateSameMember($SCHEMA_EXPERIMENT_ID, member, value));
                case 1 -> variant((String) SchemaUtils.validateSameMember($SCHEMA_VARIANT, member, value));
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
                    case 0 -> builder.experimentId(de.readString(member));
                    case 1 -> builder.variant(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

