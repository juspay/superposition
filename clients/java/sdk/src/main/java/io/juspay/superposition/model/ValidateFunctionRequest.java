
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
import software.amazon.smithy.java.core.serde.document.Document;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class ValidateFunctionRequest implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ValidateFunctionRequest");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("key", PreludeSchemas.STRING)
        .putMember("value", PreludeSchemas.DOCUMENT)
        .build();

    private static final Schema $SCHEMA_KEY = $SCHEMA.member("key");
    private static final Schema $SCHEMA_VALUE = $SCHEMA.member("value");

    private final transient String key;
    private final transient Document value;

    private ValidateFunctionRequest(Builder builder) {
        this.key = builder.key;
        this.value = builder.value;
    }

    public String key() {
        return key;
    }

    public Document value() {
        return value;
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
        ValidateFunctionRequest that = (ValidateFunctionRequest) other;
        return Objects.equals(this.key, that.key)
               && Objects.equals(this.value, that.value);
    }

    @Override
    public int hashCode() {
        return Objects.hash(key, value);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        if (key != null) {
            serializer.writeString($SCHEMA_KEY, key);
        }
        if (value != null) {
            serializer.writeDocument($SCHEMA_VALUE, value);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_KEY, member, key);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_VALUE, member, value);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link ValidateFunctionRequest}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.key(this.key);
        builder.value(this.value);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link ValidateFunctionRequest}.
     */
    public static final class Builder implements ShapeBuilder<ValidateFunctionRequest> {
        private String key;
        private Document value;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * @return this builder.
         */
        public Builder key(String key) {
            this.key = key;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder value(Document value) {
            this.value = value;
            return this;
        }

        @Override
        public ValidateFunctionRequest build() {
            return new ValidateFunctionRequest(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> key((String) SchemaUtils.validateSameMember($SCHEMA_KEY, member, value));
                case 1 -> value((Document) SchemaUtils.validateSameMember($SCHEMA_VALUE, member, value));
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
                    case 0 -> builder.key(de.readString(member));
                    case 1 -> builder.value(de.readDocument());
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

