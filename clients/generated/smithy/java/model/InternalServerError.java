
package io.juspay.superposition.model;

import software.amazon.smithy.java.core.error.ModeledException;
import software.amazon.smithy.java.core.schema.PreludeSchemas;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.java.core.schema.SchemaUtils;
import software.amazon.smithy.java.core.schema.ShapeBuilder;
import software.amazon.smithy.java.core.serde.ShapeDeserializer;
import software.amazon.smithy.java.core.serde.ShapeSerializer;
import software.amazon.smithy.java.core.serde.ToStringSerializer;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.ErrorTrait;
import software.amazon.smithy.model.traits.HttpErrorTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class InternalServerError extends ModeledException {
    public static final ShapeId $ID = ShapeId.from("io.superposition#InternalServerError");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID,
            new ErrorTrait("server"),
            new HttpErrorTrait(500))
        .putMember("message", PreludeSchemas.STRING)
        .build();

    private static final Schema $SCHEMA_MESSAGE = $SCHEMA.member("message");

    private InternalServerError(Builder builder) {
        super($SCHEMA, builder.message, builder.$cause, builder.$captureStackTrace, builder.$deserialized);
    }

    @Override
    public String toString() {
        return ToStringSerializer.serialize(this);
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        if (getMessage() != null) {
            serializer.writeString($SCHEMA_MESSAGE, getMessage());
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_MESSAGE, member, getMessage());
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link InternalServerError}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.message(getMessage());
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link InternalServerError}.
     */
    public static final class Builder implements ShapeBuilder<InternalServerError> {
        private String message;
        private Throwable $cause;
        private Boolean $captureStackTrace;
        private boolean $deserialized;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * @return this builder.
         */
        public Builder message(String message) {
            this.message = message;
            return this;
        }

        public Builder withStackTrace() {
            this.$captureStackTrace = true;
            return this;
        }

        public Builder withoutStackTrace() {
            this.$captureStackTrace = false;
            return this;
        }

        public Builder withCause(Throwable cause) {
            this.$cause = cause;
            return this;
        }

        @Override
        public InternalServerError build() {
            return new InternalServerError(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> message((String) SchemaUtils.validateSameMember($SCHEMA_MESSAGE, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public Builder deserialize(ShapeDeserializer decoder) {
            this.$deserialized = true;
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
                    case 0 -> builder.message(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

