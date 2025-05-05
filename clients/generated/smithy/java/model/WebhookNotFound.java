
package io.juspay.superposition.model;

import software.amazon.smithy.java.core.error.ModeledException;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.java.core.schema.ShapeBuilder;
import software.amazon.smithy.java.core.serde.ShapeDeserializer;
import software.amazon.smithy.java.core.serde.ShapeSerializer;
import software.amazon.smithy.java.core.serde.ToStringSerializer;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.ErrorTrait;
import software.amazon.smithy.model.traits.HttpErrorTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class WebhookNotFound extends ModeledException {
    public static final ShapeId $ID = ShapeId.from("io.superposition#WebhookNotFound");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID,
            new ErrorTrait("client"),
            new HttpErrorTrait(404)).build();

    private WebhookNotFound(Builder builder) {
        super($SCHEMA, null, builder.$cause, builder.$captureStackTrace, builder.$deserialized);
    }

    @Override
    public String toString() {
        return ToStringSerializer.serialize(this);
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {

    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link WebhookNotFound}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link WebhookNotFound}.
     */
    public static final class Builder implements ShapeBuilder<WebhookNotFound> {
        private Throwable $cause;
        private Boolean $captureStackTrace;
        private boolean $deserialized;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
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
        public WebhookNotFound build() {
            return new WebhookNotFound(this);
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
            public void accept(Builder builder, Schema member, ShapeDeserializer de) {}
        }
    }
}

