
package io.juspay.superposition.model;

import software.amazon.smithy.java.core.error.ModeledException;
import software.amazon.smithy.java.core.schema.PreludeSchemas;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.java.core.schema.SchemaUtils;
import software.amazon.smithy.java.core.schema.ShapeBuilder;
import software.amazon.smithy.java.core.serde.ShapeDeserializer;
import software.amazon.smithy.java.core.serde.ShapeSerializer;
import software.amazon.smithy.java.core.serde.ToStringSerializer;
import software.amazon.smithy.java.core.serde.document.Document;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.ErrorTrait;
import software.amazon.smithy.model.traits.HttpErrorTrait;
import software.amazon.smithy.utils.SmithyGenerated;

/**
 * Indicates that the operation succeeded but the webhook call failed. The response body contains the
 * successful result, but the client should be aware that webhook notification did not complete.
 */
@SmithyGenerated
public final class WebhookFailed extends ModeledException {
    public static final ShapeId $ID = ShapeId.from("io.superposition#WebhookFailed");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID,
            new ErrorTrait("server"),
            new HttpErrorTrait(512))
        .putMember("data", PreludeSchemas.DOCUMENT)
        .build();

    private static final Schema $SCHEMA_DATA = $SCHEMA.member("data");

    private final transient Document data;

    private WebhookFailed(Builder builder) {
        super($SCHEMA, null, builder.$cause, builder.$captureStackTrace, builder.$deserialized);
        this.data = builder.data;
    }

    /**
     * The successful operation result that would have been returned with HTTP 200. The structure matches
     * the operation's normal output type.
     */
    public Document data() {
        return data;
    }

    @Override
    public String toString() {
        return ToStringSerializer.serialize(this);
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        if (data != null) {
            serializer.writeDocument($SCHEMA_DATA, data);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_DATA, member, data);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link WebhookFailed}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.data(this.data);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link WebhookFailed}.
     */
    public static final class Builder implements ShapeBuilder<WebhookFailed> {
        private Document data;
        private Throwable $cause;
        private Boolean $captureStackTrace;
        private boolean $deserialized;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * The successful operation result that would have been returned with HTTP 200. The structure matches
         * the operation's normal output type.
         *
         * @return this builder.
         */
        public Builder data(Document data) {
            this.data = data;
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
        public WebhookFailed build() {
            return new WebhookFailed(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> data((Document) SchemaUtils.validateSameMember($SCHEMA_DATA, member, value));
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
                    case 0 -> builder.data(de.readDocument());
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

