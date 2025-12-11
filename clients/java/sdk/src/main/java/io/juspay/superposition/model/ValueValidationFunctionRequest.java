
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
public final class ValueValidationFunctionRequest implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ValueValidationFunctionRequest");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("key", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("value", PreludeSchemas.DOCUMENT,
                new RequiredTrait())
        .putMember("type", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("environment", PreludeSchemas.DOCUMENT,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_KEY = $SCHEMA.member("key");
    private static final Schema $SCHEMA_VALUE = $SCHEMA.member("value");
    private static final Schema $SCHEMA_TYPE_MEMBER = $SCHEMA.member("type");
    private static final Schema $SCHEMA_ENVIRONMENT = $SCHEMA.member("environment");

    private final transient String key;
    private final transient Document value;
    private final transient String typeMember;
    private final transient Document environment;

    private ValueValidationFunctionRequest(Builder builder) {
        this.key = builder.key;
        this.value = builder.value;
        this.typeMember = builder.typeMember;
        this.environment = builder.environment;
    }

    public String key() {
        return key;
    }

    public Document value() {
        return value;
    }

    public String typeMember() {
        return typeMember;
    }

    public Document environment() {
        return environment;
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
        ValueValidationFunctionRequest that = (ValueValidationFunctionRequest) other;
        return Objects.equals(this.key, that.key)
               && Objects.equals(this.value, that.value)
               && Objects.equals(this.typeMember, that.typeMember)
               && Objects.equals(this.environment, that.environment);
    }

    @Override
    public int hashCode() {
        return Objects.hash(key, value, typeMember, environment);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_KEY, key);
        serializer.writeDocument($SCHEMA_VALUE, value);
        serializer.writeString($SCHEMA_TYPE_MEMBER, typeMember);
        serializer.writeDocument($SCHEMA_ENVIRONMENT, environment);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_KEY, member, key);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_VALUE, member, value);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_TYPE_MEMBER, member, typeMember);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_ENVIRONMENT, member, environment);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link ValueValidationFunctionRequest}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.key(this.key);
        builder.value(this.value);
        builder.typeMember(this.typeMember);
        builder.environment(this.environment);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link ValueValidationFunctionRequest}.
     */
    public static final class Builder implements ShapeBuilder<ValueValidationFunctionRequest> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String key;
        private Document value;
        private String typeMember;
        private Document environment;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder key(String key) {
            this.key = Objects.requireNonNull(key, "key cannot be null");
            tracker.setMember($SCHEMA_KEY);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder value(Document value) {
            this.value = Objects.requireNonNull(value, "value cannot be null");
            tracker.setMember($SCHEMA_VALUE);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder typeMember(String typeMember) {
            this.typeMember = Objects.requireNonNull(typeMember, "typeMember cannot be null");
            tracker.setMember($SCHEMA_TYPE_MEMBER);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder environment(Document environment) {
            this.environment = Objects.requireNonNull(environment, "environment cannot be null");
            tracker.setMember($SCHEMA_ENVIRONMENT);
            return this;
        }

        @Override
        public ValueValidationFunctionRequest build() {
            tracker.validate();
            return new ValueValidationFunctionRequest(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> key((String) SchemaUtils.validateSameMember($SCHEMA_KEY, member, value));
                case 1 -> value((Document) SchemaUtils.validateSameMember($SCHEMA_VALUE, member, value));
                case 2 -> typeMember((String) SchemaUtils.validateSameMember($SCHEMA_TYPE_MEMBER, member, value));
                case 3 -> environment((Document) SchemaUtils.validateSameMember($SCHEMA_ENVIRONMENT, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<ValueValidationFunctionRequest> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_KEY)) {
                key("");
            }
            if (!tracker.checkMember($SCHEMA_VALUE)) {
                tracker.setMember($SCHEMA_VALUE);
            }
            if (!tracker.checkMember($SCHEMA_TYPE_MEMBER)) {
                typeMember("");
            }
            if (!tracker.checkMember($SCHEMA_ENVIRONMENT)) {
                tracker.setMember($SCHEMA_ENVIRONMENT);
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
                    case 0 -> builder.key(de.readString(member));
                    case 1 -> builder.value(de.readDocument());
                    case 2 -> builder.typeMember(de.readString(member));
                    case 3 -> builder.environment(de.readDocument());
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

