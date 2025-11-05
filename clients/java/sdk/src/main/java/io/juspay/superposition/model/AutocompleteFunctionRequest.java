
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
public final class AutocompleteFunctionRequest implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#AutocompleteFunctionRequest");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("name", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("prefix", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("environment", PreludeSchemas.DOCUMENT,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_NAME = $SCHEMA.member("name");
    private static final Schema $SCHEMA_PREFIX = $SCHEMA.member("prefix");
    private static final Schema $SCHEMA_ENVIRONMENT = $SCHEMA.member("environment");

    private final transient String name;
    private final transient String prefix;
    private final transient Document environment;

    private AutocompleteFunctionRequest(Builder builder) {
        this.name = builder.name;
        this.prefix = builder.prefix;
        this.environment = builder.environment;
    }

    public String name() {
        return name;
    }

    public String prefix() {
        return prefix;
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
        AutocompleteFunctionRequest that = (AutocompleteFunctionRequest) other;
        return Objects.equals(this.name, that.name)
               && Objects.equals(this.prefix, that.prefix)
               && Objects.equals(this.environment, that.environment);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, prefix, environment);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_NAME, name);
        serializer.writeString($SCHEMA_PREFIX, prefix);
        serializer.writeDocument($SCHEMA_ENVIRONMENT, environment);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_NAME, member, name);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_PREFIX, member, prefix);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_ENVIRONMENT, member, environment);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link AutocompleteFunctionRequest}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.name(this.name);
        builder.prefix(this.prefix);
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
     * Builder for {@link AutocompleteFunctionRequest}.
     */
    public static final class Builder implements ShapeBuilder<AutocompleteFunctionRequest> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String name;
        private String prefix;
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
        public Builder name(String name) {
            this.name = Objects.requireNonNull(name, "name cannot be null");
            tracker.setMember($SCHEMA_NAME);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder prefix(String prefix) {
            this.prefix = Objects.requireNonNull(prefix, "prefix cannot be null");
            tracker.setMember($SCHEMA_PREFIX);
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
        public AutocompleteFunctionRequest build() {
            tracker.validate();
            return new AutocompleteFunctionRequest(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> name((String) SchemaUtils.validateSameMember($SCHEMA_NAME, member, value));
                case 1 -> prefix((String) SchemaUtils.validateSameMember($SCHEMA_PREFIX, member, value));
                case 2 -> environment((Document) SchemaUtils.validateSameMember($SCHEMA_ENVIRONMENT, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<AutocompleteFunctionRequest> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_NAME)) {
                name("");
            }
            if (!tracker.checkMember($SCHEMA_PREFIX)) {
                prefix("");
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
                    case 0 -> builder.name(de.readString(member));
                    case 1 -> builder.prefix(de.readString(member));
                    case 2 -> builder.environment(de.readDocument());
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

