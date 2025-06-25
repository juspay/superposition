
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
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class CreateContextOutput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ContextActionResponse");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("context_id", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("override_id", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("weight", SharedSchemas.WEIGHT,
                new RequiredTrait())
        .putMember("description", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("change_reason", PreludeSchemas.STRING,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_CONTEXT_ID = $SCHEMA.member("context_id");
    private static final Schema $SCHEMA_OVERRIDE_ID = $SCHEMA.member("override_id");
    private static final Schema $SCHEMA_WEIGHT = $SCHEMA.member("weight");
    private static final Schema $SCHEMA_DESCRIPTION = $SCHEMA.member("description");
    private static final Schema $SCHEMA_CHANGE_REASON = $SCHEMA.member("change_reason");

    private final transient String contextId;
    private final transient String overrideId;
    private final transient String weight;
    private final transient String description;
    private final transient String changeReason;

    private CreateContextOutput(Builder builder) {
        this.contextId = builder.contextId;
        this.overrideId = builder.overrideId;
        this.weight = builder.weight;
        this.description = builder.description;
        this.changeReason = builder.changeReason;
    }

    public String contextId() {
        return contextId;
    }

    public String overrideId() {
        return overrideId;
    }

    public String weight() {
        return weight;
    }

    public String description() {
        return description;
    }

    public String changeReason() {
        return changeReason;
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
        CreateContextOutput that = (CreateContextOutput) other;
        return Objects.equals(this.contextId, that.contextId)
               && Objects.equals(this.overrideId, that.overrideId)
               && Objects.equals(this.weight, that.weight)
               && Objects.equals(this.description, that.description)
               && Objects.equals(this.changeReason, that.changeReason);
    }

    @Override
    public int hashCode() {
        return Objects.hash(contextId, overrideId, weight, description, changeReason);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_CONTEXT_ID, contextId);
        serializer.writeString($SCHEMA_OVERRIDE_ID, overrideId);
        serializer.writeString($SCHEMA_WEIGHT, weight);
        serializer.writeString($SCHEMA_DESCRIPTION, description);
        serializer.writeString($SCHEMA_CHANGE_REASON, changeReason);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONTEXT_ID, member, contextId);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_OVERRIDE_ID, member, overrideId);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_WEIGHT, member, weight);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, description);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, changeReason);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link CreateContextOutput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.contextId(this.contextId);
        builder.overrideId(this.overrideId);
        builder.weight(this.weight);
        builder.description(this.description);
        builder.changeReason(this.changeReason);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link CreateContextOutput}.
     */
    public static final class Builder implements ShapeBuilder<CreateContextOutput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String contextId;
        private String overrideId;
        private String weight;
        private String description;
        private String changeReason;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder contextId(String contextId) {
            this.contextId = Objects.requireNonNull(contextId, "contextId cannot be null");
            tracker.setMember($SCHEMA_CONTEXT_ID);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder overrideId(String overrideId) {
            this.overrideId = Objects.requireNonNull(overrideId, "overrideId cannot be null");
            tracker.setMember($SCHEMA_OVERRIDE_ID);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder weight(String weight) {
            this.weight = Objects.requireNonNull(weight, "weight cannot be null");
            tracker.setMember($SCHEMA_WEIGHT);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder description(String description) {
            this.description = Objects.requireNonNull(description, "description cannot be null");
            tracker.setMember($SCHEMA_DESCRIPTION);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder changeReason(String changeReason) {
            this.changeReason = Objects.requireNonNull(changeReason, "changeReason cannot be null");
            tracker.setMember($SCHEMA_CHANGE_REASON);
            return this;
        }

        @Override
        public CreateContextOutput build() {
            tracker.validate();
            return new CreateContextOutput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> contextId((String) SchemaUtils.validateSameMember($SCHEMA_CONTEXT_ID, member, value));
                case 1 -> overrideId((String) SchemaUtils.validateSameMember($SCHEMA_OVERRIDE_ID, member, value));
                case 2 -> weight((String) SchemaUtils.validateSameMember($SCHEMA_WEIGHT, member, value));
                case 3 -> description((String) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, value));
                case 4 -> changeReason((String) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<CreateContextOutput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_CONTEXT_ID)) {
                contextId("");
            }
            if (!tracker.checkMember($SCHEMA_OVERRIDE_ID)) {
                overrideId("");
            }
            if (!tracker.checkMember($SCHEMA_WEIGHT)) {
                weight("");
            }
            if (!tracker.checkMember($SCHEMA_DESCRIPTION)) {
                description("");
            }
            if (!tracker.checkMember($SCHEMA_CHANGE_REASON)) {
                changeReason("");
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
                    case 0 -> builder.contextId(de.readString(member));
                    case 1 -> builder.overrideId(de.readString(member));
                    case 2 -> builder.weight(de.readString(member));
                    case 3 -> builder.description(de.readString(member));
                    case 4 -> builder.changeReason(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

