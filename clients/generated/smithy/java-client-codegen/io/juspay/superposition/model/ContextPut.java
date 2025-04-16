
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
public final class ContextPut implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ContextPut");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("condition", SharedSchemas.CONDITION,
                new RequiredTrait())
        .putMember("override", SharedSchemas.OVERRIDES,
                new RequiredTrait())
        .putMember("description", PreludeSchemas.STRING)
        .putMember("change_reason", PreludeSchemas.STRING,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_CONDITION = $SCHEMA.member("condition");
    private static final Schema $SCHEMA_OVERRIDE = $SCHEMA.member("override");
    private static final Schema $SCHEMA_DESCRIPTION = $SCHEMA.member("description");
    private static final Schema $SCHEMA_CHANGE_REASON = $SCHEMA.member("change_reason");

    private final transient Map<String, Document> condition;
    private final transient Map<String, Document> override;
    private final transient String description;
    private final transient String changeReason;

    private ContextPut(Builder builder) {
        this.condition = Collections.unmodifiableMap(builder.condition);
        this.override = Collections.unmodifiableMap(builder.override);
        this.description = builder.description;
        this.changeReason = builder.changeReason;
    }

    public Map<String, Document> condition() {
        return condition;
    }

    public boolean hasCondition() {
        return true;
    }

    public Map<String, Document> override() {
        return override;
    }

    public boolean hasOverride() {
        return true;
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
        ContextPut that = (ContextPut) other;
        return Objects.equals(this.condition, that.condition)
               && Objects.equals(this.override, that.override)
               && Objects.equals(this.description, that.description)
               && Objects.equals(this.changeReason, that.changeReason);
    }

    @Override
    public int hashCode() {
        return Objects.hash(condition, override, description, changeReason);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeMap($SCHEMA_CONDITION, condition, condition.size(), SharedSerde.ConditionSerializer.INSTANCE);
        serializer.writeMap($SCHEMA_OVERRIDE, override, override.size(), SharedSerde.OverridesSerializer.INSTANCE);
        if (description != null) {
            serializer.writeString($SCHEMA_DESCRIPTION, description);
        }
        serializer.writeString($SCHEMA_CHANGE_REASON, changeReason);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONDITION, member, condition);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_OVERRIDE, member, override);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, changeReason);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, description);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link ContextPut}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.condition(this.condition);
        builder.override(this.override);
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
     * Builder for {@link ContextPut}.
     */
    public static final class Builder implements ShapeBuilder<ContextPut> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private Map<String, Document> condition;
        private Map<String, Document> override;
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
        public Builder condition(Map<String, Document> condition) {
            this.condition = Objects.requireNonNull(condition, "condition cannot be null");
            tracker.setMember($SCHEMA_CONDITION);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder override(Map<String, Document> override) {
            this.override = Objects.requireNonNull(override, "override cannot be null");
            tracker.setMember($SCHEMA_OVERRIDE);
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder description(String description) {
            this.description = description;
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
        public ContextPut build() {
            tracker.validate();
            return new ContextPut(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> condition((Map<String, Document>) SchemaUtils.validateSameMember($SCHEMA_CONDITION, member, value));
                case 1 -> override((Map<String, Document>) SchemaUtils.validateSameMember($SCHEMA_OVERRIDE, member, value));
                case 2 -> changeReason((String) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, value));
                case 3 -> description((String) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<ContextPut> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_CONDITION)) {
                condition(Collections.emptyMap());
            }
            if (!tracker.checkMember($SCHEMA_OVERRIDE)) {
                override(Collections.emptyMap());
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
                    case 0 -> builder.condition(SharedSerde.deserializeCondition(member, de));
                    case 1 -> builder.override(SharedSerde.deserializeOverrides(member, de));
                    case 2 -> builder.changeReason(de.readString(member));
                    case 3 -> builder.description(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

