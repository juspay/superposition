
package io.juspay.superposition.model;

import java.util.Collections;
import java.util.List;
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
public final class ContextPartial implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ContextPartial");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("id", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("condition", SharedSchemas.CONDITION,
                new RequiredTrait())
        .putMember("priority", PreludeSchemas.INTEGER,
                new RequiredTrait())
        .putMember("weight", PreludeSchemas.INTEGER,
                new RequiredTrait())
        .putMember("override_with_keys", SharedSchemas.OVERRIDE_WITH_KEYS,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_ID = $SCHEMA.member("id");
    private static final Schema $SCHEMA_CONDITION = $SCHEMA.member("condition");
    private static final Schema $SCHEMA_PRIORITY = $SCHEMA.member("priority");
    private static final Schema $SCHEMA_WEIGHT = $SCHEMA.member("weight");
    private static final Schema $SCHEMA_OVERRIDE_WITH_KEYS = $SCHEMA.member("override_with_keys");

    private final transient String id;
    private final transient Map<String, Document> condition;
    private final transient int priority;
    private final transient int weight;
    private final transient List<String> overrideWithKeys;

    private ContextPartial(Builder builder) {
        this.id = builder.id;
        this.condition = Collections.unmodifiableMap(builder.condition);
        this.priority = builder.priority;
        this.weight = builder.weight;
        this.overrideWithKeys = Collections.unmodifiableList(builder.overrideWithKeys);
    }

    public String id() {
        return id;
    }

    public Map<String, Document> condition() {
        return condition;
    }

    public boolean hasCondition() {
        return true;
    }

    public int priority() {
        return priority;
    }

    public int weight() {
        return weight;
    }

    public List<String> overrideWithKeys() {
        return overrideWithKeys;
    }

    public boolean hasOverrideWithKeys() {
        return true;
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
        ContextPartial that = (ContextPartial) other;
        return Objects.equals(this.id, that.id)
               && Objects.equals(this.condition, that.condition)
               && this.priority == that.priority
               && this.weight == that.weight
               && Objects.equals(this.overrideWithKeys, that.overrideWithKeys);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, condition, priority, weight, overrideWithKeys);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_ID, id);
        serializer.writeMap($SCHEMA_CONDITION, condition, condition.size(), SharedSerde.ConditionSerializer.INSTANCE);
        serializer.writeInteger($SCHEMA_PRIORITY, priority);
        serializer.writeInteger($SCHEMA_WEIGHT, weight);
        serializer.writeList($SCHEMA_OVERRIDE_WITH_KEYS, overrideWithKeys, overrideWithKeys.size(), SharedSerde.OverrideWithKeysSerializer.INSTANCE);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_ID, member, id);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONDITION, member, condition);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_PRIORITY, member, priority);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_WEIGHT, member, weight);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_OVERRIDE_WITH_KEYS, member, overrideWithKeys);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link ContextPartial}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.id(this.id);
        builder.condition(this.condition);
        builder.priority(this.priority);
        builder.weight(this.weight);
        builder.overrideWithKeys(this.overrideWithKeys);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link ContextPartial}.
     */
    public static final class Builder implements ShapeBuilder<ContextPartial> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String id;
        private Map<String, Document> condition;
        private int priority;
        private int weight;
        private List<String> overrideWithKeys;

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
        public Builder condition(Map<String, Document> condition) {
            this.condition = Objects.requireNonNull(condition, "condition cannot be null");
            tracker.setMember($SCHEMA_CONDITION);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder priority(int priority) {
            this.priority = priority;
            tracker.setMember($SCHEMA_PRIORITY);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder weight(int weight) {
            this.weight = weight;
            tracker.setMember($SCHEMA_WEIGHT);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder overrideWithKeys(List<String> overrideWithKeys) {
            this.overrideWithKeys = Objects.requireNonNull(overrideWithKeys, "overrideWithKeys cannot be null");
            tracker.setMember($SCHEMA_OVERRIDE_WITH_KEYS);
            return this;
        }

        @Override
        public ContextPartial build() {
            tracker.validate();
            return new ContextPartial(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> id((String) SchemaUtils.validateSameMember($SCHEMA_ID, member, value));
                case 1 -> condition((Map<String, Document>) SchemaUtils.validateSameMember($SCHEMA_CONDITION, member, value));
                case 2 -> priority((int) SchemaUtils.validateSameMember($SCHEMA_PRIORITY, member, value));
                case 3 -> weight((int) SchemaUtils.validateSameMember($SCHEMA_WEIGHT, member, value));
                case 4 -> overrideWithKeys((List<String>) SchemaUtils.validateSameMember($SCHEMA_OVERRIDE_WITH_KEYS, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<ContextPartial> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_ID)) {
                id("");
            }
            if (!tracker.checkMember($SCHEMA_CONDITION)) {
                condition(Collections.emptyMap());
            }
            if (!tracker.checkMember($SCHEMA_PRIORITY)) {
                tracker.setMember($SCHEMA_PRIORITY);
            }
            if (!tracker.checkMember($SCHEMA_WEIGHT)) {
                tracker.setMember($SCHEMA_WEIGHT);
            }
            if (!tracker.checkMember($SCHEMA_OVERRIDE_WITH_KEYS)) {
                overrideWithKeys(Collections.emptyList());
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
                    case 1 -> builder.condition(SharedSerde.deserializeCondition(member, de));
                    case 2 -> builder.priority(de.readInteger(member));
                    case 3 -> builder.weight(de.readInteger(member));
                    case 4 -> builder.overrideWithKeys(SharedSerde.deserializeOverrideWithKeys(member, de));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

