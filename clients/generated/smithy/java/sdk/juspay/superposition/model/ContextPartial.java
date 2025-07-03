
package io.juspay.superposition.model;

import java.util.Collections;
import java.util.List;
import java.util.Map;
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
public final class ContextPartial implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ContextPartial");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("id", PreludeSchemas.STRING)
        .putMember("condition", SharedSchemas.CONDITION)
        .putMember("priority", PreludeSchemas.INTEGER)
        .putMember("weight", PreludeSchemas.INTEGER)
        .putMember("override_with_keys", SharedSchemas.OVERRIDE_WITH_KEYS)
        .build();

    private static final Schema $SCHEMA_ID = $SCHEMA.member("id");
    private static final Schema $SCHEMA_CONDITION = $SCHEMA.member("condition");
    private static final Schema $SCHEMA_PRIORITY = $SCHEMA.member("priority");
    private static final Schema $SCHEMA_WEIGHT = $SCHEMA.member("weight");
    private static final Schema $SCHEMA_OVERRIDE_WITH_KEYS = $SCHEMA.member("override_with_keys");

    private final transient String id;
    private final transient Map<String, Document> condition;
    private final transient Integer priority;
    private final transient Integer weight;
    private final transient List<String> overrideWithKeys;

    private ContextPartial(Builder builder) {
        this.id = builder.id;
        this.condition = builder.condition == null ? null : Collections.unmodifiableMap(builder.condition);
        this.priority = builder.priority;
        this.weight = builder.weight;
        this.overrideWithKeys = builder.overrideWithKeys == null ? null : Collections.unmodifiableList(builder.overrideWithKeys);
    }

    public String id() {
        return id;
    }

    public Map<String, Document> condition() {
        if (condition == null) {
            return Collections.emptyMap();
        }
        return condition;
    }

    public boolean hasCondition() {
        return condition != null;
    }

    public Integer priority() {
        return priority;
    }

    public Integer weight() {
        return weight;
    }

    public List<String> overrideWithKeys() {
        if (overrideWithKeys == null) {
            return Collections.emptyList();
        }
        return overrideWithKeys;
    }

    public boolean hasOverrideWithKeys() {
        return overrideWithKeys != null;
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
               && Objects.equals(this.priority, that.priority)
               && Objects.equals(this.weight, that.weight)
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
        if (id != null) {
            serializer.writeString($SCHEMA_ID, id);
        }
        if (condition != null) {
            serializer.writeMap($SCHEMA_CONDITION, condition, condition.size(), SharedSerde.ConditionSerializer.INSTANCE);
        }
        if (priority != null) {
            serializer.writeInteger($SCHEMA_PRIORITY, priority);
        }
        if (weight != null) {
            serializer.writeInteger($SCHEMA_WEIGHT, weight);
        }
        if (overrideWithKeys != null) {
            serializer.writeList($SCHEMA_OVERRIDE_WITH_KEYS, overrideWithKeys, overrideWithKeys.size(), SharedSerde.OverrideWithKeysSerializer.INSTANCE);
        }
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
        private String id;
        private Map<String, Document> condition;
        private Integer priority;
        private Integer weight;
        private List<String> overrideWithKeys;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * @return this builder.
         */
        public Builder id(String id) {
            this.id = id;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder condition(Map<String, Document> condition) {
            this.condition = condition;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder priority(int priority) {
            this.priority = priority;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder weight(int weight) {
            this.weight = weight;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder overrideWithKeys(List<String> overrideWithKeys) {
            this.overrideWithKeys = overrideWithKeys;
            return this;
        }

        @Override
        public ContextPartial build() {
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

