
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
public final class WeightRecomputeResponse implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#WeightRecomputeResponse");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("id", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("condition", SharedSchemas.CONDITION,
                new RequiredTrait())
        .putMember("old_weight", SharedSchemas.WEIGHT,
                new RequiredTrait())
        .putMember("new_weight", SharedSchemas.WEIGHT,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_ID = $SCHEMA.member("id");
    private static final Schema $SCHEMA_CONDITION = $SCHEMA.member("condition");
    private static final Schema $SCHEMA_OLD_WEIGHT = $SCHEMA.member("old_weight");
    private static final Schema $SCHEMA_NEW_WEIGHT = $SCHEMA.member("new_weight");

    private final transient String id;
    private final transient Map<String, Document> condition;
    private final transient String oldWeight;
    private final transient String newWeight;

    private WeightRecomputeResponse(Builder builder) {
        this.id = builder.id;
        this.condition = Collections.unmodifiableMap(builder.condition);
        this.oldWeight = builder.oldWeight;
        this.newWeight = builder.newWeight;
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

    public String oldWeight() {
        return oldWeight;
    }

    public String newWeight() {
        return newWeight;
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
        WeightRecomputeResponse that = (WeightRecomputeResponse) other;
        return Objects.equals(this.id, that.id)
               && Objects.equals(this.condition, that.condition)
               && Objects.equals(this.oldWeight, that.oldWeight)
               && Objects.equals(this.newWeight, that.newWeight);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, condition, oldWeight, newWeight);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_ID, id);
        serializer.writeMap($SCHEMA_CONDITION, condition, condition.size(), SharedSerde.ConditionSerializer.INSTANCE);
        serializer.writeString($SCHEMA_OLD_WEIGHT, oldWeight);
        serializer.writeString($SCHEMA_NEW_WEIGHT, newWeight);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_ID, member, id);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONDITION, member, condition);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_OLD_WEIGHT, member, oldWeight);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_NEW_WEIGHT, member, newWeight);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link WeightRecomputeResponse}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.id(this.id);
        builder.condition(this.condition);
        builder.oldWeight(this.oldWeight);
        builder.newWeight(this.newWeight);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link WeightRecomputeResponse}.
     */
    public static final class Builder implements ShapeBuilder<WeightRecomputeResponse> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String id;
        private Map<String, Document> condition;
        private String oldWeight;
        private String newWeight;

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
        public Builder oldWeight(String oldWeight) {
            this.oldWeight = Objects.requireNonNull(oldWeight, "oldWeight cannot be null");
            tracker.setMember($SCHEMA_OLD_WEIGHT);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder newWeight(String newWeight) {
            this.newWeight = Objects.requireNonNull(newWeight, "newWeight cannot be null");
            tracker.setMember($SCHEMA_NEW_WEIGHT);
            return this;
        }

        @Override
        public WeightRecomputeResponse build() {
            tracker.validate();
            return new WeightRecomputeResponse(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> id((String) SchemaUtils.validateSameMember($SCHEMA_ID, member, value));
                case 1 -> condition((Map<String, Document>) SchemaUtils.validateSameMember($SCHEMA_CONDITION, member, value));
                case 2 -> oldWeight((String) SchemaUtils.validateSameMember($SCHEMA_OLD_WEIGHT, member, value));
                case 3 -> newWeight((String) SchemaUtils.validateSameMember($SCHEMA_NEW_WEIGHT, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<WeightRecomputeResponse> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_ID)) {
                id("");
            }
            if (!tracker.checkMember($SCHEMA_CONDITION)) {
                condition(Collections.emptyMap());
            }
            if (!tracker.checkMember($SCHEMA_OLD_WEIGHT)) {
                oldWeight("");
            }
            if (!tracker.checkMember($SCHEMA_NEW_WEIGHT)) {
                newWeight("");
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
                    case 2 -> builder.oldWeight(de.readString(member));
                    case 3 -> builder.newWeight(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

