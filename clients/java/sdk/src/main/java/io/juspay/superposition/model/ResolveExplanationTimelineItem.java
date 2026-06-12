
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
public final class ResolveExplanationTimelineItem implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ResolveExplanationTimelineItem");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("context_id", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("condition", SharedSchemas.CONDITION,
                new RequiredTrait())
        .putMember("override_id", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("value_before", PreludeSchemas.DOCUMENT,
                new RequiredTrait())
        .putMember("value_after", PreludeSchemas.DOCUMENT,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_CONTEXT_ID = $SCHEMA.member("context_id");
    private static final Schema $SCHEMA_CONDITION = $SCHEMA.member("condition");
    private static final Schema $SCHEMA_OVERRIDE_ID = $SCHEMA.member("override_id");
    private static final Schema $SCHEMA_VALUE_BEFORE = $SCHEMA.member("value_before");
    private static final Schema $SCHEMA_VALUE_AFTER = $SCHEMA.member("value_after");

    private final transient String contextId;
    private final transient Map<String, Document> condition;
    private final transient String overrideId;
    private final transient Document valueBefore;
    private final transient Document valueAfter;

    private ResolveExplanationTimelineItem(Builder builder) {
        this.contextId = builder.contextId;
        this.condition = Collections.unmodifiableMap(builder.condition);
        this.overrideId = builder.overrideId;
        this.valueBefore = builder.valueBefore;
        this.valueAfter = builder.valueAfter;
    }

    public String contextId() {
        return contextId;
    }

    public Map<String, Document> condition() {
        return condition;
    }

    public boolean hasCondition() {
        return true;
    }

    public String overrideId() {
        return overrideId;
    }

    public Document valueBefore() {
        return valueBefore;
    }

    public Document valueAfter() {
        return valueAfter;
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
        ResolveExplanationTimelineItem that = (ResolveExplanationTimelineItem) other;
        return Objects.equals(this.contextId, that.contextId)
               && Objects.equals(this.condition, that.condition)
               && Objects.equals(this.overrideId, that.overrideId)
               && Objects.equals(this.valueBefore, that.valueBefore)
               && Objects.equals(this.valueAfter, that.valueAfter);
    }

    @Override
    public int hashCode() {
        return Objects.hash(contextId, condition, overrideId, valueBefore, valueAfter);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_CONTEXT_ID, contextId);
        serializer.writeMap($SCHEMA_CONDITION, condition, condition.size(), SharedSerde.ConditionSerializer.INSTANCE);
        serializer.writeString($SCHEMA_OVERRIDE_ID, overrideId);
        serializer.writeDocument($SCHEMA_VALUE_BEFORE, valueBefore);
        serializer.writeDocument($SCHEMA_VALUE_AFTER, valueAfter);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONTEXT_ID, member, contextId);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONDITION, member, condition);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_OVERRIDE_ID, member, overrideId);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_VALUE_BEFORE, member, valueBefore);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_VALUE_AFTER, member, valueAfter);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link ResolveExplanationTimelineItem}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.contextId(this.contextId);
        builder.condition(this.condition);
        builder.overrideId(this.overrideId);
        builder.valueBefore(this.valueBefore);
        builder.valueAfter(this.valueAfter);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link ResolveExplanationTimelineItem}.
     */
    public static final class Builder implements ShapeBuilder<ResolveExplanationTimelineItem> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String contextId;
        private Map<String, Document> condition;
        private String overrideId;
        private Document valueBefore;
        private Document valueAfter;

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
        public Builder condition(Map<String, Document> condition) {
            this.condition = Objects.requireNonNull(condition, "condition cannot be null");
            tracker.setMember($SCHEMA_CONDITION);
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
        public Builder valueBefore(Document valueBefore) {
            this.valueBefore = Objects.requireNonNull(valueBefore, "valueBefore cannot be null");
            tracker.setMember($SCHEMA_VALUE_BEFORE);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder valueAfter(Document valueAfter) {
            this.valueAfter = Objects.requireNonNull(valueAfter, "valueAfter cannot be null");
            tracker.setMember($SCHEMA_VALUE_AFTER);
            return this;
        }

        @Override
        public ResolveExplanationTimelineItem build() {
            tracker.validate();
            return new ResolveExplanationTimelineItem(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> contextId((String) SchemaUtils.validateSameMember($SCHEMA_CONTEXT_ID, member, value));
                case 1 -> condition((Map<String, Document>) SchemaUtils.validateSameMember($SCHEMA_CONDITION, member, value));
                case 2 -> overrideId((String) SchemaUtils.validateSameMember($SCHEMA_OVERRIDE_ID, member, value));
                case 3 -> valueBefore((Document) SchemaUtils.validateSameMember($SCHEMA_VALUE_BEFORE, member, value));
                case 4 -> valueAfter((Document) SchemaUtils.validateSameMember($SCHEMA_VALUE_AFTER, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<ResolveExplanationTimelineItem> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_CONTEXT_ID)) {
                contextId("");
            }
            if (!tracker.checkMember($SCHEMA_CONDITION)) {
                condition(Collections.emptyMap());
            }
            if (!tracker.checkMember($SCHEMA_OVERRIDE_ID)) {
                overrideId("");
            }
            if (!tracker.checkMember($SCHEMA_VALUE_BEFORE)) {
                tracker.setMember($SCHEMA_VALUE_BEFORE);
            }
            if (!tracker.checkMember($SCHEMA_VALUE_AFTER)) {
                tracker.setMember($SCHEMA_VALUE_AFTER);
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
                    case 1 -> builder.condition(SharedSerde.deserializeCondition(member, de));
                    case 2 -> builder.overrideId(de.readString(member));
                    case 3 -> builder.valueBefore(de.readDocument());
                    case 4 -> builder.valueAfter(de.readDocument());
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

