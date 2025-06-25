
package io.juspay.superposition.model;

import java.time.Instant;
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
public final class GetContextOutput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ContextFull");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("id", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("value", SharedSchemas.CONDITION)
        .putMember("override", SharedSchemas.OVERRIDES)
        .putMember("override_id", PreludeSchemas.STRING)
        .putMember("weight", SharedSchemas.WEIGHT)
        .putMember("description", PreludeSchemas.STRING)
        .putMember("change_reason", PreludeSchemas.STRING)
        .putMember("created_at", SharedSchemas.DATE_TIME)
        .putMember("created_by", PreludeSchemas.STRING)
        .putMember("last_modified_at", SharedSchemas.DATE_TIME)
        .putMember("last_modified_by", PreludeSchemas.STRING)
        .build();

    private static final Schema $SCHEMA_ID = $SCHEMA.member("id");
    private static final Schema $SCHEMA_VALUE = $SCHEMA.member("value");
    private static final Schema $SCHEMA_OVERRIDE = $SCHEMA.member("override");
    private static final Schema $SCHEMA_OVERRIDE_ID = $SCHEMA.member("override_id");
    private static final Schema $SCHEMA_WEIGHT = $SCHEMA.member("weight");
    private static final Schema $SCHEMA_DESCRIPTION = $SCHEMA.member("description");
    private static final Schema $SCHEMA_CHANGE_REASON = $SCHEMA.member("change_reason");
    private static final Schema $SCHEMA_CREATED_AT = $SCHEMA.member("created_at");
    private static final Schema $SCHEMA_CREATED_BY = $SCHEMA.member("created_by");
    private static final Schema $SCHEMA_LAST_MODIFIED_AT = $SCHEMA.member("last_modified_at");
    private static final Schema $SCHEMA_LAST_MODIFIED_BY = $SCHEMA.member("last_modified_by");

    private final transient String id;
    private final transient Map<String, Document> value;
    private final transient Map<String, Document> override;
    private final transient String overrideId;
    private final transient String weight;
    private final transient String description;
    private final transient String changeReason;
    private final transient Instant createdAt;
    private final transient String createdBy;
    private final transient Instant lastModifiedAt;
    private final transient String lastModifiedBy;

    private GetContextOutput(Builder builder) {
        this.id = builder.id;
        this.value = builder.value == null ? null : Collections.unmodifiableMap(builder.value);
        this.override = builder.override == null ? null : Collections.unmodifiableMap(builder.override);
        this.overrideId = builder.overrideId;
        this.weight = builder.weight;
        this.description = builder.description;
        this.changeReason = builder.changeReason;
        this.createdAt = builder.createdAt;
        this.createdBy = builder.createdBy;
        this.lastModifiedAt = builder.lastModifiedAt;
        this.lastModifiedBy = builder.lastModifiedBy;
    }

    public String id() {
        return id;
    }

    public Map<String, Document> value() {
        if (value == null) {
            return Collections.emptyMap();
        }
        return value;
    }

    public boolean hasValue() {
        return value != null;
    }

    public Map<String, Document> override() {
        if (override == null) {
            return Collections.emptyMap();
        }
        return override;
    }

    public boolean hasOverride() {
        return override != null;
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

    public Instant createdAt() {
        return createdAt;
    }

    public String createdBy() {
        return createdBy;
    }

    public Instant lastModifiedAt() {
        return lastModifiedAt;
    }

    public String lastModifiedBy() {
        return lastModifiedBy;
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
        GetContextOutput that = (GetContextOutput) other;
        return Objects.equals(this.id, that.id)
               && Objects.equals(this.value, that.value)
               && Objects.equals(this.override, that.override)
               && Objects.equals(this.overrideId, that.overrideId)
               && Objects.equals(this.weight, that.weight)
               && Objects.equals(this.description, that.description)
               && Objects.equals(this.changeReason, that.changeReason)
               && Objects.equals(this.createdAt, that.createdAt)
               && Objects.equals(this.createdBy, that.createdBy)
               && Objects.equals(this.lastModifiedAt, that.lastModifiedAt)
               && Objects.equals(this.lastModifiedBy, that.lastModifiedBy);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, value, override, overrideId, weight, description, changeReason, createdAt, createdBy, lastModifiedAt, lastModifiedBy);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_ID, id);
        if (value != null) {
            serializer.writeMap($SCHEMA_VALUE, value, value.size(), SharedSerde.ConditionSerializer.INSTANCE);
        }
        if (override != null) {
            serializer.writeMap($SCHEMA_OVERRIDE, override, override.size(), SharedSerde.OverridesSerializer.INSTANCE);
        }
        if (overrideId != null) {
            serializer.writeString($SCHEMA_OVERRIDE_ID, overrideId);
        }
        if (weight != null) {
            serializer.writeString($SCHEMA_WEIGHT, weight);
        }
        if (description != null) {
            serializer.writeString($SCHEMA_DESCRIPTION, description);
        }
        if (changeReason != null) {
            serializer.writeString($SCHEMA_CHANGE_REASON, changeReason);
        }
        if (createdAt != null) {
            serializer.writeTimestamp($SCHEMA_CREATED_AT, createdAt);
        }
        if (createdBy != null) {
            serializer.writeString($SCHEMA_CREATED_BY, createdBy);
        }
        if (lastModifiedAt != null) {
            serializer.writeTimestamp($SCHEMA_LAST_MODIFIED_AT, lastModifiedAt);
        }
        if (lastModifiedBy != null) {
            serializer.writeString($SCHEMA_LAST_MODIFIED_BY, lastModifiedBy);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_ID, member, id);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_VALUE, member, value);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_OVERRIDE, member, override);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_OVERRIDE_ID, member, overrideId);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_WEIGHT, member, weight);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, description);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, changeReason);
            case 7 -> (T) SchemaUtils.validateSameMember($SCHEMA_CREATED_AT, member, createdAt);
            case 8 -> (T) SchemaUtils.validateSameMember($SCHEMA_CREATED_BY, member, createdBy);
            case 9 -> (T) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_AT, member, lastModifiedAt);
            case 10 -> (T) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_BY, member, lastModifiedBy);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link GetContextOutput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.id(this.id);
        builder.value(this.value);
        builder.override(this.override);
        builder.overrideId(this.overrideId);
        builder.weight(this.weight);
        builder.description(this.description);
        builder.changeReason(this.changeReason);
        builder.createdAt(this.createdAt);
        builder.createdBy(this.createdBy);
        builder.lastModifiedAt(this.lastModifiedAt);
        builder.lastModifiedBy(this.lastModifiedBy);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link GetContextOutput}.
     */
    public static final class Builder implements ShapeBuilder<GetContextOutput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String id;
        private Map<String, Document> value;
        private Map<String, Document> override;
        private String overrideId;
        private String weight;
        private String description;
        private String changeReason;
        private Instant createdAt;
        private String createdBy;
        private Instant lastModifiedAt;
        private String lastModifiedBy;

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
         * @return this builder.
         */
        public Builder value(Map<String, Document> value) {
            this.value = value;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder override(Map<String, Document> override) {
            this.override = override;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder overrideId(String overrideId) {
            this.overrideId = overrideId;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder weight(String weight) {
            this.weight = weight;
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
         * @return this builder.
         */
        public Builder changeReason(String changeReason) {
            this.changeReason = changeReason;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder createdAt(Instant createdAt) {
            this.createdAt = createdAt;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder createdBy(String createdBy) {
            this.createdBy = createdBy;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder lastModifiedAt(Instant lastModifiedAt) {
            this.lastModifiedAt = lastModifiedAt;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder lastModifiedBy(String lastModifiedBy) {
            this.lastModifiedBy = lastModifiedBy;
            return this;
        }

        @Override
        public GetContextOutput build() {
            tracker.validate();
            return new GetContextOutput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> id((String) SchemaUtils.validateSameMember($SCHEMA_ID, member, value));
                case 1 -> value((Map<String, Document>) SchemaUtils.validateSameMember($SCHEMA_VALUE, member, value));
                case 2 -> override((Map<String, Document>) SchemaUtils.validateSameMember($SCHEMA_OVERRIDE, member, value));
                case 3 -> overrideId((String) SchemaUtils.validateSameMember($SCHEMA_OVERRIDE_ID, member, value));
                case 4 -> weight((String) SchemaUtils.validateSameMember($SCHEMA_WEIGHT, member, value));
                case 5 -> description((String) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, value));
                case 6 -> changeReason((String) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, value));
                case 7 -> createdAt((Instant) SchemaUtils.validateSameMember($SCHEMA_CREATED_AT, member, value));
                case 8 -> createdBy((String) SchemaUtils.validateSameMember($SCHEMA_CREATED_BY, member, value));
                case 9 -> lastModifiedAt((Instant) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_AT, member, value));
                case 10 -> lastModifiedBy((String) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_BY, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<GetContextOutput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_ID)) {
                id("");
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
                    case 1 -> builder.value(SharedSerde.deserializeCondition(member, de));
                    case 2 -> builder.override(SharedSerde.deserializeOverrides(member, de));
                    case 3 -> builder.overrideId(de.readString(member));
                    case 4 -> builder.weight(de.readString(member));
                    case 5 -> builder.description(de.readString(member));
                    case 6 -> builder.changeReason(de.readString(member));
                    case 7 -> builder.createdAt(de.readTimestamp(member));
                    case 8 -> builder.createdBy(de.readString(member));
                    case 9 -> builder.lastModifiedAt(de.readTimestamp(member));
                    case 10 -> builder.lastModifiedBy(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

