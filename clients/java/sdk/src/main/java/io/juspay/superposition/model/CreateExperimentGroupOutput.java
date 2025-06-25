
package io.juspay.superposition.model;

import java.math.BigDecimal;
import java.time.Instant;
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
import software.amazon.smithy.model.traits.RangeTrait;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

/**
 * Standard response structure for an experiment group.
 */
@SmithyGenerated
public final class CreateExperimentGroupOutput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ExperimentGroupResponse");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("id", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("context_hash", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("name", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("description", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("change_reason", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("context", SharedSchemas.CONDITION,
                new RequiredTrait())
        .putMember("traffic_percentage", PreludeSchemas.INTEGER,
                RangeTrait.builder().min(new BigDecimal("0")).max(new BigDecimal("100")).build(),
                new RequiredTrait())
        .putMember("member_experiment_ids", SharedSchemas.STRING_LIST,
                new RequiredTrait())
        .putMember("created_at", SharedSchemas.DATE_TIME,
                new RequiredTrait())
        .putMember("created_by", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("last_modified_at", SharedSchemas.DATE_TIME,
                new RequiredTrait())
        .putMember("last_modified_by", PreludeSchemas.STRING,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_ID = $SCHEMA.member("id");
    private static final Schema $SCHEMA_CONTEXT_HASH = $SCHEMA.member("context_hash");
    private static final Schema $SCHEMA_NAME = $SCHEMA.member("name");
    private static final Schema $SCHEMA_DESCRIPTION = $SCHEMA.member("description");
    private static final Schema $SCHEMA_CHANGE_REASON = $SCHEMA.member("change_reason");
    private static final Schema $SCHEMA_CONTEXT = $SCHEMA.member("context");
    private static final Schema $SCHEMA_TRAFFIC_PERCENTAGE = $SCHEMA.member("traffic_percentage");
    private static final Schema $SCHEMA_MEMBER_EXPERIMENT_IDS = $SCHEMA.member("member_experiment_ids");
    private static final Schema $SCHEMA_CREATED_AT = $SCHEMA.member("created_at");
    private static final Schema $SCHEMA_CREATED_BY = $SCHEMA.member("created_by");
    private static final Schema $SCHEMA_LAST_MODIFIED_AT = $SCHEMA.member("last_modified_at");
    private static final Schema $SCHEMA_LAST_MODIFIED_BY = $SCHEMA.member("last_modified_by");

    private final transient String id;
    private final transient String contextHash;
    private final transient String name;
    private final transient String description;
    private final transient String changeReason;
    private final transient Map<String, Document> context;
    private final transient int trafficPercentage;
    private final transient List<String> memberExperimentIds;
    private final transient Instant createdAt;
    private final transient String createdBy;
    private final transient Instant lastModifiedAt;
    private final transient String lastModifiedBy;

    private CreateExperimentGroupOutput(Builder builder) {
        this.id = builder.id;
        this.contextHash = builder.contextHash;
        this.name = builder.name;
        this.description = builder.description;
        this.changeReason = builder.changeReason;
        this.context = Collections.unmodifiableMap(builder.context);
        this.trafficPercentage = builder.trafficPercentage;
        this.memberExperimentIds = Collections.unmodifiableList(builder.memberExperimentIds);
        this.createdAt = builder.createdAt;
        this.createdBy = builder.createdBy;
        this.lastModifiedAt = builder.lastModifiedAt;
        this.lastModifiedBy = builder.lastModifiedBy;
    }

    public String id() {
        return id;
    }

    public String contextHash() {
        return contextHash;
    }

    public String name() {
        return name;
    }

    public String description() {
        return description;
    }

    public String changeReason() {
        return changeReason;
    }

    public Map<String, Document> context() {
        return context;
    }

    public boolean hasContext() {
        return true;
    }

    public int trafficPercentage() {
        return trafficPercentage;
    }

    public List<String> memberExperimentIds() {
        return memberExperimentIds;
    }

    public boolean hasMemberExperimentIds() {
        return true;
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
        CreateExperimentGroupOutput that = (CreateExperimentGroupOutput) other;
        return Objects.equals(this.id, that.id)
               && Objects.equals(this.contextHash, that.contextHash)
               && Objects.equals(this.name, that.name)
               && Objects.equals(this.description, that.description)
               && Objects.equals(this.changeReason, that.changeReason)
               && Objects.equals(this.context, that.context)
               && this.trafficPercentage == that.trafficPercentage
               && Objects.equals(this.memberExperimentIds, that.memberExperimentIds)
               && Objects.equals(this.createdAt, that.createdAt)
               && Objects.equals(this.createdBy, that.createdBy)
               && Objects.equals(this.lastModifiedAt, that.lastModifiedAt)
               && Objects.equals(this.lastModifiedBy, that.lastModifiedBy);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, contextHash, name, description, changeReason, context, trafficPercentage, memberExperimentIds, createdAt, createdBy, lastModifiedAt, lastModifiedBy);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_ID, id);
        serializer.writeString($SCHEMA_CONTEXT_HASH, contextHash);
        serializer.writeString($SCHEMA_NAME, name);
        serializer.writeString($SCHEMA_DESCRIPTION, description);
        serializer.writeString($SCHEMA_CHANGE_REASON, changeReason);
        serializer.writeMap($SCHEMA_CONTEXT, context, context.size(), SharedSerde.ConditionSerializer.INSTANCE);
        serializer.writeInteger($SCHEMA_TRAFFIC_PERCENTAGE, trafficPercentage);
        serializer.writeList($SCHEMA_MEMBER_EXPERIMENT_IDS, memberExperimentIds, memberExperimentIds.size(), SharedSerde.StringListSerializer.INSTANCE);
        serializer.writeTimestamp($SCHEMA_CREATED_AT, createdAt);
        serializer.writeString($SCHEMA_CREATED_BY, createdBy);
        serializer.writeTimestamp($SCHEMA_LAST_MODIFIED_AT, lastModifiedAt);
        serializer.writeString($SCHEMA_LAST_MODIFIED_BY, lastModifiedBy);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_ID, member, id);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONTEXT_HASH, member, contextHash);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_NAME, member, name);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, description);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, changeReason);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONTEXT, member, context);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_TRAFFIC_PERCENTAGE, member, trafficPercentage);
            case 7 -> (T) SchemaUtils.validateSameMember($SCHEMA_MEMBER_EXPERIMENT_IDS, member, memberExperimentIds);
            case 8 -> (T) SchemaUtils.validateSameMember($SCHEMA_CREATED_AT, member, createdAt);
            case 9 -> (T) SchemaUtils.validateSameMember($SCHEMA_CREATED_BY, member, createdBy);
            case 10 -> (T) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_AT, member, lastModifiedAt);
            case 11 -> (T) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_BY, member, lastModifiedBy);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link CreateExperimentGroupOutput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.id(this.id);
        builder.contextHash(this.contextHash);
        builder.name(this.name);
        builder.description(this.description);
        builder.changeReason(this.changeReason);
        builder.context(this.context);
        builder.trafficPercentage(this.trafficPercentage);
        builder.memberExperimentIds(this.memberExperimentIds);
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
     * Builder for {@link CreateExperimentGroupOutput}.
     */
    public static final class Builder implements ShapeBuilder<CreateExperimentGroupOutput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String id;
        private String contextHash;
        private String name;
        private String description;
        private String changeReason;
        private Map<String, Document> context;
        private int trafficPercentage;
        private List<String> memberExperimentIds;
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
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder contextHash(String contextHash) {
            this.contextHash = Objects.requireNonNull(contextHash, "contextHash cannot be null");
            tracker.setMember($SCHEMA_CONTEXT_HASH);
            return this;
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

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder context(Map<String, Document> context) {
            this.context = Objects.requireNonNull(context, "context cannot be null");
            tracker.setMember($SCHEMA_CONTEXT);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder trafficPercentage(int trafficPercentage) {
            this.trafficPercentage = trafficPercentage;
            tracker.setMember($SCHEMA_TRAFFIC_PERCENTAGE);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder memberExperimentIds(List<String> memberExperimentIds) {
            this.memberExperimentIds = Objects.requireNonNull(memberExperimentIds, "memberExperimentIds cannot be null");
            tracker.setMember($SCHEMA_MEMBER_EXPERIMENT_IDS);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder createdAt(Instant createdAt) {
            this.createdAt = Objects.requireNonNull(createdAt, "createdAt cannot be null");
            tracker.setMember($SCHEMA_CREATED_AT);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder createdBy(String createdBy) {
            this.createdBy = Objects.requireNonNull(createdBy, "createdBy cannot be null");
            tracker.setMember($SCHEMA_CREATED_BY);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder lastModifiedAt(Instant lastModifiedAt) {
            this.lastModifiedAt = Objects.requireNonNull(lastModifiedAt, "lastModifiedAt cannot be null");
            tracker.setMember($SCHEMA_LAST_MODIFIED_AT);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder lastModifiedBy(String lastModifiedBy) {
            this.lastModifiedBy = Objects.requireNonNull(lastModifiedBy, "lastModifiedBy cannot be null");
            tracker.setMember($SCHEMA_LAST_MODIFIED_BY);
            return this;
        }

        @Override
        public CreateExperimentGroupOutput build() {
            tracker.validate();
            return new CreateExperimentGroupOutput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> id((String) SchemaUtils.validateSameMember($SCHEMA_ID, member, value));
                case 1 -> contextHash((String) SchemaUtils.validateSameMember($SCHEMA_CONTEXT_HASH, member, value));
                case 2 -> name((String) SchemaUtils.validateSameMember($SCHEMA_NAME, member, value));
                case 3 -> description((String) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, value));
                case 4 -> changeReason((String) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, value));
                case 5 -> context((Map<String, Document>) SchemaUtils.validateSameMember($SCHEMA_CONTEXT, member, value));
                case 6 -> trafficPercentage((int) SchemaUtils.validateSameMember($SCHEMA_TRAFFIC_PERCENTAGE, member, value));
                case 7 -> memberExperimentIds((List<String>) SchemaUtils.validateSameMember($SCHEMA_MEMBER_EXPERIMENT_IDS, member, value));
                case 8 -> createdAt((Instant) SchemaUtils.validateSameMember($SCHEMA_CREATED_AT, member, value));
                case 9 -> createdBy((String) SchemaUtils.validateSameMember($SCHEMA_CREATED_BY, member, value));
                case 10 -> lastModifiedAt((Instant) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_AT, member, value));
                case 11 -> lastModifiedBy((String) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_BY, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<CreateExperimentGroupOutput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_ID)) {
                id("");
            }
            if (!tracker.checkMember($SCHEMA_CONTEXT_HASH)) {
                contextHash("");
            }
            if (!tracker.checkMember($SCHEMA_NAME)) {
                name("");
            }
            if (!tracker.checkMember($SCHEMA_DESCRIPTION)) {
                description("");
            }
            if (!tracker.checkMember($SCHEMA_CHANGE_REASON)) {
                changeReason("");
            }
            if (!tracker.checkMember($SCHEMA_CONTEXT)) {
                context(Collections.emptyMap());
            }
            if (!tracker.checkMember($SCHEMA_TRAFFIC_PERCENTAGE)) {
                tracker.setMember($SCHEMA_TRAFFIC_PERCENTAGE);
            }
            if (!tracker.checkMember($SCHEMA_MEMBER_EXPERIMENT_IDS)) {
                memberExperimentIds(Collections.emptyList());
            }
            if (!tracker.checkMember($SCHEMA_CREATED_AT)) {
                createdAt(Instant.EPOCH);
            }
            if (!tracker.checkMember($SCHEMA_CREATED_BY)) {
                createdBy("");
            }
            if (!tracker.checkMember($SCHEMA_LAST_MODIFIED_AT)) {
                lastModifiedAt(Instant.EPOCH);
            }
            if (!tracker.checkMember($SCHEMA_LAST_MODIFIED_BY)) {
                lastModifiedBy("");
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
                    case 1 -> builder.contextHash(de.readString(member));
                    case 2 -> builder.name(de.readString(member));
                    case 3 -> builder.description(de.readString(member));
                    case 4 -> builder.changeReason(de.readString(member));
                    case 5 -> builder.context(SharedSerde.deserializeCondition(member, de));
                    case 6 -> builder.trafficPercentage(de.readInteger(member));
                    case 7 -> builder.memberExperimentIds(SharedSerde.deserializeStringList(member, de));
                    case 8 -> builder.createdAt(de.readTimestamp(member));
                    case 9 -> builder.createdBy(de.readString(member));
                    case 10 -> builder.lastModifiedAt(de.readTimestamp(member));
                    case 11 -> builder.lastModifiedBy(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

