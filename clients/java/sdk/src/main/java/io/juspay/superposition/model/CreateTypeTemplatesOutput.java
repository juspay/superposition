
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
public final class CreateTypeTemplatesOutput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#TypeTemplatesResponse");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("type_name", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("type_schema", SharedSchemas.OBJECT,
                new RequiredTrait())
        .putMember("description", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("change_reason", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("created_by", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("created_at", SharedSchemas.DATE_TIME,
                new RequiredTrait())
        .putMember("last_modified_at", SharedSchemas.DATE_TIME,
                new RequiredTrait())
        .putMember("last_modified_by", PreludeSchemas.STRING,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_TYPE_NAME = $SCHEMA.member("type_name");
    private static final Schema $SCHEMA_TYPE_SCHEMA = $SCHEMA.member("type_schema");
    private static final Schema $SCHEMA_DESCRIPTION = $SCHEMA.member("description");
    private static final Schema $SCHEMA_CHANGE_REASON = $SCHEMA.member("change_reason");
    private static final Schema $SCHEMA_CREATED_BY = $SCHEMA.member("created_by");
    private static final Schema $SCHEMA_CREATED_AT = $SCHEMA.member("created_at");
    private static final Schema $SCHEMA_LAST_MODIFIED_AT = $SCHEMA.member("last_modified_at");
    private static final Schema $SCHEMA_LAST_MODIFIED_BY = $SCHEMA.member("last_modified_by");

    private final transient String typeName;
    private final transient Map<String, Document> typeSchema;
    private final transient String description;
    private final transient String changeReason;
    private final transient String createdBy;
    private final transient Instant createdAt;
    private final transient Instant lastModifiedAt;
    private final transient String lastModifiedBy;

    private CreateTypeTemplatesOutput(Builder builder) {
        this.typeName = builder.typeName;
        this.typeSchema = Collections.unmodifiableMap(builder.typeSchema);
        this.description = builder.description;
        this.changeReason = builder.changeReason;
        this.createdBy = builder.createdBy;
        this.createdAt = builder.createdAt;
        this.lastModifiedAt = builder.lastModifiedAt;
        this.lastModifiedBy = builder.lastModifiedBy;
    }

    public String typeName() {
        return typeName;
    }

    public Map<String, Document> typeSchema() {
        return typeSchema;
    }

    public boolean hasTypeSchema() {
        return true;
    }

    public String description() {
        return description;
    }

    public String changeReason() {
        return changeReason;
    }

    public String createdBy() {
        return createdBy;
    }

    public Instant createdAt() {
        return createdAt;
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
        CreateTypeTemplatesOutput that = (CreateTypeTemplatesOutput) other;
        return Objects.equals(this.typeName, that.typeName)
               && Objects.equals(this.typeSchema, that.typeSchema)
               && Objects.equals(this.description, that.description)
               && Objects.equals(this.changeReason, that.changeReason)
               && Objects.equals(this.createdBy, that.createdBy)
               && Objects.equals(this.createdAt, that.createdAt)
               && Objects.equals(this.lastModifiedAt, that.lastModifiedAt)
               && Objects.equals(this.lastModifiedBy, that.lastModifiedBy);
    }

    @Override
    public int hashCode() {
        return Objects.hash(typeName, typeSchema, description, changeReason, createdBy, createdAt, lastModifiedAt, lastModifiedBy);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_TYPE_NAME, typeName);
        serializer.writeMap($SCHEMA_TYPE_SCHEMA, typeSchema, typeSchema.size(), SharedSerde.ObjectShapeSerializer.INSTANCE);
        serializer.writeString($SCHEMA_DESCRIPTION, description);
        serializer.writeString($SCHEMA_CHANGE_REASON, changeReason);
        serializer.writeString($SCHEMA_CREATED_BY, createdBy);
        serializer.writeTimestamp($SCHEMA_CREATED_AT, createdAt);
        serializer.writeTimestamp($SCHEMA_LAST_MODIFIED_AT, lastModifiedAt);
        serializer.writeString($SCHEMA_LAST_MODIFIED_BY, lastModifiedBy);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_TYPE_NAME, member, typeName);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_TYPE_SCHEMA, member, typeSchema);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, description);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, changeReason);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_CREATED_BY, member, createdBy);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_CREATED_AT, member, createdAt);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_AT, member, lastModifiedAt);
            case 7 -> (T) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_BY, member, lastModifiedBy);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link CreateTypeTemplatesOutput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.typeName(this.typeName);
        builder.typeSchema(this.typeSchema);
        builder.description(this.description);
        builder.changeReason(this.changeReason);
        builder.createdBy(this.createdBy);
        builder.createdAt(this.createdAt);
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
     * Builder for {@link CreateTypeTemplatesOutput}.
     */
    public static final class Builder implements ShapeBuilder<CreateTypeTemplatesOutput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String typeName;
        private Map<String, Document> typeSchema;
        private String description;
        private String changeReason;
        private String createdBy;
        private Instant createdAt;
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
        public Builder typeName(String typeName) {
            this.typeName = Objects.requireNonNull(typeName, "typeName cannot be null");
            tracker.setMember($SCHEMA_TYPE_NAME);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder typeSchema(Map<String, Document> typeSchema) {
            this.typeSchema = Objects.requireNonNull(typeSchema, "typeSchema cannot be null");
            tracker.setMember($SCHEMA_TYPE_SCHEMA);
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
        public Builder createdBy(String createdBy) {
            this.createdBy = Objects.requireNonNull(createdBy, "createdBy cannot be null");
            tracker.setMember($SCHEMA_CREATED_BY);
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
        public CreateTypeTemplatesOutput build() {
            tracker.validate();
            return new CreateTypeTemplatesOutput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> typeName((String) SchemaUtils.validateSameMember($SCHEMA_TYPE_NAME, member, value));
                case 1 -> typeSchema((Map<String, Document>) SchemaUtils.validateSameMember($SCHEMA_TYPE_SCHEMA, member, value));
                case 2 -> description((String) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, value));
                case 3 -> changeReason((String) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, value));
                case 4 -> createdBy((String) SchemaUtils.validateSameMember($SCHEMA_CREATED_BY, member, value));
                case 5 -> createdAt((Instant) SchemaUtils.validateSameMember($SCHEMA_CREATED_AT, member, value));
                case 6 -> lastModifiedAt((Instant) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_AT, member, value));
                case 7 -> lastModifiedBy((String) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_BY, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<CreateTypeTemplatesOutput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_TYPE_NAME)) {
                typeName("");
            }
            if (!tracker.checkMember($SCHEMA_TYPE_SCHEMA)) {
                typeSchema(Collections.emptyMap());
            }
            if (!tracker.checkMember($SCHEMA_DESCRIPTION)) {
                description("");
            }
            if (!tracker.checkMember($SCHEMA_CHANGE_REASON)) {
                changeReason("");
            }
            if (!tracker.checkMember($SCHEMA_CREATED_BY)) {
                createdBy("");
            }
            if (!tracker.checkMember($SCHEMA_CREATED_AT)) {
                createdAt(Instant.EPOCH);
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
                    case 0 -> builder.typeName(de.readString(member));
                    case 1 -> builder.typeSchema(SharedSerde.deserializeObjectShape(member, de));
                    case 2 -> builder.description(de.readString(member));
                    case 3 -> builder.changeReason(de.readString(member));
                    case 4 -> builder.createdBy(de.readString(member));
                    case 5 -> builder.createdAt(de.readTimestamp(member));
                    case 6 -> builder.lastModifiedAt(de.readTimestamp(member));
                    case 7 -> builder.lastModifiedBy(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

