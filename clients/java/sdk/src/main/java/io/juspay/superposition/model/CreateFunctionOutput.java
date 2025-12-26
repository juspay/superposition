
package io.juspay.superposition.model;

import java.time.Instant;
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
public final class CreateFunctionOutput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#FunctionResponse");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("function_name", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("published_code", PreludeSchemas.STRING)
        .putMember("draft_code", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("published_runtime_version", FunctionRuntimeVersion.$SCHEMA)
        .putMember("draft_runtime_version", FunctionRuntimeVersion.$SCHEMA,
                new RequiredTrait())
        .putMember("published_at", SharedSchemas.DATE_TIME)
        .putMember("draft_edited_at", SharedSchemas.DATE_TIME,
                new RequiredTrait())
        .putMember("published_by", PreludeSchemas.STRING)
        .putMember("draft_edited_by", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("last_modified_at", SharedSchemas.DATE_TIME,
                new RequiredTrait())
        .putMember("last_modified_by", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("change_reason", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("description", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("function_type", FunctionTypes.$SCHEMA,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_FUNCTION_NAME = $SCHEMA.member("function_name");
    private static final Schema $SCHEMA_PUBLISHED_CODE = $SCHEMA.member("published_code");
    private static final Schema $SCHEMA_DRAFT_CODE = $SCHEMA.member("draft_code");
    private static final Schema $SCHEMA_PUBLISHED_RUNTIME_VERSION = $SCHEMA.member("published_runtime_version");
    private static final Schema $SCHEMA_DRAFT_RUNTIME_VERSION = $SCHEMA.member("draft_runtime_version");
    private static final Schema $SCHEMA_PUBLISHED_AT = $SCHEMA.member("published_at");
    private static final Schema $SCHEMA_DRAFT_EDITED_AT = $SCHEMA.member("draft_edited_at");
    private static final Schema $SCHEMA_PUBLISHED_BY = $SCHEMA.member("published_by");
    private static final Schema $SCHEMA_DRAFT_EDITED_BY = $SCHEMA.member("draft_edited_by");
    private static final Schema $SCHEMA_LAST_MODIFIED_AT = $SCHEMA.member("last_modified_at");
    private static final Schema $SCHEMA_LAST_MODIFIED_BY = $SCHEMA.member("last_modified_by");
    private static final Schema $SCHEMA_CHANGE_REASON = $SCHEMA.member("change_reason");
    private static final Schema $SCHEMA_DESCRIPTION = $SCHEMA.member("description");
    private static final Schema $SCHEMA_FUNCTION_TYPE = $SCHEMA.member("function_type");

    private final transient String functionName;
    private final transient String publishedCode;
    private final transient String draftCode;
    private final transient FunctionRuntimeVersion publishedRuntimeVersion;
    private final transient FunctionRuntimeVersion draftRuntimeVersion;
    private final transient Instant publishedAt;
    private final transient Instant draftEditedAt;
    private final transient String publishedBy;
    private final transient String draftEditedBy;
    private final transient Instant lastModifiedAt;
    private final transient String lastModifiedBy;
    private final transient String changeReason;
    private final transient String description;
    private final transient FunctionTypes functionType;

    private CreateFunctionOutput(Builder builder) {
        this.functionName = builder.functionName;
        this.publishedCode = builder.publishedCode;
        this.draftCode = builder.draftCode;
        this.publishedRuntimeVersion = builder.publishedRuntimeVersion;
        this.draftRuntimeVersion = builder.draftRuntimeVersion;
        this.publishedAt = builder.publishedAt;
        this.draftEditedAt = builder.draftEditedAt;
        this.publishedBy = builder.publishedBy;
        this.draftEditedBy = builder.draftEditedBy;
        this.lastModifiedAt = builder.lastModifiedAt;
        this.lastModifiedBy = builder.lastModifiedBy;
        this.changeReason = builder.changeReason;
        this.description = builder.description;
        this.functionType = builder.functionType;
    }

    public String functionName() {
        return functionName;
    }

    public String publishedCode() {
        return publishedCode;
    }

    public String draftCode() {
        return draftCode;
    }

    public FunctionRuntimeVersion publishedRuntimeVersion() {
        return publishedRuntimeVersion;
    }

    public FunctionRuntimeVersion draftRuntimeVersion() {
        return draftRuntimeVersion;
    }

    public Instant publishedAt() {
        return publishedAt;
    }

    public Instant draftEditedAt() {
        return draftEditedAt;
    }

    public String publishedBy() {
        return publishedBy;
    }

    public String draftEditedBy() {
        return draftEditedBy;
    }

    public Instant lastModifiedAt() {
        return lastModifiedAt;
    }

    public String lastModifiedBy() {
        return lastModifiedBy;
    }

    public String changeReason() {
        return changeReason;
    }

    public String description() {
        return description;
    }

    public FunctionTypes functionType() {
        return functionType;
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
        CreateFunctionOutput that = (CreateFunctionOutput) other;
        return Objects.equals(this.functionName, that.functionName)
               && Objects.equals(this.publishedCode, that.publishedCode)
               && Objects.equals(this.draftCode, that.draftCode)
               && Objects.equals(this.publishedRuntimeVersion, that.publishedRuntimeVersion)
               && Objects.equals(this.draftRuntimeVersion, that.draftRuntimeVersion)
               && Objects.equals(this.publishedAt, that.publishedAt)
               && Objects.equals(this.draftEditedAt, that.draftEditedAt)
               && Objects.equals(this.publishedBy, that.publishedBy)
               && Objects.equals(this.draftEditedBy, that.draftEditedBy)
               && Objects.equals(this.lastModifiedAt, that.lastModifiedAt)
               && Objects.equals(this.lastModifiedBy, that.lastModifiedBy)
               && Objects.equals(this.changeReason, that.changeReason)
               && Objects.equals(this.description, that.description)
               && Objects.equals(this.functionType, that.functionType);
    }

    @Override
    public int hashCode() {
        return Objects.hash(functionName, publishedCode, draftCode, publishedRuntimeVersion, draftRuntimeVersion, publishedAt, draftEditedAt, publishedBy, draftEditedBy, lastModifiedAt, lastModifiedBy, changeReason, description, functionType);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_FUNCTION_NAME, functionName);
        if (publishedCode != null) {
            serializer.writeString($SCHEMA_PUBLISHED_CODE, publishedCode);
        }
        serializer.writeString($SCHEMA_DRAFT_CODE, draftCode);
        if (publishedRuntimeVersion != null) {
            serializer.writeString($SCHEMA_PUBLISHED_RUNTIME_VERSION, publishedRuntimeVersion.value());
        }
        serializer.writeString($SCHEMA_DRAFT_RUNTIME_VERSION, draftRuntimeVersion.value());
        if (publishedAt != null) {
            serializer.writeTimestamp($SCHEMA_PUBLISHED_AT, publishedAt);
        }
        serializer.writeTimestamp($SCHEMA_DRAFT_EDITED_AT, draftEditedAt);
        if (publishedBy != null) {
            serializer.writeString($SCHEMA_PUBLISHED_BY, publishedBy);
        }
        serializer.writeString($SCHEMA_DRAFT_EDITED_BY, draftEditedBy);
        serializer.writeTimestamp($SCHEMA_LAST_MODIFIED_AT, lastModifiedAt);
        serializer.writeString($SCHEMA_LAST_MODIFIED_BY, lastModifiedBy);
        serializer.writeString($SCHEMA_CHANGE_REASON, changeReason);
        serializer.writeString($SCHEMA_DESCRIPTION, description);
        serializer.writeString($SCHEMA_FUNCTION_TYPE, functionType.value());
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_FUNCTION_NAME, member, functionName);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_DRAFT_CODE, member, draftCode);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_DRAFT_RUNTIME_VERSION, member, draftRuntimeVersion);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_DRAFT_EDITED_AT, member, draftEditedAt);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_DRAFT_EDITED_BY, member, draftEditedBy);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_AT, member, lastModifiedAt);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_BY, member, lastModifiedBy);
            case 7 -> (T) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, changeReason);
            case 8 -> (T) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, description);
            case 9 -> (T) SchemaUtils.validateSameMember($SCHEMA_FUNCTION_TYPE, member, functionType);
            case 10 -> (T) SchemaUtils.validateSameMember($SCHEMA_PUBLISHED_CODE, member, publishedCode);
            case 11 -> (T) SchemaUtils.validateSameMember($SCHEMA_PUBLISHED_RUNTIME_VERSION, member, publishedRuntimeVersion);
            case 12 -> (T) SchemaUtils.validateSameMember($SCHEMA_PUBLISHED_AT, member, publishedAt);
            case 13 -> (T) SchemaUtils.validateSameMember($SCHEMA_PUBLISHED_BY, member, publishedBy);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link CreateFunctionOutput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.functionName(this.functionName);
        builder.publishedCode(this.publishedCode);
        builder.draftCode(this.draftCode);
        builder.publishedRuntimeVersion(this.publishedRuntimeVersion);
        builder.draftRuntimeVersion(this.draftRuntimeVersion);
        builder.publishedAt(this.publishedAt);
        builder.draftEditedAt(this.draftEditedAt);
        builder.publishedBy(this.publishedBy);
        builder.draftEditedBy(this.draftEditedBy);
        builder.lastModifiedAt(this.lastModifiedAt);
        builder.lastModifiedBy(this.lastModifiedBy);
        builder.changeReason(this.changeReason);
        builder.description(this.description);
        builder.functionType(this.functionType);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link CreateFunctionOutput}.
     */
    public static final class Builder implements ShapeBuilder<CreateFunctionOutput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String functionName;
        private String publishedCode;
        private String draftCode;
        private FunctionRuntimeVersion publishedRuntimeVersion;
        private FunctionRuntimeVersion draftRuntimeVersion;
        private Instant publishedAt;
        private Instant draftEditedAt;
        private String publishedBy;
        private String draftEditedBy;
        private Instant lastModifiedAt;
        private String lastModifiedBy;
        private String changeReason;
        private String description;
        private FunctionTypes functionType;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder functionName(String functionName) {
            this.functionName = Objects.requireNonNull(functionName, "functionName cannot be null");
            tracker.setMember($SCHEMA_FUNCTION_NAME);
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder publishedCode(String publishedCode) {
            this.publishedCode = publishedCode;
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder draftCode(String draftCode) {
            this.draftCode = Objects.requireNonNull(draftCode, "draftCode cannot be null");
            tracker.setMember($SCHEMA_DRAFT_CODE);
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder publishedRuntimeVersion(FunctionRuntimeVersion publishedRuntimeVersion) {
            this.publishedRuntimeVersion = publishedRuntimeVersion;
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder draftRuntimeVersion(FunctionRuntimeVersion draftRuntimeVersion) {
            this.draftRuntimeVersion = Objects.requireNonNull(draftRuntimeVersion, "draftRuntimeVersion cannot be null");
            tracker.setMember($SCHEMA_DRAFT_RUNTIME_VERSION);
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder publishedAt(Instant publishedAt) {
            this.publishedAt = publishedAt;
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder draftEditedAt(Instant draftEditedAt) {
            this.draftEditedAt = Objects.requireNonNull(draftEditedAt, "draftEditedAt cannot be null");
            tracker.setMember($SCHEMA_DRAFT_EDITED_AT);
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder publishedBy(String publishedBy) {
            this.publishedBy = publishedBy;
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder draftEditedBy(String draftEditedBy) {
            this.draftEditedBy = Objects.requireNonNull(draftEditedBy, "draftEditedBy cannot be null");
            tracker.setMember($SCHEMA_DRAFT_EDITED_BY);
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
        public Builder description(String description) {
            this.description = Objects.requireNonNull(description, "description cannot be null");
            tracker.setMember($SCHEMA_DESCRIPTION);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder functionType(FunctionTypes functionType) {
            this.functionType = Objects.requireNonNull(functionType, "functionType cannot be null");
            tracker.setMember($SCHEMA_FUNCTION_TYPE);
            return this;
        }

        @Override
        public CreateFunctionOutput build() {
            tracker.validate();
            return new CreateFunctionOutput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> functionName((String) SchemaUtils.validateSameMember($SCHEMA_FUNCTION_NAME, member, value));
                case 1 -> draftCode((String) SchemaUtils.validateSameMember($SCHEMA_DRAFT_CODE, member, value));
                case 2 -> draftRuntimeVersion((FunctionRuntimeVersion) SchemaUtils.validateSameMember($SCHEMA_DRAFT_RUNTIME_VERSION, member, value));
                case 3 -> draftEditedAt((Instant) SchemaUtils.validateSameMember($SCHEMA_DRAFT_EDITED_AT, member, value));
                case 4 -> draftEditedBy((String) SchemaUtils.validateSameMember($SCHEMA_DRAFT_EDITED_BY, member, value));
                case 5 -> lastModifiedAt((Instant) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_AT, member, value));
                case 6 -> lastModifiedBy((String) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_BY, member, value));
                case 7 -> changeReason((String) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, value));
                case 8 -> description((String) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, value));
                case 9 -> functionType((FunctionTypes) SchemaUtils.validateSameMember($SCHEMA_FUNCTION_TYPE, member, value));
                case 10 -> publishedCode((String) SchemaUtils.validateSameMember($SCHEMA_PUBLISHED_CODE, member, value));
                case 11 -> publishedRuntimeVersion((FunctionRuntimeVersion) SchemaUtils.validateSameMember($SCHEMA_PUBLISHED_RUNTIME_VERSION, member, value));
                case 12 -> publishedAt((Instant) SchemaUtils.validateSameMember($SCHEMA_PUBLISHED_AT, member, value));
                case 13 -> publishedBy((String) SchemaUtils.validateSameMember($SCHEMA_PUBLISHED_BY, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<CreateFunctionOutput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_FUNCTION_NAME)) {
                functionName("");
            }
            if (!tracker.checkMember($SCHEMA_DRAFT_CODE)) {
                draftCode("");
            }
            if (!tracker.checkMember($SCHEMA_DRAFT_RUNTIME_VERSION)) {
                draftRuntimeVersion(FunctionRuntimeVersion.unknown(""));
            }
            if (!tracker.checkMember($SCHEMA_DRAFT_EDITED_AT)) {
                draftEditedAt(Instant.EPOCH);
            }
            if (!tracker.checkMember($SCHEMA_DRAFT_EDITED_BY)) {
                draftEditedBy("");
            }
            if (!tracker.checkMember($SCHEMA_LAST_MODIFIED_AT)) {
                lastModifiedAt(Instant.EPOCH);
            }
            if (!tracker.checkMember($SCHEMA_LAST_MODIFIED_BY)) {
                lastModifiedBy("");
            }
            if (!tracker.checkMember($SCHEMA_CHANGE_REASON)) {
                changeReason("");
            }
            if (!tracker.checkMember($SCHEMA_DESCRIPTION)) {
                description("");
            }
            if (!tracker.checkMember($SCHEMA_FUNCTION_TYPE)) {
                functionType(FunctionTypes.unknown(""));
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
                    case 0 -> builder.functionName(de.readString(member));
                    case 1 -> builder.draftCode(de.readString(member));
                    case 2 -> builder.draftRuntimeVersion(FunctionRuntimeVersion.builder().deserializeMember(de, member).build());
                    case 3 -> builder.draftEditedAt(de.readTimestamp(member));
                    case 4 -> builder.draftEditedBy(de.readString(member));
                    case 5 -> builder.lastModifiedAt(de.readTimestamp(member));
                    case 6 -> builder.lastModifiedBy(de.readString(member));
                    case 7 -> builder.changeReason(de.readString(member));
                    case 8 -> builder.description(de.readString(member));
                    case 9 -> builder.functionType(FunctionTypes.builder().deserializeMember(de, member).build());
                    case 10 -> builder.publishedCode(de.readString(member));
                    case 11 -> builder.publishedRuntimeVersion(FunctionRuntimeVersion.builder().deserializeMember(de, member).build());
                    case 12 -> builder.publishedAt(de.readTimestamp(member));
                    case 13 -> builder.publishedBy(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

