
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
import software.amazon.smithy.java.core.serde.document.Document;
import software.amazon.smithy.model.node.Node;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.DefaultTrait;
import software.amazon.smithy.model.traits.HttpHeaderTrait;
import software.amazon.smithy.model.traits.HttpLabelTrait;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class UpdateDefaultConfigInput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#UpdateDefaultConfigInput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("workspace_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-tenant"),
                new RequiredTrait())
        .putMember("org_id", PreludeSchemas.STRING,
                new DefaultTrait(Node.from("juspay")),
                new RequiredTrait(),
                new HttpHeaderTrait("x-org-id"))
        .putMember("key", PreludeSchemas.STRING,
                new HttpLabelTrait(),
                new RequiredTrait())
        .putMember("change_reason", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("value", PreludeSchemas.DOCUMENT)
        .putMember("schema", PreludeSchemas.DOCUMENT)
        .putMember("function_name", PreludeSchemas.STRING)
        .putMember("description", PreludeSchemas.STRING)
        .putMember("autocomplete_function_name", PreludeSchemas.STRING)
        .build();

    private static final Schema $SCHEMA_WORKSPACE_ID = $SCHEMA.member("workspace_id");
    private static final Schema $SCHEMA_ORG_ID = $SCHEMA.member("org_id");
    private static final Schema $SCHEMA_KEY = $SCHEMA.member("key");
    private static final Schema $SCHEMA_CHANGE_REASON = $SCHEMA.member("change_reason");
    private static final Schema $SCHEMA_VALUE = $SCHEMA.member("value");
    private static final Schema $SCHEMA_SCHEMA_MEMBER = $SCHEMA.member("schema");
    private static final Schema $SCHEMA_FUNCTION_NAME = $SCHEMA.member("function_name");
    private static final Schema $SCHEMA_DESCRIPTION = $SCHEMA.member("description");
    private static final Schema $SCHEMA_AUTOCOMPLETE_FUNCTION_NAME = $SCHEMA.member("autocomplete_function_name");

    private final transient String workspaceId;
    private final transient String orgId;
    private final transient String key;
    private final transient String changeReason;
    private final transient Document value;
    private final transient Document schemaMember;
    private final transient String functionName;
    private final transient String description;
    private final transient String autocompleteFunctionName;

    private UpdateDefaultConfigInput(Builder builder) {
        this.workspaceId = builder.workspaceId;
        this.orgId = builder.orgId;
        this.key = builder.key;
        this.changeReason = builder.changeReason;
        this.value = builder.value;
        this.schemaMember = builder.schemaMember;
        this.functionName = builder.functionName;
        this.description = builder.description;
        this.autocompleteFunctionName = builder.autocompleteFunctionName;
    }

    public String workspaceId() {
        return workspaceId;
    }

    public String orgId() {
        return orgId;
    }

    public String key() {
        return key;
    }

    public String changeReason() {
        return changeReason;
    }

    public Document value() {
        return value;
    }

    public Document schemaMember() {
        return schemaMember;
    }

    public String functionName() {
        return functionName;
    }

    public String description() {
        return description;
    }

    public String autocompleteFunctionName() {
        return autocompleteFunctionName;
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
        UpdateDefaultConfigInput that = (UpdateDefaultConfigInput) other;
        return Objects.equals(this.workspaceId, that.workspaceId)
               && Objects.equals(this.orgId, that.orgId)
               && Objects.equals(this.key, that.key)
               && Objects.equals(this.changeReason, that.changeReason)
               && Objects.equals(this.value, that.value)
               && Objects.equals(this.schemaMember, that.schemaMember)
               && Objects.equals(this.functionName, that.functionName)
               && Objects.equals(this.description, that.description)
               && Objects.equals(this.autocompleteFunctionName, that.autocompleteFunctionName);
    }

    @Override
    public int hashCode() {
        return Objects.hash(workspaceId, orgId, key, changeReason, value, schemaMember, functionName, description, autocompleteFunctionName);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_WORKSPACE_ID, workspaceId);
        serializer.writeString($SCHEMA_ORG_ID, orgId);
        serializer.writeString($SCHEMA_KEY, key);
        serializer.writeString($SCHEMA_CHANGE_REASON, changeReason);
        if (value != null) {
            serializer.writeDocument($SCHEMA_VALUE, value);
        }
        if (schemaMember != null) {
            serializer.writeDocument($SCHEMA_SCHEMA_MEMBER, schemaMember);
        }
        if (functionName != null) {
            serializer.writeString($SCHEMA_FUNCTION_NAME, functionName);
        }
        if (description != null) {
            serializer.writeString($SCHEMA_DESCRIPTION, description);
        }
        if (autocompleteFunctionName != null) {
            serializer.writeString($SCHEMA_AUTOCOMPLETE_FUNCTION_NAME, autocompleteFunctionName);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, workspaceId);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_KEY, member, key);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, changeReason);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, orgId);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_VALUE, member, value);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_SCHEMA_MEMBER, member, schemaMember);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_FUNCTION_NAME, member, functionName);
            case 7 -> (T) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, description);
            case 8 -> (T) SchemaUtils.validateSameMember($SCHEMA_AUTOCOMPLETE_FUNCTION_NAME, member, autocompleteFunctionName);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link UpdateDefaultConfigInput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.workspaceId(this.workspaceId);
        builder.orgId(this.orgId);
        builder.key(this.key);
        builder.changeReason(this.changeReason);
        builder.value(this.value);
        builder.schemaMember(this.schemaMember);
        builder.functionName(this.functionName);
        builder.description(this.description);
        builder.autocompleteFunctionName(this.autocompleteFunctionName);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link UpdateDefaultConfigInput}.
     */
    public static final class Builder implements ShapeBuilder<UpdateDefaultConfigInput> {
        private static final String ORG_ID_DEFAULT = "juspay";
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String workspaceId;
        private String orgId = ORG_ID_DEFAULT;
        private String key;
        private String changeReason;
        private Document value;
        private Document schemaMember;
        private String functionName;
        private String description;
        private String autocompleteFunctionName;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder workspaceId(String workspaceId) {
            this.workspaceId = Objects.requireNonNull(workspaceId, "workspaceId cannot be null");
            tracker.setMember($SCHEMA_WORKSPACE_ID);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder orgId(String orgId) {
            this.orgId = Objects.requireNonNull(orgId, "orgId cannot be null");
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder key(String key) {
            this.key = Objects.requireNonNull(key, "key cannot be null");
            tracker.setMember($SCHEMA_KEY);
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
         * @return this builder.
         */
        public Builder value(Document value) {
            this.value = value;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder schemaMember(Document schemaMember) {
            this.schemaMember = schemaMember;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder functionName(String functionName) {
            this.functionName = functionName;
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
        public Builder autocompleteFunctionName(String autocompleteFunctionName) {
            this.autocompleteFunctionName = autocompleteFunctionName;
            return this;
        }

        @Override
        public UpdateDefaultConfigInput build() {
            tracker.validate();
            return new UpdateDefaultConfigInput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> workspaceId((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, value));
                case 1 -> key((String) SchemaUtils.validateSameMember($SCHEMA_KEY, member, value));
                case 2 -> changeReason((String) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, value));
                case 3 -> orgId((String) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, value));
                case 4 -> value((Document) SchemaUtils.validateSameMember($SCHEMA_VALUE, member, value));
                case 5 -> schemaMember((Document) SchemaUtils.validateSameMember($SCHEMA_SCHEMA_MEMBER, member, value));
                case 6 -> functionName((String) SchemaUtils.validateSameMember($SCHEMA_FUNCTION_NAME, member, value));
                case 7 -> description((String) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, value));
                case 8 -> autocompleteFunctionName((String) SchemaUtils.validateSameMember($SCHEMA_AUTOCOMPLETE_FUNCTION_NAME, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<UpdateDefaultConfigInput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_WORKSPACE_ID)) {
                workspaceId("");
            }
            if (!tracker.checkMember($SCHEMA_KEY)) {
                key("");
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
                    case 0 -> builder.workspaceId(de.readString(member));
                    case 1 -> builder.key(de.readString(member));
                    case 2 -> builder.changeReason(de.readString(member));
                    case 3 -> builder.orgId(de.readString(member));
                    case 4 -> builder.value(de.readDocument());
                    case 5 -> builder.schemaMember(de.readDocument());
                    case 6 -> builder.functionName(de.readString(member));
                    case 7 -> builder.description(de.readString(member));
                    case 8 -> builder.autocompleteFunctionName(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

