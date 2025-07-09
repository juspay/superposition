
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
import software.amazon.smithy.model.node.Node;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.DefaultTrait;
import software.amazon.smithy.model.traits.HttpHeaderTrait;
import software.amazon.smithy.model.traits.HttpLabelTrait;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class UpdateFunctionInput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#UpdateFunctionRequest");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("workspace_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-tenant"),
                new RequiredTrait())
        .putMember("org_id", PreludeSchemas.STRING,
                new DefaultTrait(Node.from("juspay")),
                new RequiredTrait(),
                new HttpHeaderTrait("x-org-id"))
        .putMember("function_name", PreludeSchemas.STRING,
                new HttpLabelTrait(),
                new RequiredTrait())
        .putMember("description", PreludeSchemas.STRING)
        .putMember("change_reason", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("function", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("runtime_version", PreludeSchemas.STRING,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_WORKSPACE_ID = $SCHEMA.member("workspace_id");
    private static final Schema $SCHEMA_ORG_ID = $SCHEMA.member("org_id");
    private static final Schema $SCHEMA_FUNCTION_NAME = $SCHEMA.member("function_name");
    private static final Schema $SCHEMA_DESCRIPTION = $SCHEMA.member("description");
    private static final Schema $SCHEMA_CHANGE_REASON = $SCHEMA.member("change_reason");
    private static final Schema $SCHEMA_FUNCTION = $SCHEMA.member("function");
    private static final Schema $SCHEMA_RUNTIME_VERSION = $SCHEMA.member("runtime_version");

    private final transient String workspaceId;
    private final transient String orgId;
    private final transient String functionName;
    private final transient String description;
    private final transient String changeReason;
    private final transient String function;
    private final transient String runtimeVersion;

    private UpdateFunctionInput(Builder builder) {
        this.workspaceId = builder.workspaceId;
        this.orgId = builder.orgId;
        this.functionName = builder.functionName;
        this.description = builder.description;
        this.changeReason = builder.changeReason;
        this.function = builder.function;
        this.runtimeVersion = builder.runtimeVersion;
    }

    public String workspaceId() {
        return workspaceId;
    }

    public String orgId() {
        return orgId;
    }

    public String functionName() {
        return functionName;
    }

    public String description() {
        return description;
    }

    public String changeReason() {
        return changeReason;
    }

    public String function() {
        return function;
    }

    public String runtimeVersion() {
        return runtimeVersion;
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
        UpdateFunctionInput that = (UpdateFunctionInput) other;
        return Objects.equals(this.workspaceId, that.workspaceId)
               && Objects.equals(this.orgId, that.orgId)
               && Objects.equals(this.functionName, that.functionName)
               && Objects.equals(this.description, that.description)
               && Objects.equals(this.changeReason, that.changeReason)
               && Objects.equals(this.function, that.function)
               && Objects.equals(this.runtimeVersion, that.runtimeVersion);
    }

    @Override
    public int hashCode() {
        return Objects.hash(workspaceId, orgId, functionName, description, changeReason, function, runtimeVersion);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_WORKSPACE_ID, workspaceId);
        serializer.writeString($SCHEMA_ORG_ID, orgId);
        serializer.writeString($SCHEMA_FUNCTION_NAME, functionName);
        if (description != null) {
            serializer.writeString($SCHEMA_DESCRIPTION, description);
        }
        serializer.writeString($SCHEMA_CHANGE_REASON, changeReason);
        serializer.writeString($SCHEMA_FUNCTION, function);
        serializer.writeString($SCHEMA_RUNTIME_VERSION, runtimeVersion);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, workspaceId);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_FUNCTION_NAME, member, functionName);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, changeReason);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_FUNCTION, member, function);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_RUNTIME_VERSION, member, runtimeVersion);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, orgId);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, description);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link UpdateFunctionInput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.workspaceId(this.workspaceId);
        builder.orgId(this.orgId);
        builder.functionName(this.functionName);
        builder.description(this.description);
        builder.changeReason(this.changeReason);
        builder.function(this.function);
        builder.runtimeVersion(this.runtimeVersion);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link UpdateFunctionInput}.
     */
    public static final class Builder implements ShapeBuilder<UpdateFunctionInput> {
        private static final String ORG_ID_DEFAULT = "juspay";
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String workspaceId;
        private String orgId = ORG_ID_DEFAULT;
        private String functionName;
        private String description;
        private String changeReason;
        private String function;
        private String runtimeVersion;

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
        public Builder functionName(String functionName) {
            this.functionName = Objects.requireNonNull(functionName, "functionName cannot be null");
            tracker.setMember($SCHEMA_FUNCTION_NAME);
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

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder function(String function) {
            this.function = Objects.requireNonNull(function, "function cannot be null");
            tracker.setMember($SCHEMA_FUNCTION);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder runtimeVersion(String runtimeVersion) {
            this.runtimeVersion = Objects.requireNonNull(runtimeVersion, "runtimeVersion cannot be null");
            tracker.setMember($SCHEMA_RUNTIME_VERSION);
            return this;
        }

        @Override
        public UpdateFunctionInput build() {
            tracker.validate();
            return new UpdateFunctionInput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> workspaceId((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, value));
                case 1 -> functionName((String) SchemaUtils.validateSameMember($SCHEMA_FUNCTION_NAME, member, value));
                case 2 -> changeReason((String) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, value));
                case 3 -> function((String) SchemaUtils.validateSameMember($SCHEMA_FUNCTION, member, value));
                case 4 -> runtimeVersion((String) SchemaUtils.validateSameMember($SCHEMA_RUNTIME_VERSION, member, value));
                case 5 -> orgId((String) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, value));
                case 6 -> description((String) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<UpdateFunctionInput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_WORKSPACE_ID)) {
                workspaceId("");
            }
            if (!tracker.checkMember($SCHEMA_FUNCTION_NAME)) {
                functionName("");
            }
            if (!tracker.checkMember($SCHEMA_CHANGE_REASON)) {
                changeReason("");
            }
            if (!tracker.checkMember($SCHEMA_FUNCTION)) {
                function("");
            }
            if (!tracker.checkMember($SCHEMA_RUNTIME_VERSION)) {
                runtimeVersion("");
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
                    case 1 -> builder.functionName(de.readString(member));
                    case 2 -> builder.changeReason(de.readString(member));
                    case 3 -> builder.function(de.readString(member));
                    case 4 -> builder.runtimeVersion(de.readString(member));
                    case 5 -> builder.orgId(de.readString(member));
                    case 6 -> builder.description(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

