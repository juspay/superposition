
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
import software.amazon.smithy.model.node.Node;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.DefaultTrait;
import software.amazon.smithy.model.traits.HttpHeaderTrait;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class CreateContextInput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#CreateContextInput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("workspace_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-tenant"),
                new RequiredTrait())
        .putMember("org_id", PreludeSchemas.STRING,
                new DefaultTrait(Node.from("juspay")),
                new RequiredTrait(),
                new HttpHeaderTrait("x-org-id"))
        .putMember("context", SharedSchemas.CONDITION,
                new RequiredTrait())
        .putMember("config_tags", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-config-tags"))
        .putMember("override", SharedSchemas.OVERRIDES,
                new RequiredTrait())
        .putMember("description", PreludeSchemas.STRING)
        .putMember("change_reason", PreludeSchemas.STRING,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_WORKSPACE_ID = $SCHEMA.member("workspace_id");
    private static final Schema $SCHEMA_ORG_ID = $SCHEMA.member("org_id");
    private static final Schema $SCHEMA_CONTEXT = $SCHEMA.member("context");
    private static final Schema $SCHEMA_CONFIG_TAGS = $SCHEMA.member("config_tags");
    private static final Schema $SCHEMA_OVERRIDE = $SCHEMA.member("override");
    private static final Schema $SCHEMA_DESCRIPTION = $SCHEMA.member("description");
    private static final Schema $SCHEMA_CHANGE_REASON = $SCHEMA.member("change_reason");

    private final transient String workspaceId;
    private final transient String orgId;
    private final transient Map<String, Document> context;
    private final transient String configTags;
    private final transient Map<String, Document> override;
    private final transient String description;
    private final transient String changeReason;

    private CreateContextInput(Builder builder) {
        this.workspaceId = builder.workspaceId;
        this.orgId = builder.orgId;
        this.context = Collections.unmodifiableMap(builder.context);
        this.configTags = builder.configTags;
        this.override = Collections.unmodifiableMap(builder.override);
        this.description = builder.description;
        this.changeReason = builder.changeReason;
    }

    public String workspaceId() {
        return workspaceId;
    }

    public String orgId() {
        return orgId;
    }

    public Map<String, Document> context() {
        return context;
    }

    public boolean hasContext() {
        return true;
    }

    public String configTags() {
        return configTags;
    }

    public Map<String, Document> override() {
        return override;
    }

    public boolean hasOverride() {
        return true;
    }

    public String description() {
        return description;
    }

    public String changeReason() {
        return changeReason;
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
        CreateContextInput that = (CreateContextInput) other;
        return Objects.equals(this.workspaceId, that.workspaceId)
               && Objects.equals(this.orgId, that.orgId)
               && Objects.equals(this.context, that.context)
               && Objects.equals(this.configTags, that.configTags)
               && Objects.equals(this.override, that.override)
               && Objects.equals(this.description, that.description)
               && Objects.equals(this.changeReason, that.changeReason);
    }

    @Override
    public int hashCode() {
        return Objects.hash(workspaceId, orgId, context, configTags, override, description, changeReason);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_WORKSPACE_ID, workspaceId);
        serializer.writeString($SCHEMA_ORG_ID, orgId);
        serializer.writeMap($SCHEMA_CONTEXT, context, context.size(), SharedSerde.ConditionSerializer.INSTANCE);
        if (configTags != null) {
            serializer.writeString($SCHEMA_CONFIG_TAGS, configTags);
        }
        serializer.writeMap($SCHEMA_OVERRIDE, override, override.size(), SharedSerde.OverridesSerializer.INSTANCE);
        if (description != null) {
            serializer.writeString($SCHEMA_DESCRIPTION, description);
        }
        serializer.writeString($SCHEMA_CHANGE_REASON, changeReason);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, workspaceId);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONTEXT, member, context);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_OVERRIDE, member, override);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, changeReason);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, orgId);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONFIG_TAGS, member, configTags);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, description);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link CreateContextInput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.workspaceId(this.workspaceId);
        builder.orgId(this.orgId);
        builder.context(this.context);
        builder.configTags(this.configTags);
        builder.override(this.override);
        builder.description(this.description);
        builder.changeReason(this.changeReason);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link CreateContextInput}.
     */
    public static final class Builder implements ShapeBuilder<CreateContextInput> {
        private static final String ORG_ID_DEFAULT = "juspay";
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String workspaceId;
        private String orgId = ORG_ID_DEFAULT;
        private Map<String, Document> context;
        private String configTags;
        private Map<String, Document> override;
        private String description;
        private String changeReason;

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
        public Builder context(Map<String, Document> context) {
            this.context = Objects.requireNonNull(context, "context cannot be null");
            tracker.setMember($SCHEMA_CONTEXT);
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder configTags(String configTags) {
            this.configTags = configTags;
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder override(Map<String, Document> override) {
            this.override = Objects.requireNonNull(override, "override cannot be null");
            tracker.setMember($SCHEMA_OVERRIDE);
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

        @Override
        public CreateContextInput build() {
            tracker.validate();
            return new CreateContextInput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> workspaceId((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, value));
                case 1 -> context((Map<String, Document>) SchemaUtils.validateSameMember($SCHEMA_CONTEXT, member, value));
                case 2 -> override((Map<String, Document>) SchemaUtils.validateSameMember($SCHEMA_OVERRIDE, member, value));
                case 3 -> changeReason((String) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, value));
                case 4 -> orgId((String) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, value));
                case 5 -> configTags((String) SchemaUtils.validateSameMember($SCHEMA_CONFIG_TAGS, member, value));
                case 6 -> description((String) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<CreateContextInput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_WORKSPACE_ID)) {
                workspaceId("");
            }
            if (!tracker.checkMember($SCHEMA_CONTEXT)) {
                context(Collections.emptyMap());
            }
            if (!tracker.checkMember($SCHEMA_OVERRIDE)) {
                override(Collections.emptyMap());
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
                    case 1 -> builder.context(SharedSerde.deserializeCondition(member, de));
                    case 2 -> builder.override(SharedSerde.deserializeOverrides(member, de));
                    case 3 -> builder.changeReason(de.readString(member));
                    case 4 -> builder.orgId(de.readString(member));
                    case 5 -> builder.configTags(de.readString(member));
                    case 6 -> builder.description(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

