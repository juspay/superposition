
package io.juspay.superposition.model;

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
import software.amazon.smithy.model.traits.HttpHeaderTrait;
import software.amazon.smithy.model.traits.HttpQueryTrait;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class ApplicableVariantsInput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ApplicableVariantsInput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("workspace_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-workspace"),
                new RequiredTrait())
        .putMember("org_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-org-id"),
                new RequiredTrait())
        .putMember("context", SharedSchemas.CONDITION,
                new RequiredTrait())
        .putMember("identifier", PreludeSchemas.STRING,
                new RequiredTrait(),
                new HttpQueryTrait("identifier"))
        .putMember("prefix", SharedSchemas.STRING_LIST,
                new HttpQueryTrait("prefix"))
        .build();

    private static final Schema $SCHEMA_WORKSPACE_ID = $SCHEMA.member("workspace_id");
    private static final Schema $SCHEMA_ORG_ID = $SCHEMA.member("org_id");
    private static final Schema $SCHEMA_CONTEXT = $SCHEMA.member("context");
    private static final Schema $SCHEMA_IDENTIFIER = $SCHEMA.member("identifier");
    private static final Schema $SCHEMA_PREFIX = $SCHEMA.member("prefix");

    private final transient String workspaceId;
    private final transient String orgId;
    private final transient Map<String, Document> context;
    private final transient String identifier;
    private final transient List<String> prefix;

    private ApplicableVariantsInput(Builder builder) {
        this.workspaceId = builder.workspaceId;
        this.orgId = builder.orgId;
        this.context = Collections.unmodifiableMap(builder.context);
        this.identifier = builder.identifier;
        this.prefix = builder.prefix == null ? null : Collections.unmodifiableList(builder.prefix);
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

    public String identifier() {
        return identifier;
    }

    public List<String> prefix() {
        if (prefix == null) {
            return Collections.emptyList();
        }
        return prefix;
    }

    public boolean hasPrefix() {
        return prefix != null;
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
        ApplicableVariantsInput that = (ApplicableVariantsInput) other;
        return Objects.equals(this.workspaceId, that.workspaceId)
               && Objects.equals(this.orgId, that.orgId)
               && Objects.equals(this.context, that.context)
               && Objects.equals(this.identifier, that.identifier)
               && Objects.equals(this.prefix, that.prefix);
    }

    @Override
    public int hashCode() {
        return Objects.hash(workspaceId, orgId, context, identifier, prefix);
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
        serializer.writeString($SCHEMA_IDENTIFIER, identifier);
        if (prefix != null) {
            serializer.writeList($SCHEMA_PREFIX, prefix, prefix.size(), SharedSerde.StringListSerializer.INSTANCE);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, workspaceId);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, orgId);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONTEXT, member, context);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_IDENTIFIER, member, identifier);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_PREFIX, member, prefix);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link ApplicableVariantsInput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.workspaceId(this.workspaceId);
        builder.orgId(this.orgId);
        builder.context(this.context);
        builder.identifier(this.identifier);
        builder.prefix(this.prefix);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link ApplicableVariantsInput}.
     */
    public static final class Builder implements ShapeBuilder<ApplicableVariantsInput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String workspaceId;
        private String orgId;
        private Map<String, Document> context;
        private String identifier;
        private List<String> prefix;

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
            tracker.setMember($SCHEMA_ORG_ID);
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
        public Builder identifier(String identifier) {
            this.identifier = Objects.requireNonNull(identifier, "identifier cannot be null");
            tracker.setMember($SCHEMA_IDENTIFIER);
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder prefix(List<String> prefix) {
            this.prefix = prefix;
            return this;
        }

        @Override
        public ApplicableVariantsInput build() {
            tracker.validate();
            return new ApplicableVariantsInput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> workspaceId((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, value));
                case 1 -> orgId((String) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, value));
                case 2 -> context((Map<String, Document>) SchemaUtils.validateSameMember($SCHEMA_CONTEXT, member, value));
                case 3 -> identifier((String) SchemaUtils.validateSameMember($SCHEMA_IDENTIFIER, member, value));
                case 4 -> prefix((List<String>) SchemaUtils.validateSameMember($SCHEMA_PREFIX, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<ApplicableVariantsInput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_WORKSPACE_ID)) {
                workspaceId("");
            }
            if (!tracker.checkMember($SCHEMA_ORG_ID)) {
                orgId("");
            }
            if (!tracker.checkMember($SCHEMA_CONTEXT)) {
                context(Collections.emptyMap());
            }
            if (!tracker.checkMember($SCHEMA_IDENTIFIER)) {
                identifier("");
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
                    case 1 -> builder.orgId(de.readString(member));
                    case 2 -> builder.context(SharedSerde.deserializeCondition(member, de));
                    case 3 -> builder.identifier(de.readString(member));
                    case 4 -> builder.prefix(SharedSerde.deserializeStringList(member, de));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

