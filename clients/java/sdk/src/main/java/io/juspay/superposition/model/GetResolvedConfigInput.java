
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
public final class GetResolvedConfigInput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#GetResolvedConfigInput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("workspace_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-workspace"),
                new RequiredTrait())
        .putMember("org_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-org-id"),
                new RequiredTrait())
        .putMember("prefix", SharedSchemas.STRING_LIST,
                new HttpQueryTrait("prefix"))
        .putMember("version", PreludeSchemas.STRING,
                new HttpQueryTrait("version"))
        .putMember("show_reasoning", PreludeSchemas.BOOLEAN,
                new HttpQueryTrait("show_reasoning"))
        .putMember("merge_strategy", MergeStrategy.$SCHEMA,
                new HttpHeaderTrait("x-merge-strategy"))
        .putMember("context_id", PreludeSchemas.STRING,
                new HttpQueryTrait("context_id"))
        .putMember("context", SharedSchemas.CONTEXT_MAP)
        .build();

    private static final Schema $SCHEMA_WORKSPACE_ID = $SCHEMA.member("workspace_id");
    private static final Schema $SCHEMA_ORG_ID = $SCHEMA.member("org_id");
    private static final Schema $SCHEMA_PREFIX = $SCHEMA.member("prefix");
    private static final Schema $SCHEMA_VERSION = $SCHEMA.member("version");
    private static final Schema $SCHEMA_SHOW_REASONING = $SCHEMA.member("show_reasoning");
    private static final Schema $SCHEMA_MERGE_STRATEGY = $SCHEMA.member("merge_strategy");
    private static final Schema $SCHEMA_CONTEXT_ID = $SCHEMA.member("context_id");
    private static final Schema $SCHEMA_CONTEXT = $SCHEMA.member("context");

    private final transient String workspaceId;
    private final transient String orgId;
    private final transient List<String> prefix;
    private final transient String version;
    private final transient Boolean showReasoning;
    private final transient MergeStrategy mergeStrategy;
    private final transient String contextId;
    private final transient Map<String, Document> context;

    private GetResolvedConfigInput(Builder builder) {
        this.workspaceId = builder.workspaceId;
        this.orgId = builder.orgId;
        this.prefix = builder.prefix == null ? null : Collections.unmodifiableList(builder.prefix);
        this.version = builder.version;
        this.showReasoning = builder.showReasoning;
        this.mergeStrategy = builder.mergeStrategy;
        this.contextId = builder.contextId;
        this.context = builder.context == null ? null : Collections.unmodifiableMap(builder.context);
    }

    public String workspaceId() {
        return workspaceId;
    }

    public String orgId() {
        return orgId;
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

    public String version() {
        return version;
    }

    public Boolean showReasoning() {
        return showReasoning;
    }

    public MergeStrategy mergeStrategy() {
        return mergeStrategy;
    }

    public String contextId() {
        return contextId;
    }

    public Map<String, Document> context() {
        if (context == null) {
            return Collections.emptyMap();
        }
        return context;
    }

    public boolean hasContext() {
        return context != null;
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
        GetResolvedConfigInput that = (GetResolvedConfigInput) other;
        return Objects.equals(this.workspaceId, that.workspaceId)
               && Objects.equals(this.orgId, that.orgId)
               && Objects.equals(this.prefix, that.prefix)
               && Objects.equals(this.version, that.version)
               && Objects.equals(this.showReasoning, that.showReasoning)
               && Objects.equals(this.mergeStrategy, that.mergeStrategy)
               && Objects.equals(this.contextId, that.contextId)
               && Objects.equals(this.context, that.context);
    }

    @Override
    public int hashCode() {
        return Objects.hash(workspaceId, orgId, prefix, version, showReasoning, mergeStrategy, contextId, context);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_WORKSPACE_ID, workspaceId);
        serializer.writeString($SCHEMA_ORG_ID, orgId);
        if (prefix != null) {
            serializer.writeList($SCHEMA_PREFIX, prefix, prefix.size(), SharedSerde.StringListSerializer.INSTANCE);
        }
        if (version != null) {
            serializer.writeString($SCHEMA_VERSION, version);
        }
        if (showReasoning != null) {
            serializer.writeBoolean($SCHEMA_SHOW_REASONING, showReasoning);
        }
        if (mergeStrategy != null) {
            serializer.writeString($SCHEMA_MERGE_STRATEGY, mergeStrategy.value());
        }
        if (contextId != null) {
            serializer.writeString($SCHEMA_CONTEXT_ID, contextId);
        }
        if (context != null) {
            serializer.writeMap($SCHEMA_CONTEXT, context, context.size(), SharedSerde.ContextMapSerializer.INSTANCE);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, workspaceId);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, orgId);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_PREFIX, member, prefix);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_VERSION, member, version);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_SHOW_REASONING, member, showReasoning);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_MERGE_STRATEGY, member, mergeStrategy);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONTEXT_ID, member, contextId);
            case 7 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONTEXT, member, context);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link GetResolvedConfigInput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.workspaceId(this.workspaceId);
        builder.orgId(this.orgId);
        builder.prefix(this.prefix);
        builder.version(this.version);
        builder.showReasoning(this.showReasoning);
        builder.mergeStrategy(this.mergeStrategy);
        builder.contextId(this.contextId);
        builder.context(this.context);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link GetResolvedConfigInput}.
     */
    public static final class Builder implements ShapeBuilder<GetResolvedConfigInput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String workspaceId;
        private String orgId;
        private List<String> prefix;
        private String version;
        private Boolean showReasoning;
        private MergeStrategy mergeStrategy;
        private String contextId;
        private Map<String, Document> context;

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
         * @return this builder.
         */
        public Builder prefix(List<String> prefix) {
            this.prefix = prefix;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder version(String version) {
            this.version = version;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder showReasoning(boolean showReasoning) {
            this.showReasoning = showReasoning;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder mergeStrategy(MergeStrategy mergeStrategy) {
            this.mergeStrategy = mergeStrategy;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder contextId(String contextId) {
            this.contextId = contextId;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder context(Map<String, Document> context) {
            this.context = context;
            return this;
        }

        @Override
        public GetResolvedConfigInput build() {
            tracker.validate();
            return new GetResolvedConfigInput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> workspaceId((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, value));
                case 1 -> orgId((String) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, value));
                case 2 -> prefix((List<String>) SchemaUtils.validateSameMember($SCHEMA_PREFIX, member, value));
                case 3 -> version((String) SchemaUtils.validateSameMember($SCHEMA_VERSION, member, value));
                case 4 -> showReasoning((boolean) SchemaUtils.validateSameMember($SCHEMA_SHOW_REASONING, member, value));
                case 5 -> mergeStrategy((MergeStrategy) SchemaUtils.validateSameMember($SCHEMA_MERGE_STRATEGY, member, value));
                case 6 -> contextId((String) SchemaUtils.validateSameMember($SCHEMA_CONTEXT_ID, member, value));
                case 7 -> context((Map<String, Document>) SchemaUtils.validateSameMember($SCHEMA_CONTEXT, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<GetResolvedConfigInput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_WORKSPACE_ID)) {
                workspaceId("");
            }
            if (!tracker.checkMember($SCHEMA_ORG_ID)) {
                orgId("");
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
                    case 2 -> builder.prefix(SharedSerde.deserializeStringList(member, de));
                    case 3 -> builder.version(de.readString(member));
                    case 4 -> builder.showReasoning(de.readBoolean(member));
                    case 5 -> builder.mergeStrategy(MergeStrategy.builder().deserializeMember(de, member).build());
                    case 6 -> builder.contextId(de.readString(member));
                    case 7 -> builder.context(SharedSerde.deserializeContextMap(member, de));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

