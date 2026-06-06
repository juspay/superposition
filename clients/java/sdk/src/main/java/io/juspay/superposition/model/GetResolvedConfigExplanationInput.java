
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
import software.amazon.smithy.model.traits.HttpHeaderTrait;
import software.amazon.smithy.model.traits.HttpLabelTrait;
import software.amazon.smithy.model.traits.HttpQueryTrait;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class GetResolvedConfigExplanationInput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#GetResolvedConfigExplanationInput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("workspace_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-workspace"),
                new RequiredTrait())
        .putMember("org_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-org-id"),
                new RequiredTrait())
        .putMember("key", PreludeSchemas.STRING,
                new RequiredTrait(),
                new HttpLabelTrait())
        .putMember("version", PreludeSchemas.STRING,
                new HttpQueryTrait("version"))
        .putMember("merge_strategy", MergeStrategy.$SCHEMA,
                new HttpHeaderTrait("x-merge-strategy"))
        .putMember("context_id", PreludeSchemas.STRING,
                new HttpQueryTrait("context_id"))
        .putMember("resolve_remote", PreludeSchemas.BOOLEAN,
                new HttpQueryTrait("resolve_remote"))
        .putMember("context", SharedSchemas.CONTEXT_MAP)
        .build();

    private static final Schema $SCHEMA_WORKSPACE_ID = $SCHEMA.member("workspace_id");
    private static final Schema $SCHEMA_ORG_ID = $SCHEMA.member("org_id");
    private static final Schema $SCHEMA_KEY = $SCHEMA.member("key");
    private static final Schema $SCHEMA_VERSION = $SCHEMA.member("version");
    private static final Schema $SCHEMA_MERGE_STRATEGY = $SCHEMA.member("merge_strategy");
    private static final Schema $SCHEMA_CONTEXT_ID = $SCHEMA.member("context_id");
    private static final Schema $SCHEMA_RESOLVE_REMOTE = $SCHEMA.member("resolve_remote");
    private static final Schema $SCHEMA_CONTEXT = $SCHEMA.member("context");

    private final transient String workspaceId;
    private final transient String orgId;
    private final transient String key;
    private final transient String version;
    private final transient MergeStrategy mergeStrategy;
    private final transient String contextId;
    private final transient Boolean resolveRemote;
    private final transient Map<String, Document> context;

    private GetResolvedConfigExplanationInput(Builder builder) {
        this.workspaceId = builder.workspaceId;
        this.orgId = builder.orgId;
        this.key = builder.key;
        this.version = builder.version;
        this.mergeStrategy = builder.mergeStrategy;
        this.contextId = builder.contextId;
        this.resolveRemote = builder.resolveRemote;
        this.context = builder.context == null ? null : Collections.unmodifiableMap(builder.context);
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

    public String version() {
        return version;
    }

    public MergeStrategy mergeStrategy() {
        return mergeStrategy;
    }

    public String contextId() {
        return contextId;
    }

    /**
     * Intended for control resolution. If true, evaluates and includes remote cohort-based contexts during
     * config resolution.
     */
    public Boolean resolveRemote() {
        return resolveRemote;
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
        GetResolvedConfigExplanationInput that = (GetResolvedConfigExplanationInput) other;
        return Objects.equals(this.workspaceId, that.workspaceId)
               && Objects.equals(this.orgId, that.orgId)
               && Objects.equals(this.key, that.key)
               && Objects.equals(this.version, that.version)
               && Objects.equals(this.mergeStrategy, that.mergeStrategy)
               && Objects.equals(this.contextId, that.contextId)
               && Objects.equals(this.resolveRemote, that.resolveRemote)
               && Objects.equals(this.context, that.context);
    }

    @Override
    public int hashCode() {
        return Objects.hash(workspaceId, orgId, key, version, mergeStrategy, contextId, resolveRemote, context);
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
        if (version != null) {
            serializer.writeString($SCHEMA_VERSION, version);
        }
        if (mergeStrategy != null) {
            serializer.writeString($SCHEMA_MERGE_STRATEGY, mergeStrategy.value());
        }
        if (contextId != null) {
            serializer.writeString($SCHEMA_CONTEXT_ID, contextId);
        }
        if (resolveRemote != null) {
            serializer.writeBoolean($SCHEMA_RESOLVE_REMOTE, resolveRemote);
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
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_KEY, member, key);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_VERSION, member, version);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_MERGE_STRATEGY, member, mergeStrategy);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONTEXT_ID, member, contextId);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_RESOLVE_REMOTE, member, resolveRemote);
            case 7 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONTEXT, member, context);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link GetResolvedConfigExplanationInput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.workspaceId(this.workspaceId);
        builder.orgId(this.orgId);
        builder.key(this.key);
        builder.version(this.version);
        builder.mergeStrategy(this.mergeStrategy);
        builder.contextId(this.contextId);
        builder.resolveRemote(this.resolveRemote);
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
     * Builder for {@link GetResolvedConfigExplanationInput}.
     */
    public static final class Builder implements ShapeBuilder<GetResolvedConfigExplanationInput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String workspaceId;
        private String orgId;
        private String key;
        private String version;
        private MergeStrategy mergeStrategy;
        private String contextId;
        private Boolean resolveRemote;
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
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder key(String key) {
            this.key = Objects.requireNonNull(key, "key cannot be null");
            tracker.setMember($SCHEMA_KEY);
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
         * Intended for control resolution. If true, evaluates and includes remote cohort-based contexts during
         * config resolution.
         *
         * @return this builder.
         */
        public Builder resolveRemote(boolean resolveRemote) {
            this.resolveRemote = resolveRemote;
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
        public GetResolvedConfigExplanationInput build() {
            tracker.validate();
            return new GetResolvedConfigExplanationInput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> workspaceId((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, value));
                case 1 -> orgId((String) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, value));
                case 2 -> key((String) SchemaUtils.validateSameMember($SCHEMA_KEY, member, value));
                case 3 -> version((String) SchemaUtils.validateSameMember($SCHEMA_VERSION, member, value));
                case 4 -> mergeStrategy((MergeStrategy) SchemaUtils.validateSameMember($SCHEMA_MERGE_STRATEGY, member, value));
                case 5 -> contextId((String) SchemaUtils.validateSameMember($SCHEMA_CONTEXT_ID, member, value));
                case 6 -> resolveRemote((boolean) SchemaUtils.validateSameMember($SCHEMA_RESOLVE_REMOTE, member, value));
                case 7 -> context((Map<String, Document>) SchemaUtils.validateSameMember($SCHEMA_CONTEXT, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<GetResolvedConfigExplanationInput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_WORKSPACE_ID)) {
                workspaceId("");
            }
            if (!tracker.checkMember($SCHEMA_ORG_ID)) {
                orgId("");
            }
            if (!tracker.checkMember($SCHEMA_KEY)) {
                key("");
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
                    case 2 -> builder.key(de.readString(member));
                    case 3 -> builder.version(de.readString(member));
                    case 4 -> builder.mergeStrategy(MergeStrategy.builder().deserializeMember(de, member).build());
                    case 5 -> builder.contextId(de.readString(member));
                    case 6 -> builder.resolveRemote(de.readBoolean(member));
                    case 7 -> builder.context(SharedSerde.deserializeContextMap(member, de));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

