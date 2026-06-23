
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
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.HttpHeaderTrait;
import software.amazon.smithy.model.traits.HttpPayloadTrait;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class ImportConfigJsonInput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ImportConfigJsonInput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("workspace_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-workspace"),
                new RequiredTrait())
        .putMember("org_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-org-id"),
                new RequiredTrait())
        .putMember("strategy", ImportStrategy.$SCHEMA,
                new HttpHeaderTrait("x-import-strategy"))
        .putMember("on_error", ImportOnError.$SCHEMA,
                new HttpHeaderTrait("x-import-on-error"))
        .putMember("dry_run", PreludeSchemas.BOOLEAN,
                new HttpHeaderTrait("x-import-dry-run"))
        .putMember("config_tags", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-config-tags"))
        .putMember("json_config", PreludeSchemas.STRING,
                new RequiredTrait(),
                new HttpPayloadTrait())
        .build();

    private static final Schema $SCHEMA_WORKSPACE_ID = $SCHEMA.member("workspace_id");
    private static final Schema $SCHEMA_ORG_ID = $SCHEMA.member("org_id");
    private static final Schema $SCHEMA_STRATEGY = $SCHEMA.member("strategy");
    private static final Schema $SCHEMA_ON_ERROR = $SCHEMA.member("on_error");
    private static final Schema $SCHEMA_DRY_RUN = $SCHEMA.member("dry_run");
    private static final Schema $SCHEMA_CONFIG_TAGS = $SCHEMA.member("config_tags");
    private static final Schema $SCHEMA_JSON_CONFIG = $SCHEMA.member("json_config");

    private final transient String workspaceId;
    private final transient String orgId;
    private final transient ImportStrategy strategy;
    private final transient ImportOnError onError;
    private final transient Boolean dryRun;
    private final transient String configTags;
    private final transient String jsonConfig;

    private ImportConfigJsonInput(Builder builder) {
        this.workspaceId = builder.workspaceId;
        this.orgId = builder.orgId;
        this.strategy = builder.strategy;
        this.onError = builder.onError;
        this.dryRun = builder.dryRun;
        this.configTags = builder.configTags;
        this.jsonConfig = builder.jsonConfig;
    }

    public String workspaceId() {
        return workspaceId;
    }

    public String orgId() {
        return orgId;
    }

    /**
     * How the import applies file entities to the workspace. Defaults to upsert.
     */
    public ImportStrategy strategy() {
        return strategy;
    }

    /**
     * Whether to abort (default) or continue on per-entity errors.
     */
    public ImportOnError onError() {
        return onError;
    }

    /**
     * When true, validates and summarises the import without persisting anything. Defaults to false.
     */
    public Boolean dryRun() {
        return dryRun;
    }

    public String configTags() {
        return configTags;
    }

    public String jsonConfig() {
        return jsonConfig;
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
        ImportConfigJsonInput that = (ImportConfigJsonInput) other;
        return Objects.equals(this.workspaceId, that.workspaceId)
               && Objects.equals(this.orgId, that.orgId)
               && Objects.equals(this.strategy, that.strategy)
               && Objects.equals(this.onError, that.onError)
               && Objects.equals(this.dryRun, that.dryRun)
               && Objects.equals(this.configTags, that.configTags)
               && Objects.equals(this.jsonConfig, that.jsonConfig);
    }

    @Override
    public int hashCode() {
        return Objects.hash(workspaceId, orgId, strategy, onError, dryRun, configTags, jsonConfig);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_WORKSPACE_ID, workspaceId);
        serializer.writeString($SCHEMA_ORG_ID, orgId);
        if (strategy != null) {
            serializer.writeString($SCHEMA_STRATEGY, strategy.value());
        }
        if (onError != null) {
            serializer.writeString($SCHEMA_ON_ERROR, onError.value());
        }
        if (dryRun != null) {
            serializer.writeBoolean($SCHEMA_DRY_RUN, dryRun);
        }
        if (configTags != null) {
            serializer.writeString($SCHEMA_CONFIG_TAGS, configTags);
        }
        serializer.writeString($SCHEMA_JSON_CONFIG, jsonConfig);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, workspaceId);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, orgId);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_JSON_CONFIG, member, jsonConfig);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_STRATEGY, member, strategy);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_ON_ERROR, member, onError);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_DRY_RUN, member, dryRun);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONFIG_TAGS, member, configTags);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link ImportConfigJsonInput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.workspaceId(this.workspaceId);
        builder.orgId(this.orgId);
        builder.strategy(this.strategy);
        builder.onError(this.onError);
        builder.dryRun(this.dryRun);
        builder.configTags(this.configTags);
        builder.jsonConfig(this.jsonConfig);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link ImportConfigJsonInput}.
     */
    public static final class Builder implements ShapeBuilder<ImportConfigJsonInput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String workspaceId;
        private String orgId;
        private ImportStrategy strategy;
        private ImportOnError onError;
        private Boolean dryRun;
        private String configTags;
        private String jsonConfig;

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
         * How the import applies file entities to the workspace. Defaults to upsert.
         *
         * @return this builder.
         */
        public Builder strategy(ImportStrategy strategy) {
            this.strategy = strategy;
            return this;
        }

        /**
         * Whether to abort (default) or continue on per-entity errors.
         *
         * @return this builder.
         */
        public Builder onError(ImportOnError onError) {
            this.onError = onError;
            return this;
        }

        /**
         * When true, validates and summarises the import without persisting anything. Defaults to false.
         *
         * @return this builder.
         */
        public Builder dryRun(boolean dryRun) {
            this.dryRun = dryRun;
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
        public Builder jsonConfig(String jsonConfig) {
            this.jsonConfig = Objects.requireNonNull(jsonConfig, "jsonConfig cannot be null");
            tracker.setMember($SCHEMA_JSON_CONFIG);
            return this;
        }

        @Override
        public ImportConfigJsonInput build() {
            tracker.validate();
            return new ImportConfigJsonInput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> workspaceId((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, value));
                case 1 -> orgId((String) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, value));
                case 2 -> jsonConfig((String) SchemaUtils.validateSameMember($SCHEMA_JSON_CONFIG, member, value));
                case 3 -> strategy((ImportStrategy) SchemaUtils.validateSameMember($SCHEMA_STRATEGY, member, value));
                case 4 -> onError((ImportOnError) SchemaUtils.validateSameMember($SCHEMA_ON_ERROR, member, value));
                case 5 -> dryRun((boolean) SchemaUtils.validateSameMember($SCHEMA_DRY_RUN, member, value));
                case 6 -> configTags((String) SchemaUtils.validateSameMember($SCHEMA_CONFIG_TAGS, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<ImportConfigJsonInput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_WORKSPACE_ID)) {
                workspaceId("");
            }
            if (!tracker.checkMember($SCHEMA_ORG_ID)) {
                orgId("");
            }
            if (!tracker.checkMember($SCHEMA_JSON_CONFIG)) {
                jsonConfig("");
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
                    case 2 -> builder.jsonConfig(de.readString(member));
                    case 3 -> builder.strategy(ImportStrategy.builder().deserializeMember(de, member).build());
                    case 4 -> builder.onError(ImportOnError.builder().deserializeMember(de, member).build());
                    case 5 -> builder.dryRun(de.readBoolean(member));
                    case 6 -> builder.configTags(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

