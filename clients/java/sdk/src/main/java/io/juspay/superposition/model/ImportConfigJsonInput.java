
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
        .putMember("mode", ImportMode.$SCHEMA,
                new HttpHeaderTrait("x-import-mode"))
        .putMember("overwrite", PreludeSchemas.BOOLEAN,
                new HttpHeaderTrait("x-import-overwrite"))
        .putMember("on_error", ImportOnError.$SCHEMA,
                new HttpHeaderTrait("x-import-on-error"))
        .putMember("dry_run", PreludeSchemas.BOOLEAN,
                new HttpHeaderTrait("x-import-dry-run"))
        .putMember("value_merge", PreludeSchemas.BOOLEAN,
                new HttpHeaderTrait("x-import-value-merge"))
        .putMember("config_tags", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-config-tags"))
        .putMember("json_config", PreludeSchemas.STRING,
                new RequiredTrait(),
                new HttpPayloadTrait())
        .build();

    private static final Schema $SCHEMA_WORKSPACE_ID = $SCHEMA.member("workspace_id");
    private static final Schema $SCHEMA_ORG_ID = $SCHEMA.member("org_id");
    private static final Schema $SCHEMA_MODE = $SCHEMA.member("mode");
    private static final Schema $SCHEMA_OVERWRITE = $SCHEMA.member("overwrite");
    private static final Schema $SCHEMA_ON_ERROR = $SCHEMA.member("on_error");
    private static final Schema $SCHEMA_DRY_RUN = $SCHEMA.member("dry_run");
    private static final Schema $SCHEMA_VALUE_MERGE = $SCHEMA.member("value_merge");
    private static final Schema $SCHEMA_CONFIG_TAGS = $SCHEMA.member("config_tags");
    private static final Schema $SCHEMA_JSON_CONFIG = $SCHEMA.member("json_config");

    private final transient String workspaceId;
    private final transient String orgId;
    private final transient ImportMode mode;
    private final transient Boolean overwrite;
    private final transient ImportOnError onError;
    private final transient Boolean dryRun;
    private final transient Boolean valueMerge;
    private final transient String configTags;
    private final transient String jsonConfig;

    private ImportConfigJsonInput(Builder builder) {
        this.workspaceId = builder.workspaceId;
        this.orgId = builder.orgId;
        this.mode = builder.mode;
        this.overwrite = builder.overwrite;
        this.onError = builder.onError;
        this.dryRun = builder.dryRun;
        this.valueMerge = builder.valueMerge;
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
     * Whether to merge (default) or replace existing workspace config.
     */
    public ImportMode mode() {
        return mode;
    }

    /**
     * When false, entities that already exist are skipped instead of updated. Defaults to true.
     */
    public Boolean overwrite() {
        return overwrite;
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

    /**
     * When true, deep-merges object-valued default-configs with the existing value. Defaults to false.
     */
    public Boolean valueMerge() {
        return valueMerge;
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
               && Objects.equals(this.mode, that.mode)
               && Objects.equals(this.overwrite, that.overwrite)
               && Objects.equals(this.onError, that.onError)
               && Objects.equals(this.dryRun, that.dryRun)
               && Objects.equals(this.valueMerge, that.valueMerge)
               && Objects.equals(this.configTags, that.configTags)
               && Objects.equals(this.jsonConfig, that.jsonConfig);
    }

    @Override
    public int hashCode() {
        return Objects.hash(workspaceId, orgId, mode, overwrite, onError, dryRun, valueMerge, configTags, jsonConfig);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_WORKSPACE_ID, workspaceId);
        serializer.writeString($SCHEMA_ORG_ID, orgId);
        if (mode != null) {
            serializer.writeString($SCHEMA_MODE, mode.value());
        }
        if (overwrite != null) {
            serializer.writeBoolean($SCHEMA_OVERWRITE, overwrite);
        }
        if (onError != null) {
            serializer.writeString($SCHEMA_ON_ERROR, onError.value());
        }
        if (dryRun != null) {
            serializer.writeBoolean($SCHEMA_DRY_RUN, dryRun);
        }
        if (valueMerge != null) {
            serializer.writeBoolean($SCHEMA_VALUE_MERGE, valueMerge);
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
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_MODE, member, mode);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_OVERWRITE, member, overwrite);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_ON_ERROR, member, onError);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_DRY_RUN, member, dryRun);
            case 7 -> (T) SchemaUtils.validateSameMember($SCHEMA_VALUE_MERGE, member, valueMerge);
            case 8 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONFIG_TAGS, member, configTags);
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
        builder.mode(this.mode);
        builder.overwrite(this.overwrite);
        builder.onError(this.onError);
        builder.dryRun(this.dryRun);
        builder.valueMerge(this.valueMerge);
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
        private ImportMode mode;
        private Boolean overwrite;
        private ImportOnError onError;
        private Boolean dryRun;
        private Boolean valueMerge;
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
         * Whether to merge (default) or replace existing workspace config.
         *
         * @return this builder.
         */
        public Builder mode(ImportMode mode) {
            this.mode = mode;
            return this;
        }

        /**
         * When false, entities that already exist are skipped instead of updated. Defaults to true.
         *
         * @return this builder.
         */
        public Builder overwrite(boolean overwrite) {
            this.overwrite = overwrite;
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
         * When true, deep-merges object-valued default-configs with the existing value. Defaults to false.
         *
         * @return this builder.
         */
        public Builder valueMerge(boolean valueMerge) {
            this.valueMerge = valueMerge;
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
                case 3 -> mode((ImportMode) SchemaUtils.validateSameMember($SCHEMA_MODE, member, value));
                case 4 -> overwrite((boolean) SchemaUtils.validateSameMember($SCHEMA_OVERWRITE, member, value));
                case 5 -> onError((ImportOnError) SchemaUtils.validateSameMember($SCHEMA_ON_ERROR, member, value));
                case 6 -> dryRun((boolean) SchemaUtils.validateSameMember($SCHEMA_DRY_RUN, member, value));
                case 7 -> valueMerge((boolean) SchemaUtils.validateSameMember($SCHEMA_VALUE_MERGE, member, value));
                case 8 -> configTags((String) SchemaUtils.validateSameMember($SCHEMA_CONFIG_TAGS, member, value));
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
                    case 3 -> builder.mode(ImportMode.builder().deserializeMember(de, member).build());
                    case 4 -> builder.overwrite(de.readBoolean(member));
                    case 5 -> builder.onError(ImportOnError.builder().deserializeMember(de, member).build());
                    case 6 -> builder.dryRun(de.readBoolean(member));
                    case 7 -> builder.valueMerge(de.readBoolean(member));
                    case 8 -> builder.configTags(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

