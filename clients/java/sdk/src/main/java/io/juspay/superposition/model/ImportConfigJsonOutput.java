
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
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

/**
 * Summary of what an import created, updated, skipped or deleted.
 */
@SmithyGenerated
public final class ImportConfigJsonOutput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ImportConfigOutput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("mode", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("dry_run", PreludeSchemas.BOOLEAN,
                new RequiredTrait())
        .putMember("config_version", PreludeSchemas.STRING)
        .putMember("dimensions", ImportEntityReport.$SCHEMA,
                new RequiredTrait())
        .putMember("default_configs", ImportEntityReport.$SCHEMA,
                new RequiredTrait())
        .putMember("contexts", ImportEntityReport.$SCHEMA,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_MODE = $SCHEMA.member("mode");
    private static final Schema $SCHEMA_DRY_RUN = $SCHEMA.member("dry_run");
    private static final Schema $SCHEMA_CONFIG_VERSION = $SCHEMA.member("config_version");
    private static final Schema $SCHEMA_DIMENSIONS = $SCHEMA.member("dimensions");
    private static final Schema $SCHEMA_DEFAULT_CONFIGS = $SCHEMA.member("default_configs");
    private static final Schema $SCHEMA_CONTEXTS = $SCHEMA.member("contexts");

    private final transient String mode;
    private final transient boolean dryRun;
    private final transient String configVersion;
    private final transient ImportEntityReport dimensions;
    private final transient ImportEntityReport defaultConfigs;
    private final transient ImportEntityReport contexts;

    private ImportConfigJsonOutput(Builder builder) {
        this.mode = builder.mode;
        this.dryRun = builder.dryRun;
        this.configVersion = builder.configVersion;
        this.dimensions = builder.dimensions;
        this.defaultConfigs = builder.defaultConfigs;
        this.contexts = builder.contexts;
    }

    public String mode() {
        return mode;
    }

    public boolean dryRun() {
        return dryRun;
    }

    public String configVersion() {
        return configVersion;
    }

    public ImportEntityReport dimensions() {
        return dimensions;
    }

    public ImportEntityReport defaultConfigs() {
        return defaultConfigs;
    }

    public ImportEntityReport contexts() {
        return contexts;
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
        ImportConfigJsonOutput that = (ImportConfigJsonOutput) other;
        return Objects.equals(this.mode, that.mode)
               && this.dryRun == that.dryRun
               && Objects.equals(this.configVersion, that.configVersion)
               && Objects.equals(this.dimensions, that.dimensions)
               && Objects.equals(this.defaultConfigs, that.defaultConfigs)
               && Objects.equals(this.contexts, that.contexts);
    }

    @Override
    public int hashCode() {
        return Objects.hash(mode, dryRun, configVersion, dimensions, defaultConfigs, contexts);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_MODE, mode);
        serializer.writeBoolean($SCHEMA_DRY_RUN, dryRun);
        if (configVersion != null) {
            serializer.writeString($SCHEMA_CONFIG_VERSION, configVersion);
        }
        if (dimensions != null) {
            serializer.writeStruct($SCHEMA_DIMENSIONS, dimensions);
        }
        if (defaultConfigs != null) {
            serializer.writeStruct($SCHEMA_DEFAULT_CONFIGS, defaultConfigs);
        }
        if (contexts != null) {
            serializer.writeStruct($SCHEMA_CONTEXTS, contexts);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_MODE, member, mode);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_DRY_RUN, member, dryRun);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_DIMENSIONS, member, dimensions);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_DEFAULT_CONFIGS, member, defaultConfigs);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONTEXTS, member, contexts);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONFIG_VERSION, member, configVersion);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link ImportConfigJsonOutput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.mode(this.mode);
        builder.dryRun(this.dryRun);
        builder.configVersion(this.configVersion);
        builder.dimensions(this.dimensions);
        builder.defaultConfigs(this.defaultConfigs);
        builder.contexts(this.contexts);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link ImportConfigJsonOutput}.
     */
    public static final class Builder implements ShapeBuilder<ImportConfigJsonOutput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String mode;
        private boolean dryRun;
        private String configVersion;
        private ImportEntityReport dimensions;
        private ImportEntityReport defaultConfigs;
        private ImportEntityReport contexts;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder mode(String mode) {
            this.mode = Objects.requireNonNull(mode, "mode cannot be null");
            tracker.setMember($SCHEMA_MODE);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder dryRun(boolean dryRun) {
            this.dryRun = dryRun;
            tracker.setMember($SCHEMA_DRY_RUN);
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder configVersion(String configVersion) {
            this.configVersion = configVersion;
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder dimensions(ImportEntityReport dimensions) {
            this.dimensions = Objects.requireNonNull(dimensions, "dimensions cannot be null");
            tracker.setMember($SCHEMA_DIMENSIONS);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder defaultConfigs(ImportEntityReport defaultConfigs) {
            this.defaultConfigs = Objects.requireNonNull(defaultConfigs, "defaultConfigs cannot be null");
            tracker.setMember($SCHEMA_DEFAULT_CONFIGS);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder contexts(ImportEntityReport contexts) {
            this.contexts = Objects.requireNonNull(contexts, "contexts cannot be null");
            tracker.setMember($SCHEMA_CONTEXTS);
            return this;
        }

        @Override
        public ImportConfigJsonOutput build() {
            tracker.validate();
            return new ImportConfigJsonOutput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> mode((String) SchemaUtils.validateSameMember($SCHEMA_MODE, member, value));
                case 1 -> dryRun((boolean) SchemaUtils.validateSameMember($SCHEMA_DRY_RUN, member, value));
                case 2 -> dimensions((ImportEntityReport) SchemaUtils.validateSameMember($SCHEMA_DIMENSIONS, member, value));
                case 3 -> defaultConfigs((ImportEntityReport) SchemaUtils.validateSameMember($SCHEMA_DEFAULT_CONFIGS, member, value));
                case 4 -> contexts((ImportEntityReport) SchemaUtils.validateSameMember($SCHEMA_CONTEXTS, member, value));
                case 5 -> configVersion((String) SchemaUtils.validateSameMember($SCHEMA_CONFIG_VERSION, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<ImportConfigJsonOutput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_MODE)) {
                mode("");
            }
            if (!tracker.checkMember($SCHEMA_DRY_RUN)) {
                tracker.setMember($SCHEMA_DRY_RUN);
            }
            if (!tracker.checkMember($SCHEMA_DIMENSIONS)) {
                tracker.setMember($SCHEMA_DIMENSIONS);
            }
            if (!tracker.checkMember($SCHEMA_DEFAULT_CONFIGS)) {
                tracker.setMember($SCHEMA_DEFAULT_CONFIGS);
            }
            if (!tracker.checkMember($SCHEMA_CONTEXTS)) {
                tracker.setMember($SCHEMA_CONTEXTS);
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
                    case 0 -> builder.mode(de.readString(member));
                    case 1 -> builder.dryRun(de.readBoolean(member));
                    case 2 -> builder.dimensions(ImportEntityReport.builder().deserializeMember(de, member).build());
                    case 3 -> builder.defaultConfigs(ImportEntityReport.builder().deserializeMember(de, member).build());
                    case 4 -> builder.contexts(ImportEntityReport.builder().deserializeMember(de, member).build());
                    case 5 -> builder.configVersion(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

