
package io.juspay.superposition.model;

import java.util.Collections;
import java.util.List;
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
 * Per-entity outcome counts for an import.
 */
@SmithyGenerated
public final class ImportEntityReport implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ImportEntityReport");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("created", PreludeSchemas.INTEGER,
                new RequiredTrait())
        .putMember("updated", PreludeSchemas.INTEGER,
                new RequiredTrait())
        .putMember("skipped", PreludeSchemas.INTEGER,
                new RequiredTrait())
        .putMember("deleted", PreludeSchemas.INTEGER,
                new RequiredTrait())
        .putMember("errors", SharedSchemas.IMPORT_ERROR_LIST)
        .build();

    private static final Schema $SCHEMA_CREATED = $SCHEMA.member("created");
    private static final Schema $SCHEMA_UPDATED = $SCHEMA.member("updated");
    private static final Schema $SCHEMA_SKIPPED = $SCHEMA.member("skipped");
    private static final Schema $SCHEMA_DELETED = $SCHEMA.member("deleted");
    private static final Schema $SCHEMA_ERRORS = $SCHEMA.member("errors");

    private final transient int created;
    private final transient int updated;
    private final transient int skipped;
    private final transient int deleted;
    private final transient List<ImportErrorItem> errors;

    private ImportEntityReport(Builder builder) {
        this.created = builder.created;
        this.updated = builder.updated;
        this.skipped = builder.skipped;
        this.deleted = builder.deleted;
        this.errors = builder.errors == null ? null : Collections.unmodifiableList(builder.errors);
    }

    public int created() {
        return created;
    }

    public int updated() {
        return updated;
    }

    public int skipped() {
        return skipped;
    }

    public int deleted() {
        return deleted;
    }

    public List<ImportErrorItem> errors() {
        if (errors == null) {
            return Collections.emptyList();
        }
        return errors;
    }

    public boolean hasErrors() {
        return errors != null;
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
        ImportEntityReport that = (ImportEntityReport) other;
        return this.created == that.created
               && this.updated == that.updated
               && this.skipped == that.skipped
               && this.deleted == that.deleted
               && Objects.equals(this.errors, that.errors);
    }

    @Override
    public int hashCode() {
        return Objects.hash(created, updated, skipped, deleted, errors);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeInteger($SCHEMA_CREATED, created);
        serializer.writeInteger($SCHEMA_UPDATED, updated);
        serializer.writeInteger($SCHEMA_SKIPPED, skipped);
        serializer.writeInteger($SCHEMA_DELETED, deleted);
        if (errors != null) {
            serializer.writeList($SCHEMA_ERRORS, errors, errors.size(), SharedSerde.ImportErrorListSerializer.INSTANCE);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_CREATED, member, created);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_UPDATED, member, updated);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_SKIPPED, member, skipped);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_DELETED, member, deleted);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_ERRORS, member, errors);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link ImportEntityReport}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.created(this.created);
        builder.updated(this.updated);
        builder.skipped(this.skipped);
        builder.deleted(this.deleted);
        builder.errors(this.errors);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link ImportEntityReport}.
     */
    public static final class Builder implements ShapeBuilder<ImportEntityReport> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private int created;
        private int updated;
        private int skipped;
        private int deleted;
        private List<ImportErrorItem> errors;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder created(int created) {
            this.created = created;
            tracker.setMember($SCHEMA_CREATED);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder updated(int updated) {
            this.updated = updated;
            tracker.setMember($SCHEMA_UPDATED);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder skipped(int skipped) {
            this.skipped = skipped;
            tracker.setMember($SCHEMA_SKIPPED);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder deleted(int deleted) {
            this.deleted = deleted;
            tracker.setMember($SCHEMA_DELETED);
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder errors(List<ImportErrorItem> errors) {
            this.errors = errors;
            return this;
        }

        @Override
        public ImportEntityReport build() {
            tracker.validate();
            return new ImportEntityReport(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> created((int) SchemaUtils.validateSameMember($SCHEMA_CREATED, member, value));
                case 1 -> updated((int) SchemaUtils.validateSameMember($SCHEMA_UPDATED, member, value));
                case 2 -> skipped((int) SchemaUtils.validateSameMember($SCHEMA_SKIPPED, member, value));
                case 3 -> deleted((int) SchemaUtils.validateSameMember($SCHEMA_DELETED, member, value));
                case 4 -> errors((List<ImportErrorItem>) SchemaUtils.validateSameMember($SCHEMA_ERRORS, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<ImportEntityReport> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_CREATED)) {
                tracker.setMember($SCHEMA_CREATED);
            }
            if (!tracker.checkMember($SCHEMA_UPDATED)) {
                tracker.setMember($SCHEMA_UPDATED);
            }
            if (!tracker.checkMember($SCHEMA_SKIPPED)) {
                tracker.setMember($SCHEMA_SKIPPED);
            }
            if (!tracker.checkMember($SCHEMA_DELETED)) {
                tracker.setMember($SCHEMA_DELETED);
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
                    case 0 -> builder.created(de.readInteger(member));
                    case 1 -> builder.updated(de.readInteger(member));
                    case 2 -> builder.skipped(de.readInteger(member));
                    case 3 -> builder.deleted(de.readInteger(member));
                    case 4 -> builder.errors(SharedSerde.deserializeImportErrorList(member, de));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

