
package io.juspay.superposition.model;

import java.time.Instant;
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
import software.amazon.smithy.model.traits.HttpHeaderTrait;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class ListExperimentOutput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ListExperimentOutput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("total_pages", PreludeSchemas.INTEGER,
                new RequiredTrait())
        .putMember("total_items", PreludeSchemas.INTEGER,
                new RequiredTrait())
        .putMember("data", SharedSchemas.EXPERIMENT_LIST,
                new RequiredTrait())
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> c1293812 (Test)
        .putMember("last_modified", SharedSchemas.DATE_TIME,
=======
        .putMember("last_modified_at", SharedSchemas.DATE_TIME,
>>>>>>> 6e8749e1 (Test)
                new HttpHeaderTrait("last-modified"),
                new RequiredTrait())
=======
<<<<<<< HEAD
=======
        .putMember("last_modified", SharedSchemas.DATE_TIME,
                new HttpHeaderTrait("last-modified"),
                new RequiredTrait())
>>>>>>> 8fc501b7 (fix: more fixes)
>>>>>>> 91d47048 (fix: more fixes)
=======
        .putMember("last_modified", SharedSchemas.DATE_TIME,
                new HttpHeaderTrait("last-modified"),
                new RequiredTrait())
>>>>>>> 588a53c4 (feat: Add prefix filter in list exp)
        .build();

    private static final Schema $SCHEMA_TOTAL_PAGES = $SCHEMA.member("total_pages");
    private static final Schema $SCHEMA_TOTAL_ITEMS = $SCHEMA.member("total_items");
    private static final Schema $SCHEMA_DATA = $SCHEMA.member("data");
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
    private static final Schema $SCHEMA_LAST_MODIFIED = $SCHEMA.member("last_modified");
=======
<<<<<<< HEAD
=======
    private static final Schema $SCHEMA_LAST_MODIFIED = $SCHEMA.member("last_modified");
>>>>>>> 8fc501b7 (fix: more fixes)
>>>>>>> 91d47048 (fix: more fixes)
=======
    private static final Schema $SCHEMA_LAST_MODIFIED = $SCHEMA.member("last_modified");
>>>>>>> 588a53c4 (feat: Add prefix filter in list exp)
=======
    private static final Schema $SCHEMA_LAST_MODIFIED = $SCHEMA.member("last_modified");
=======
    private static final Schema $SCHEMA_LAST_MODIFIED_AT = $SCHEMA.member("last_modified_at");
>>>>>>> 6e8749e1 (Test)
>>>>>>> c1293812 (Test)

    private final transient int totalPages;
    private final transient int totalItems;
    private final transient List<ExperimentResponse> data;
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
    private final transient Instant lastModified;
=======
<<<<<<< HEAD
=======
    private final transient Instant lastModified;
>>>>>>> 8fc501b7 (fix: more fixes)
>>>>>>> 91d47048 (fix: more fixes)
=======
    private final transient Instant lastModified;
>>>>>>> 588a53c4 (feat: Add prefix filter in list exp)
=======
    private final transient Instant lastModified;
=======
    private final transient Instant lastModifiedAt;
>>>>>>> 6e8749e1 (Test)
>>>>>>> c1293812 (Test)

    private ListExperimentOutput(Builder builder) {
        this.totalPages = builder.totalPages;
        this.totalItems = builder.totalItems;
        this.data = Collections.unmodifiableList(builder.data);
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
        this.lastModified = builder.lastModified;
=======
<<<<<<< HEAD
=======
        this.lastModified = builder.lastModified;
>>>>>>> 8fc501b7 (fix: more fixes)
>>>>>>> 91d47048 (fix: more fixes)
=======
        this.lastModified = builder.lastModified;
>>>>>>> 588a53c4 (feat: Add prefix filter in list exp)
=======
        this.lastModified = builder.lastModified;
=======
        this.lastModifiedAt = builder.lastModifiedAt;
>>>>>>> 6e8749e1 (Test)
>>>>>>> c1293812 (Test)
    }

    public int totalPages() {
        return totalPages;
    }

    public int totalItems() {
        return totalItems;
    }

    public List<ExperimentResponse> data() {
        return data;
    }

    public boolean hasData() {
        return true;
    }

<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
<<<<<<< HEAD
=======
>>>>>>> 91d47048 (fix: more fixes)
=======
>>>>>>> 588a53c4 (feat: Add prefix filter in list exp)
=======
>>>>>>> c1293812 (Test)
    public Instant lastModified() {
        return lastModified;
=======
    public Instant lastModifiedAt() {
        return lastModifiedAt;
>>>>>>> 6e8749e1 (Test)
    }

<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 8fc501b7 (fix: more fixes)
>>>>>>> 91d47048 (fix: more fixes)
=======
>>>>>>> 588a53c4 (feat: Add prefix filter in list exp)
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
        ListExperimentOutput that = (ListExperimentOutput) other;
        return this.totalPages == that.totalPages
               && this.totalItems == that.totalItems
<<<<<<< HEAD
<<<<<<< HEAD
               && Objects.equals(this.data, that.data)
<<<<<<< HEAD
               && Objects.equals(this.lastModified, that.lastModified);
=======
<<<<<<< HEAD
<<<<<<< HEAD
               && Objects.equals(this.data, that.data);
=======
               && Objects.equals(this.data, that.data)
               && Objects.equals(this.lastModified, that.lastModified);
>>>>>>> 8fc501b7 (fix: more fixes)
>>>>>>> 91d47048 (fix: more fixes)
=======
               && Objects.equals(this.data, that.data)
               && Objects.equals(this.lastModified, that.lastModified);
>>>>>>> 588a53c4 (feat: Add prefix filter in list exp)
=======
               && Objects.equals(this.lastModifiedAt, that.lastModifiedAt);
>>>>>>> 6e8749e1 (Test)
>>>>>>> c1293812 (Test)
    }

    @Override
    public int hashCode() {
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
        return Objects.hash(totalPages, totalItems, data, lastModified);
=======
<<<<<<< HEAD
        return Objects.hash(totalPages, totalItems, data);
=======
        return Objects.hash(totalPages, totalItems, data, lastModified);
>>>>>>> 8fc501b7 (fix: more fixes)
>>>>>>> 91d47048 (fix: more fixes)
=======
        return Objects.hash(totalPages, totalItems, data, lastModified);
>>>>>>> 588a53c4 (feat: Add prefix filter in list exp)
=======
        return Objects.hash(totalPages, totalItems, data, lastModified);
=======
        return Objects.hash(totalPages, totalItems, data, lastModifiedAt);
>>>>>>> 6e8749e1 (Test)
>>>>>>> c1293812 (Test)
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeInteger($SCHEMA_TOTAL_PAGES, totalPages);
        serializer.writeInteger($SCHEMA_TOTAL_ITEMS, totalItems);
        serializer.writeList($SCHEMA_DATA, data, data.size(), SharedSerde.ExperimentListSerializer.INSTANCE);
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
        serializer.writeTimestamp($SCHEMA_LAST_MODIFIED, lastModified);
=======
<<<<<<< HEAD
=======
        serializer.writeTimestamp($SCHEMA_LAST_MODIFIED, lastModified);
>>>>>>> 8fc501b7 (fix: more fixes)
>>>>>>> 91d47048 (fix: more fixes)
=======
        serializer.writeTimestamp($SCHEMA_LAST_MODIFIED, lastModified);
>>>>>>> 588a53c4 (feat: Add prefix filter in list exp)
=======
        serializer.writeTimestamp($SCHEMA_LAST_MODIFIED, lastModified);
=======
        serializer.writeTimestamp($SCHEMA_LAST_MODIFIED_AT, lastModifiedAt);
>>>>>>> 6e8749e1 (Test)
>>>>>>> c1293812 (Test)
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_TOTAL_PAGES, member, totalPages);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_TOTAL_ITEMS, member, totalItems);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_DATA, member, data);
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED, member, lastModified);
=======
<<<<<<< HEAD
=======
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED, member, lastModified);
>>>>>>> 8fc501b7 (fix: more fixes)
>>>>>>> 91d47048 (fix: more fixes)
=======
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED, member, lastModified);
>>>>>>> 588a53c4 (feat: Add prefix filter in list exp)
=======
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED, member, lastModified);
=======
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_AT, member, lastModifiedAt);
>>>>>>> 6e8749e1 (Test)
>>>>>>> c1293812 (Test)
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link ListExperimentOutput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.totalPages(this.totalPages);
        builder.totalItems(this.totalItems);
        builder.data(this.data);
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
        builder.lastModified(this.lastModified);
=======
<<<<<<< HEAD
=======
        builder.lastModified(this.lastModified);
>>>>>>> 8fc501b7 (fix: more fixes)
>>>>>>> 91d47048 (fix: more fixes)
=======
        builder.lastModified(this.lastModified);
>>>>>>> 588a53c4 (feat: Add prefix filter in list exp)
=======
        builder.lastModified(this.lastModified);
=======
        builder.lastModifiedAt(this.lastModifiedAt);
>>>>>>> 6e8749e1 (Test)
>>>>>>> c1293812 (Test)
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link ListExperimentOutput}.
     */
    public static final class Builder implements ShapeBuilder<ListExperimentOutput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private int totalPages;
        private int totalItems;
        private List<ExperimentResponse> data;
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
        private Instant lastModified;
=======
<<<<<<< HEAD
=======
        private Instant lastModified;
>>>>>>> 8fc501b7 (fix: more fixes)
>>>>>>> 91d47048 (fix: more fixes)
=======
        private Instant lastModified;
>>>>>>> 588a53c4 (feat: Add prefix filter in list exp)
=======
        private Instant lastModified;
=======
        private Instant lastModifiedAt;
>>>>>>> 6e8749e1 (Test)
>>>>>>> c1293812 (Test)

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder totalPages(int totalPages) {
            this.totalPages = totalPages;
            tracker.setMember($SCHEMA_TOTAL_PAGES);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder totalItems(int totalItems) {
            this.totalItems = totalItems;
            tracker.setMember($SCHEMA_TOTAL_ITEMS);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder data(List<ExperimentResponse> data) {
            this.data = Objects.requireNonNull(data, "data cannot be null");
            tracker.setMember($SCHEMA_DATA);
            return this;
        }

<<<<<<< HEAD
<<<<<<< HEAD
=======
<<<<<<< HEAD
=======
>>>>>>> 91d47048 (fix: more fixes)
=======
>>>>>>> 588a53c4 (feat: Add prefix filter in list exp)
        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
<<<<<<< HEAD
        public Builder lastModified(Instant lastModified) {
            this.lastModified = Objects.requireNonNull(lastModified, "lastModified cannot be null");
            tracker.setMember($SCHEMA_LAST_MODIFIED);
=======
        public Builder lastModifiedAt(Instant lastModifiedAt) {
            this.lastModifiedAt = Objects.requireNonNull(lastModifiedAt, "lastModifiedAt cannot be null");
            tracker.setMember($SCHEMA_LAST_MODIFIED_AT);
>>>>>>> 6e8749e1 (Test)
            return this;
        }

<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 8fc501b7 (fix: more fixes)
>>>>>>> 91d47048 (fix: more fixes)
=======
>>>>>>> 588a53c4 (feat: Add prefix filter in list exp)
        @Override
        public ListExperimentOutput build() {
            tracker.validate();
            return new ListExperimentOutput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> totalPages((int) SchemaUtils.validateSameMember($SCHEMA_TOTAL_PAGES, member, value));
                case 1 -> totalItems((int) SchemaUtils.validateSameMember($SCHEMA_TOTAL_ITEMS, member, value));
                case 2 -> data((List<ExperimentResponse>) SchemaUtils.validateSameMember($SCHEMA_DATA, member, value));
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
                case 3 -> lastModified((Instant) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED, member, value));
=======
<<<<<<< HEAD
=======
                case 3 -> lastModified((Instant) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED, member, value));
>>>>>>> 8fc501b7 (fix: more fixes)
>>>>>>> 91d47048 (fix: more fixes)
=======
                case 3 -> lastModified((Instant) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED, member, value));
>>>>>>> 588a53c4 (feat: Add prefix filter in list exp)
=======
                case 3 -> lastModified((Instant) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED, member, value));
=======
                case 3 -> lastModifiedAt((Instant) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_AT, member, value));
>>>>>>> 6e8749e1 (Test)
>>>>>>> c1293812 (Test)
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<ListExperimentOutput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_TOTAL_PAGES)) {
                tracker.setMember($SCHEMA_TOTAL_PAGES);
            }
            if (!tracker.checkMember($SCHEMA_TOTAL_ITEMS)) {
                tracker.setMember($SCHEMA_TOTAL_ITEMS);
            }
            if (!tracker.checkMember($SCHEMA_DATA)) {
                data(Collections.emptyList());
            }
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> c1293812 (Test)
            if (!tracker.checkMember($SCHEMA_LAST_MODIFIED)) {
                lastModified(Instant.EPOCH);
=======
            if (!tracker.checkMember($SCHEMA_LAST_MODIFIED_AT)) {
                lastModifiedAt(Instant.EPOCH);
>>>>>>> 6e8749e1 (Test)
            }
=======
<<<<<<< HEAD
=======
            if (!tracker.checkMember($SCHEMA_LAST_MODIFIED)) {
                lastModified(Instant.EPOCH);
            }
>>>>>>> 8fc501b7 (fix: more fixes)
>>>>>>> 91d47048 (fix: more fixes)
=======
            if (!tracker.checkMember($SCHEMA_LAST_MODIFIED)) {
                lastModified(Instant.EPOCH);
            }
>>>>>>> 588a53c4 (feat: Add prefix filter in list exp)
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
                    case 0 -> builder.totalPages(de.readInteger(member));
                    case 1 -> builder.totalItems(de.readInteger(member));
                    case 2 -> builder.data(SharedSerde.deserializeExperimentList(member, de));
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
                    case 3 -> builder.lastModified(de.readTimestamp(member));
=======
<<<<<<< HEAD
=======
                    case 3 -> builder.lastModified(de.readTimestamp(member));
>>>>>>> 8fc501b7 (fix: more fixes)
>>>>>>> 91d47048 (fix: more fixes)
=======
                    case 3 -> builder.lastModified(de.readTimestamp(member));
>>>>>>> 588a53c4 (feat: Add prefix filter in list exp)
=======
                    case 3 -> builder.lastModified(de.readTimestamp(member));
=======
                    case 3 -> builder.lastModifiedAt(de.readTimestamp(member));
>>>>>>> 6e8749e1 (Test)
>>>>>>> c1293812 (Test)
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

