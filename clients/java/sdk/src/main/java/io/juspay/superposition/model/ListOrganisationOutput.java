
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

@SmithyGenerated
public final class ListOrganisationOutput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ListOrganisationOutput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("total_pages", PreludeSchemas.INTEGER,
                new RequiredTrait())
        .putMember("total_items", PreludeSchemas.INTEGER,
                new RequiredTrait())
        .putMember("data", SharedSchemas.ORGANISATION_LIST,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_TOTAL_PAGES = $SCHEMA.member("total_pages");
    private static final Schema $SCHEMA_TOTAL_ITEMS = $SCHEMA.member("total_items");
    private static final Schema $SCHEMA_DATA = $SCHEMA.member("data");

    private final transient int totalPages;
    private final transient int totalItems;
    private final transient List<OrganisationResponse> data;

    private ListOrganisationOutput(Builder builder) {
        this.totalPages = builder.totalPages;
        this.totalItems = builder.totalItems;
        this.data = Collections.unmodifiableList(builder.data);
    }

    public int totalPages() {
        return totalPages;
    }

    public int totalItems() {
        return totalItems;
    }

    public List<OrganisationResponse> data() {
        return data;
    }

    public boolean hasData() {
        return true;
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
        ListOrganisationOutput that = (ListOrganisationOutput) other;
        return this.totalPages == that.totalPages
               && this.totalItems == that.totalItems
               && Objects.equals(this.data, that.data);
    }

    @Override
    public int hashCode() {
        return Objects.hash(totalPages, totalItems, data);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeInteger($SCHEMA_TOTAL_PAGES, totalPages);
        serializer.writeInteger($SCHEMA_TOTAL_ITEMS, totalItems);
        serializer.writeList($SCHEMA_DATA, data, data.size(), SharedSerde.OrganisationListSerializer.INSTANCE);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_TOTAL_PAGES, member, totalPages);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_TOTAL_ITEMS, member, totalItems);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_DATA, member, data);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link ListOrganisationOutput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.totalPages(this.totalPages);
        builder.totalItems(this.totalItems);
        builder.data(this.data);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link ListOrganisationOutput}.
     */
    public static final class Builder implements ShapeBuilder<ListOrganisationOutput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private int totalPages;
        private int totalItems;
        private List<OrganisationResponse> data;

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
        public Builder data(List<OrganisationResponse> data) {
            this.data = Objects.requireNonNull(data, "data cannot be null");
            tracker.setMember($SCHEMA_DATA);
            return this;
        }

        @Override
        public ListOrganisationOutput build() {
            tracker.validate();
            return new ListOrganisationOutput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> totalPages((int) SchemaUtils.validateSameMember($SCHEMA_TOTAL_PAGES, member, value));
                case 1 -> totalItems((int) SchemaUtils.validateSameMember($SCHEMA_TOTAL_ITEMS, member, value));
                case 2 -> data((List<OrganisationResponse>) SchemaUtils.validateSameMember($SCHEMA_DATA, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<ListOrganisationOutput> errorCorrection() {
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
                    case 2 -> builder.data(SharedSerde.deserializeOrganisationList(member, de));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

