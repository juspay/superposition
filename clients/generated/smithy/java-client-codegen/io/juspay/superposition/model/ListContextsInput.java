
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
import software.amazon.smithy.model.node.Node;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.DefaultTrait;
import software.amazon.smithy.model.traits.HttpHeaderTrait;
import software.amazon.smithy.model.traits.HttpQueryTrait;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class ListContextsInput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ListContextsInput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("workspace_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-tenant"),
                new RequiredTrait())
        .putMember("org_id", PreludeSchemas.STRING,
                new DefaultTrait(Node.from("juspay")),
                new RequiredTrait(),
                new HttpHeaderTrait("x-org-id"))
        .putMember("page", PreludeSchemas.INTEGER,
                new HttpQueryTrait("page"))
        .putMember("size", PreludeSchemas.INTEGER,
                new HttpQueryTrait("size"))
        .putMember("prefix", PreludeSchemas.STRING,
                new HttpQueryTrait("prefix"))
        .putMember("sort_on", ContextFilterSortOn.$SCHEMA,
                new HttpQueryTrait("sort_on"))
        .putMember("sort_by", SortBy.$SCHEMA,
                new HttpQueryTrait("sort_by"))
        .putMember("created_by", PreludeSchemas.STRING,
                new HttpQueryTrait("created_by"))
        .build();

    private static final Schema $SCHEMA_WORKSPACE_ID = $SCHEMA.member("workspace_id");
    private static final Schema $SCHEMA_ORG_ID = $SCHEMA.member("org_id");
    private static final Schema $SCHEMA_PAGE = $SCHEMA.member("page");
    private static final Schema $SCHEMA_SIZE = $SCHEMA.member("size");
    private static final Schema $SCHEMA_PREFIX = $SCHEMA.member("prefix");
    private static final Schema $SCHEMA_SORT_ON = $SCHEMA.member("sort_on");
    private static final Schema $SCHEMA_SORT_BY = $SCHEMA.member("sort_by");
    private static final Schema $SCHEMA_CREATED_BY = $SCHEMA.member("created_by");

    private final transient String workspaceId;
    private final transient String orgId;
    private final transient Integer page;
    private final transient Integer size;
    private final transient String prefix;
    private final transient ContextFilterSortOn sortOn;
    private final transient SortBy sortBy;
    private final transient String createdBy;

    private ListContextsInput(Builder builder) {
        this.workspaceId = builder.workspaceId;
        this.orgId = builder.orgId;
        this.page = builder.page;
        this.size = builder.size;
        this.prefix = builder.prefix;
        this.sortOn = builder.sortOn;
        this.sortBy = builder.sortBy;
        this.createdBy = builder.createdBy;
    }

    public String workspaceId() {
        return workspaceId;
    }

    public String orgId() {
        return orgId;
    }

    public Integer page() {
        return page;
    }

    public Integer size() {
        return size;
    }

    public String prefix() {
        return prefix;
    }

    public ContextFilterSortOn sortOn() {
        return sortOn;
    }

    public SortBy sortBy() {
        return sortBy;
    }

    public String createdBy() {
        return createdBy;
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
        ListContextsInput that = (ListContextsInput) other;
        return Objects.equals(this.workspaceId, that.workspaceId)
               && Objects.equals(this.orgId, that.orgId)
               && Objects.equals(this.page, that.page)
               && Objects.equals(this.size, that.size)
               && Objects.equals(this.prefix, that.prefix)
               && Objects.equals(this.sortOn, that.sortOn)
               && Objects.equals(this.sortBy, that.sortBy)
               && Objects.equals(this.createdBy, that.createdBy);
    }

    @Override
    public int hashCode() {
        return Objects.hash(workspaceId, orgId, page, size, prefix, sortOn, sortBy, createdBy);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_WORKSPACE_ID, workspaceId);
        serializer.writeString($SCHEMA_ORG_ID, orgId);
        if (page != null) {
            serializer.writeInteger($SCHEMA_PAGE, page);
        }
        if (size != null) {
            serializer.writeInteger($SCHEMA_SIZE, size);
        }
        if (prefix != null) {
            serializer.writeString($SCHEMA_PREFIX, prefix);
        }
        if (sortOn != null) {
            serializer.writeString($SCHEMA_SORT_ON, sortOn.value());
        }
        if (sortBy != null) {
            serializer.writeString($SCHEMA_SORT_BY, sortBy.value());
        }
        if (createdBy != null) {
            serializer.writeString($SCHEMA_CREATED_BY, createdBy);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, workspaceId);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, orgId);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_PAGE, member, page);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_SIZE, member, size);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_PREFIX, member, prefix);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_SORT_ON, member, sortOn);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_SORT_BY, member, sortBy);
            case 7 -> (T) SchemaUtils.validateSameMember($SCHEMA_CREATED_BY, member, createdBy);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link ListContextsInput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.workspaceId(this.workspaceId);
        builder.orgId(this.orgId);
        builder.page(this.page);
        builder.size(this.size);
        builder.prefix(this.prefix);
        builder.sortOn(this.sortOn);
        builder.sortBy(this.sortBy);
        builder.createdBy(this.createdBy);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link ListContextsInput}.
     */
    public static final class Builder implements ShapeBuilder<ListContextsInput> {
        private static final String ORG_ID_DEFAULT = "juspay";
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String workspaceId;
        private String orgId = ORG_ID_DEFAULT;
        private Integer page;
        private Integer size;
        private String prefix;
        private ContextFilterSortOn sortOn;
        private SortBy sortBy;
        private String createdBy;

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
         * @return this builder.
         */
        public Builder page(int page) {
            this.page = page;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder size(int size) {
            this.size = size;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder prefix(String prefix) {
            this.prefix = prefix;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder sortOn(ContextFilterSortOn sortOn) {
            this.sortOn = sortOn;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder sortBy(SortBy sortBy) {
            this.sortBy = sortBy;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder createdBy(String createdBy) {
            this.createdBy = createdBy;
            return this;
        }

        @Override
        public ListContextsInput build() {
            tracker.validate();
            return new ListContextsInput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> workspaceId((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, value));
                case 1 -> orgId((String) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, value));
                case 2 -> page((int) SchemaUtils.validateSameMember($SCHEMA_PAGE, member, value));
                case 3 -> size((int) SchemaUtils.validateSameMember($SCHEMA_SIZE, member, value));
                case 4 -> prefix((String) SchemaUtils.validateSameMember($SCHEMA_PREFIX, member, value));
                case 5 -> sortOn((ContextFilterSortOn) SchemaUtils.validateSameMember($SCHEMA_SORT_ON, member, value));
                case 6 -> sortBy((SortBy) SchemaUtils.validateSameMember($SCHEMA_SORT_BY, member, value));
                case 7 -> createdBy((String) SchemaUtils.validateSameMember($SCHEMA_CREATED_BY, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<ListContextsInput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_WORKSPACE_ID)) {
                workspaceId("");
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
                    case 2 -> builder.page(de.readInteger(member));
                    case 3 -> builder.size(de.readInteger(member));
                    case 4 -> builder.prefix(de.readString(member));
                    case 5 -> builder.sortOn(ContextFilterSortOn.builder().deserializeMember(de, member).build());
                    case 6 -> builder.sortBy(SortBy.builder().deserializeMember(de, member).build());
                    case 7 -> builder.createdBy(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

