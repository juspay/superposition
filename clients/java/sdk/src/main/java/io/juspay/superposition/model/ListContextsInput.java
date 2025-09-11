
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
        .putMember("count", PreludeSchemas.INTEGER,
                new HttpQueryTrait("count"))
        .putMember("all", PreludeSchemas.BOOLEAN,
                new HttpQueryTrait("all"))
        .putMember("prefix", PreludeSchemas.STRING,
                new HttpQueryTrait("prefix"))
        .putMember("sort_on", ContextFilterSortOn.$SCHEMA,
                new HttpQueryTrait("sort_on"))
        .putMember("sort_by", SortBy.$SCHEMA,
                new HttpQueryTrait("sort_by"))
        .putMember("created_by", PreludeSchemas.STRING,
                new HttpQueryTrait("created_by"))
        .putMember("last_modified_by", PreludeSchemas.STRING,
                new HttpQueryTrait("last_modified_by"))
        .putMember("plaintext", PreludeSchemas.STRING,
                new HttpQueryTrait("plaintext"))
        .putMember("dimension_match_strategy", DimensionMatchStrategy.$SCHEMA,
                new HttpQueryTrait("dimension_match_strategy"))
        .build();

    private static final Schema $SCHEMA_WORKSPACE_ID = $SCHEMA.member("workspace_id");
    private static final Schema $SCHEMA_ORG_ID = $SCHEMA.member("org_id");
    private static final Schema $SCHEMA_PAGE = $SCHEMA.member("page");
    private static final Schema $SCHEMA_COUNT = $SCHEMA.member("count");
    private static final Schema $SCHEMA_ALL = $SCHEMA.member("all");
    private static final Schema $SCHEMA_PREFIX = $SCHEMA.member("prefix");
    private static final Schema $SCHEMA_SORT_ON = $SCHEMA.member("sort_on");
    private static final Schema $SCHEMA_SORT_BY = $SCHEMA.member("sort_by");
    private static final Schema $SCHEMA_CREATED_BY = $SCHEMA.member("created_by");
    private static final Schema $SCHEMA_LAST_MODIFIED_BY = $SCHEMA.member("last_modified_by");
    private static final Schema $SCHEMA_PLAINTEXT = $SCHEMA.member("plaintext");
    private static final Schema $SCHEMA_DIMENSION_MATCH_STRATEGY = $SCHEMA.member("dimension_match_strategy");

    private final transient String workspaceId;
    private final transient String orgId;
    private final transient Integer page;
    private final transient Integer count;
    private final transient Boolean all;
    private final transient String prefix;
    private final transient ContextFilterSortOn sortOn;
    private final transient SortBy sortBy;
    private final transient String createdBy;
    private final transient String lastModifiedBy;
    private final transient String plaintext;
    private final transient DimensionMatchStrategy dimensionMatchStrategy;

    private ListContextsInput(Builder builder) {
        this.workspaceId = builder.workspaceId;
        this.orgId = builder.orgId;
        this.page = builder.page;
        this.count = builder.count;
        this.all = builder.all;
        this.prefix = builder.prefix;
        this.sortOn = builder.sortOn;
        this.sortBy = builder.sortBy;
        this.createdBy = builder.createdBy;
        this.lastModifiedBy = builder.lastModifiedBy;
        this.plaintext = builder.plaintext;
        this.dimensionMatchStrategy = builder.dimensionMatchStrategy;
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

    public Integer count() {
        return count;
    }

    public Boolean all() {
        return all;
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

    public String lastModifiedBy() {
        return lastModifiedBy;
    }

    public String plaintext() {
        return plaintext;
    }

    public DimensionMatchStrategy dimensionMatchStrategy() {
        return dimensionMatchStrategy;
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
               && Objects.equals(this.count, that.count)
               && Objects.equals(this.all, that.all)
               && Objects.equals(this.prefix, that.prefix)
               && Objects.equals(this.sortOn, that.sortOn)
               && Objects.equals(this.sortBy, that.sortBy)
               && Objects.equals(this.createdBy, that.createdBy)
               && Objects.equals(this.lastModifiedBy, that.lastModifiedBy)
               && Objects.equals(this.plaintext, that.plaintext)
               && Objects.equals(this.dimensionMatchStrategy, that.dimensionMatchStrategy);
    }

    @Override
    public int hashCode() {
        return Objects.hash(workspaceId, orgId, page, count, all, prefix, sortOn, sortBy, createdBy, lastModifiedBy, plaintext, dimensionMatchStrategy);
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
        if (count != null) {
            serializer.writeInteger($SCHEMA_COUNT, count);
        }
        if (all != null) {
            serializer.writeBoolean($SCHEMA_ALL, all);
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
        if (lastModifiedBy != null) {
            serializer.writeString($SCHEMA_LAST_MODIFIED_BY, lastModifiedBy);
        }
        if (plaintext != null) {
            serializer.writeString($SCHEMA_PLAINTEXT, plaintext);
        }
        if (dimensionMatchStrategy != null) {
            serializer.writeString($SCHEMA_DIMENSION_MATCH_STRATEGY, dimensionMatchStrategy.value());
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, workspaceId);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, orgId);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_PAGE, member, page);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_COUNT, member, count);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_ALL, member, all);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_PREFIX, member, prefix);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_SORT_ON, member, sortOn);
            case 7 -> (T) SchemaUtils.validateSameMember($SCHEMA_SORT_BY, member, sortBy);
            case 8 -> (T) SchemaUtils.validateSameMember($SCHEMA_CREATED_BY, member, createdBy);
            case 9 -> (T) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_BY, member, lastModifiedBy);
            case 10 -> (T) SchemaUtils.validateSameMember($SCHEMA_PLAINTEXT, member, plaintext);
            case 11 -> (T) SchemaUtils.validateSameMember($SCHEMA_DIMENSION_MATCH_STRATEGY, member, dimensionMatchStrategy);
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
        builder.count(this.count);
        builder.all(this.all);
        builder.prefix(this.prefix);
        builder.sortOn(this.sortOn);
        builder.sortBy(this.sortBy);
        builder.createdBy(this.createdBy);
        builder.lastModifiedBy(this.lastModifiedBy);
        builder.plaintext(this.plaintext);
        builder.dimensionMatchStrategy(this.dimensionMatchStrategy);
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
        private Integer count;
        private Boolean all;
        private String prefix;
        private ContextFilterSortOn sortOn;
        private SortBy sortBy;
        private String createdBy;
        private String lastModifiedBy;
        private String plaintext;
        private DimensionMatchStrategy dimensionMatchStrategy;

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
        public Builder count(int count) {
            this.count = count;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder all(boolean all) {
            this.all = all;
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

        /**
         * @return this builder.
         */
        public Builder lastModifiedBy(String lastModifiedBy) {
            this.lastModifiedBy = lastModifiedBy;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder plaintext(String plaintext) {
            this.plaintext = plaintext;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder dimensionMatchStrategy(DimensionMatchStrategy dimensionMatchStrategy) {
            this.dimensionMatchStrategy = dimensionMatchStrategy;
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
                case 3 -> count((int) SchemaUtils.validateSameMember($SCHEMA_COUNT, member, value));
                case 4 -> all((boolean) SchemaUtils.validateSameMember($SCHEMA_ALL, member, value));
                case 5 -> prefix((String) SchemaUtils.validateSameMember($SCHEMA_PREFIX, member, value));
                case 6 -> sortOn((ContextFilterSortOn) SchemaUtils.validateSameMember($SCHEMA_SORT_ON, member, value));
                case 7 -> sortBy((SortBy) SchemaUtils.validateSameMember($SCHEMA_SORT_BY, member, value));
                case 8 -> createdBy((String) SchemaUtils.validateSameMember($SCHEMA_CREATED_BY, member, value));
                case 9 -> lastModifiedBy((String) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_BY, member, value));
                case 10 -> plaintext((String) SchemaUtils.validateSameMember($SCHEMA_PLAINTEXT, member, value));
                case 11 -> dimensionMatchStrategy((DimensionMatchStrategy) SchemaUtils.validateSameMember($SCHEMA_DIMENSION_MATCH_STRATEGY, member, value));
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
                    case 3 -> builder.count(de.readInteger(member));
                    case 4 -> builder.all(de.readBoolean(member));
                    case 5 -> builder.prefix(de.readString(member));
                    case 6 -> builder.sortOn(ContextFilterSortOn.builder().deserializeMember(de, member).build());
                    case 7 -> builder.sortBy(SortBy.builder().deserializeMember(de, member).build());
                    case 8 -> builder.createdBy(de.readString(member));
                    case 9 -> builder.lastModifiedBy(de.readString(member));
                    case 10 -> builder.plaintext(de.readString(member));
                    case 11 -> builder.dimensionMatchStrategy(DimensionMatchStrategy.builder().deserializeMember(de, member).build());
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

