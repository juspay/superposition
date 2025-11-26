
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
import software.amazon.smithy.model.traits.HttpQueryTrait;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class ListVariablesInput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ListVariablesInput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("count", PreludeSchemas.INTEGER,
                new HttpQueryTrait("count"))
        .putMember("page", PreludeSchemas.INTEGER,
                new HttpQueryTrait("page"))
        .putMember("all", PreludeSchemas.BOOLEAN,
                new HttpQueryTrait("all"))
        .putMember("workspace_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-workspace"),
                new RequiredTrait())
        .putMember("org_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-org-id"),
                new RequiredTrait())
        .putMember("name", PreludeSchemas.STRING,
                new HttpQueryTrait("name"))
        .putMember("created_by", PreludeSchemas.STRING,
                new HttpQueryTrait("created_by"))
        .putMember("last_modified_by", PreludeSchemas.STRING,
                new HttpQueryTrait("last_modified_by"))
        .putMember("sort_on", VariableSortOn.$SCHEMA,
                new HttpQueryTrait("sort_on"))
        .putMember("sort_by", SortBy.$SCHEMA,
                new HttpQueryTrait("sort_by"))
        .build();

    private static final Schema $SCHEMA_COUNT = $SCHEMA.member("count");
    private static final Schema $SCHEMA_PAGE = $SCHEMA.member("page");
    private static final Schema $SCHEMA_ALL = $SCHEMA.member("all");
    private static final Schema $SCHEMA_WORKSPACE_ID = $SCHEMA.member("workspace_id");
    private static final Schema $SCHEMA_ORG_ID = $SCHEMA.member("org_id");
    private static final Schema $SCHEMA_NAME = $SCHEMA.member("name");
    private static final Schema $SCHEMA_CREATED_BY = $SCHEMA.member("created_by");
    private static final Schema $SCHEMA_LAST_MODIFIED_BY = $SCHEMA.member("last_modified_by");
    private static final Schema $SCHEMA_SORT_ON = $SCHEMA.member("sort_on");
    private static final Schema $SCHEMA_SORT_BY = $SCHEMA.member("sort_by");

    private final transient Integer count;
    private final transient Integer page;
    private final transient Boolean all;
    private final transient String workspaceId;
    private final transient String orgId;
    private final transient String name;
    private final transient String createdBy;
    private final transient String lastModifiedBy;
    private final transient VariableSortOn sortOn;
    private final transient SortBy sortBy;

    private ListVariablesInput(Builder builder) {
        this.count = builder.count;
        this.page = builder.page;
        this.all = builder.all;
        this.workspaceId = builder.workspaceId;
        this.orgId = builder.orgId;
        this.name = builder.name;
        this.createdBy = builder.createdBy;
        this.lastModifiedBy = builder.lastModifiedBy;
        this.sortOn = builder.sortOn;
        this.sortBy = builder.sortBy;
    }

    /**
     * Number of items to be returned in each page.
     */
    public Integer count() {
        return count;
    }

    /**
     * Page number to retrieve, starting from 1.
     */
    public Integer page() {
        return page;
    }

    /**
     * If true, returns all requested items, ignoring pagination parameters page and count.
     */
    public Boolean all() {
        return all;
    }

    public String workspaceId() {
        return workspaceId;
    }

    public String orgId() {
        return orgId;
    }

    /**
     * Filter by variable name (exact match or substring, depending on backend implementation).
     */
    public String name() {
        return name;
    }

    /**
     * Filter by the user who created the variable
     */
    public String createdBy() {
        return createdBy;
    }

    /**
     * Filter by the user who last modified the variable
     */
    public String lastModifiedBy() {
        return lastModifiedBy;
    }

    /**
     * Field to sort the results by.
     */
    public VariableSortOn sortOn() {
        return sortOn;
    }

    /**
     * Sort order (ascending or descending).
     */
    public SortBy sortBy() {
        return sortBy;
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
        ListVariablesInput that = (ListVariablesInput) other;
        return Objects.equals(this.count, that.count)
               && Objects.equals(this.page, that.page)
               && Objects.equals(this.all, that.all)
               && Objects.equals(this.workspaceId, that.workspaceId)
               && Objects.equals(this.orgId, that.orgId)
               && Objects.equals(this.name, that.name)
               && Objects.equals(this.createdBy, that.createdBy)
               && Objects.equals(this.lastModifiedBy, that.lastModifiedBy)
               && Objects.equals(this.sortOn, that.sortOn)
               && Objects.equals(this.sortBy, that.sortBy);
    }

    @Override
    public int hashCode() {
        return Objects.hash(count, page, all, workspaceId, orgId, name, createdBy, lastModifiedBy, sortOn, sortBy);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        if (count != null) {
            serializer.writeInteger($SCHEMA_COUNT, count);
        }
        if (page != null) {
            serializer.writeInteger($SCHEMA_PAGE, page);
        }
        if (all != null) {
            serializer.writeBoolean($SCHEMA_ALL, all);
        }
        serializer.writeString($SCHEMA_WORKSPACE_ID, workspaceId);
        serializer.writeString($SCHEMA_ORG_ID, orgId);
        if (name != null) {
            serializer.writeString($SCHEMA_NAME, name);
        }
        if (createdBy != null) {
            serializer.writeString($SCHEMA_CREATED_BY, createdBy);
        }
        if (lastModifiedBy != null) {
            serializer.writeString($SCHEMA_LAST_MODIFIED_BY, lastModifiedBy);
        }
        if (sortOn != null) {
            serializer.writeString($SCHEMA_SORT_ON, sortOn.value());
        }
        if (sortBy != null) {
            serializer.writeString($SCHEMA_SORT_BY, sortBy.value());
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, workspaceId);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, orgId);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_COUNT, member, count);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_PAGE, member, page);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_ALL, member, all);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_NAME, member, name);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_CREATED_BY, member, createdBy);
            case 7 -> (T) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_BY, member, lastModifiedBy);
            case 8 -> (T) SchemaUtils.validateSameMember($SCHEMA_SORT_ON, member, sortOn);
            case 9 -> (T) SchemaUtils.validateSameMember($SCHEMA_SORT_BY, member, sortBy);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link ListVariablesInput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.count(this.count);
        builder.page(this.page);
        builder.all(this.all);
        builder.workspaceId(this.workspaceId);
        builder.orgId(this.orgId);
        builder.name(this.name);
        builder.createdBy(this.createdBy);
        builder.lastModifiedBy(this.lastModifiedBy);
        builder.sortOn(this.sortOn);
        builder.sortBy(this.sortBy);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link ListVariablesInput}.
     */
    public static final class Builder implements ShapeBuilder<ListVariablesInput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private Integer count;
        private Integer page;
        private Boolean all;
        private String workspaceId;
        private String orgId;
        private String name;
        private String createdBy;
        private String lastModifiedBy;
        private VariableSortOn sortOn;
        private SortBy sortBy;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * Number of items to be returned in each page.
         *
         * @return this builder.
         */
        public Builder count(int count) {
            this.count = count;
            return this;
        }

        /**
         * Page number to retrieve, starting from 1.
         *
         * @return this builder.
         */
        public Builder page(int page) {
            this.page = page;
            return this;
        }

        /**
         * If true, returns all requested items, ignoring pagination parameters page and count.
         *
         * @return this builder.
         */
        public Builder all(boolean all) {
            this.all = all;
            return this;
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
         * Filter by variable name (exact match or substring, depending on backend implementation).
         *
         * @return this builder.
         */
        public Builder name(String name) {
            this.name = name;
            return this;
        }

        /**
         * Filter by the user who created the variable
         *
         * @return this builder.
         */
        public Builder createdBy(String createdBy) {
            this.createdBy = createdBy;
            return this;
        }

        /**
         * Filter by the user who last modified the variable
         *
         * @return this builder.
         */
        public Builder lastModifiedBy(String lastModifiedBy) {
            this.lastModifiedBy = lastModifiedBy;
            return this;
        }

        /**
         * Field to sort the results by.
         *
         * @return this builder.
         */
        public Builder sortOn(VariableSortOn sortOn) {
            this.sortOn = sortOn;
            return this;
        }

        /**
         * Sort order (ascending or descending).
         *
         * @return this builder.
         */
        public Builder sortBy(SortBy sortBy) {
            this.sortBy = sortBy;
            return this;
        }

        @Override
        public ListVariablesInput build() {
            tracker.validate();
            return new ListVariablesInput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> workspaceId((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, value));
                case 1 -> orgId((String) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, value));
                case 2 -> count((int) SchemaUtils.validateSameMember($SCHEMA_COUNT, member, value));
                case 3 -> page((int) SchemaUtils.validateSameMember($SCHEMA_PAGE, member, value));
                case 4 -> all((boolean) SchemaUtils.validateSameMember($SCHEMA_ALL, member, value));
                case 5 -> name((String) SchemaUtils.validateSameMember($SCHEMA_NAME, member, value));
                case 6 -> createdBy((String) SchemaUtils.validateSameMember($SCHEMA_CREATED_BY, member, value));
                case 7 -> lastModifiedBy((String) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_BY, member, value));
                case 8 -> sortOn((VariableSortOn) SchemaUtils.validateSameMember($SCHEMA_SORT_ON, member, value));
                case 9 -> sortBy((SortBy) SchemaUtils.validateSameMember($SCHEMA_SORT_BY, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<ListVariablesInput> errorCorrection() {
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
                    case 2 -> builder.count(de.readInteger(member));
                    case 3 -> builder.page(de.readInteger(member));
                    case 4 -> builder.all(de.readBoolean(member));
                    case 5 -> builder.name(de.readString(member));
                    case 6 -> builder.createdBy(de.readString(member));
                    case 7 -> builder.lastModifiedBy(de.readString(member));
                    case 8 -> builder.sortOn(VariableSortOn.builder().deserializeMember(de, member).build());
                    case 9 -> builder.sortBy(SortBy.builder().deserializeMember(de, member).build());
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

