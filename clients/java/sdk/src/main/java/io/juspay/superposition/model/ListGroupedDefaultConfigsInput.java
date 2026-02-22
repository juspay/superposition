
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
import software.amazon.smithy.model.traits.HttpHeaderTrait;
import software.amazon.smithy.model.traits.HttpQueryTrait;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class ListGroupedDefaultConfigsInput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ListGroupedDefaultConfigsInput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("workspace_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-workspace"),
                new RequiredTrait())
        .putMember("org_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-org-id"),
                new RequiredTrait())
        .putMember("count", PreludeSchemas.INTEGER,
                new HttpQueryTrait("count"))
        .putMember("page", PreludeSchemas.INTEGER,
                new HttpQueryTrait("page"))
        .putMember("all", PreludeSchemas.BOOLEAN,
                new HttpQueryTrait("all"))
        .putMember("name", SharedSchemas.STRING_LIST,
                new HttpQueryTrait("name"))
        .putMember("prefix", PreludeSchemas.STRING,
                new HttpQueryTrait("prefix"))
        .putMember("sort_by", SortBy.$SCHEMA,
                new HttpQueryTrait("sort_by"))
        .putMember("sort_on", DefaultConfigSortOn.$SCHEMA,
                new HttpQueryTrait("sort_on"))
        .build();

    private static final Schema $SCHEMA_WORKSPACE_ID = $SCHEMA.member("workspace_id");
    private static final Schema $SCHEMA_ORG_ID = $SCHEMA.member("org_id");
    private static final Schema $SCHEMA_COUNT = $SCHEMA.member("count");
    private static final Schema $SCHEMA_PAGE = $SCHEMA.member("page");
    private static final Schema $SCHEMA_ALL = $SCHEMA.member("all");
    private static final Schema $SCHEMA_NAME = $SCHEMA.member("name");
    private static final Schema $SCHEMA_PREFIX = $SCHEMA.member("prefix");
    private static final Schema $SCHEMA_SORT_BY = $SCHEMA.member("sort_by");
    private static final Schema $SCHEMA_SORT_ON = $SCHEMA.member("sort_on");

    private final transient String workspaceId;
    private final transient String orgId;
    private final transient Integer count;
    private final transient Integer page;
    private final transient Boolean all;
    private final transient List<String> name;
    private final transient String prefix;
    private final transient SortBy sortBy;
    private final transient DefaultConfigSortOn sortOn;

    private ListGroupedDefaultConfigsInput(Builder builder) {
        this.workspaceId = builder.workspaceId;
        this.orgId = builder.orgId;
        this.count = builder.count;
        this.page = builder.page;
        this.all = builder.all;
        this.name = builder.name == null ? null : Collections.unmodifiableList(builder.name);
        this.prefix = builder.prefix;
        this.sortBy = builder.sortBy;
        this.sortOn = builder.sortOn;
    }

    public String workspaceId() {
        return workspaceId;
    }

    public String orgId() {
        return orgId;
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

    public List<String> name() {
        if (name == null) {
            return Collections.emptyList();
        }
        return name;
    }

    public boolean hasName() {
        return name != null;
    }

    public String prefix() {
        return prefix;
    }

    public SortBy sortBy() {
        return sortBy;
    }

    public DefaultConfigSortOn sortOn() {
        return sortOn;
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
        ListGroupedDefaultConfigsInput that = (ListGroupedDefaultConfigsInput) other;
        return Objects.equals(this.workspaceId, that.workspaceId)
               && Objects.equals(this.orgId, that.orgId)
               && Objects.equals(this.count, that.count)
               && Objects.equals(this.page, that.page)
               && Objects.equals(this.all, that.all)
               && Objects.equals(this.name, that.name)
               && Objects.equals(this.prefix, that.prefix)
               && Objects.equals(this.sortBy, that.sortBy)
               && Objects.equals(this.sortOn, that.sortOn);
    }

    @Override
    public int hashCode() {
        return Objects.hash(workspaceId, orgId, count, page, all, name, prefix, sortBy, sortOn);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_WORKSPACE_ID, workspaceId);
        serializer.writeString($SCHEMA_ORG_ID, orgId);
        if (count != null) {
            serializer.writeInteger($SCHEMA_COUNT, count);
        }
        if (page != null) {
            serializer.writeInteger($SCHEMA_PAGE, page);
        }
        if (all != null) {
            serializer.writeBoolean($SCHEMA_ALL, all);
        }
        if (name != null) {
            serializer.writeList($SCHEMA_NAME, name, name.size(), SharedSerde.StringListSerializer.INSTANCE);
        }
        if (prefix != null) {
            serializer.writeString($SCHEMA_PREFIX, prefix);
        }
        if (sortBy != null) {
            serializer.writeString($SCHEMA_SORT_BY, sortBy.value());
        }
        if (sortOn != null) {
            serializer.writeString($SCHEMA_SORT_ON, sortOn.value());
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
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_PREFIX, member, prefix);
            case 7 -> (T) SchemaUtils.validateSameMember($SCHEMA_SORT_BY, member, sortBy);
            case 8 -> (T) SchemaUtils.validateSameMember($SCHEMA_SORT_ON, member, sortOn);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link ListGroupedDefaultConfigsInput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.workspaceId(this.workspaceId);
        builder.orgId(this.orgId);
        builder.count(this.count);
        builder.page(this.page);
        builder.all(this.all);
        builder.name(this.name);
        builder.prefix(this.prefix);
        builder.sortBy(this.sortBy);
        builder.sortOn(this.sortOn);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link ListGroupedDefaultConfigsInput}.
     */
    public static final class Builder implements ShapeBuilder<ListGroupedDefaultConfigsInput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String workspaceId;
        private String orgId;
        private Integer count;
        private Integer page;
        private Boolean all;
        private List<String> name;
        private String prefix;
        private SortBy sortBy;
        private DefaultConfigSortOn sortOn;

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
         * @return this builder.
         */
        public Builder name(List<String> name) {
            this.name = name;
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
        public Builder sortBy(SortBy sortBy) {
            this.sortBy = sortBy;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder sortOn(DefaultConfigSortOn sortOn) {
            this.sortOn = sortOn;
            return this;
        }

        @Override
        public ListGroupedDefaultConfigsInput build() {
            tracker.validate();
            return new ListGroupedDefaultConfigsInput(this);
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
                case 5 -> name((List<String>) SchemaUtils.validateSameMember($SCHEMA_NAME, member, value));
                case 6 -> prefix((String) SchemaUtils.validateSameMember($SCHEMA_PREFIX, member, value));
                case 7 -> sortBy((SortBy) SchemaUtils.validateSameMember($SCHEMA_SORT_BY, member, value));
                case 8 -> sortOn((DefaultConfigSortOn) SchemaUtils.validateSameMember($SCHEMA_SORT_ON, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<ListGroupedDefaultConfigsInput> errorCorrection() {
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
                    case 5 -> builder.name(SharedSerde.deserializeStringList(member, de));
                    case 6 -> builder.prefix(de.readString(member));
                    case 7 -> builder.sortBy(SortBy.builder().deserializeMember(de, member).build());
                    case 8 -> builder.sortOn(DefaultConfigSortOn.builder().deserializeMember(de, member).build());
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

