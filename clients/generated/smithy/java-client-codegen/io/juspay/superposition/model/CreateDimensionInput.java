
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
import software.amazon.smithy.java.core.serde.document.Document;
import software.amazon.smithy.model.node.Node;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.DefaultTrait;
import software.amazon.smithy.model.traits.HttpHeaderTrait;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class CreateDimensionInput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#CreateDimensionInput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("workspace_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-tenant"),
                new RequiredTrait())
        .putMember("org_id", PreludeSchemas.STRING,
                new DefaultTrait(Node.from("juspay")),
                new RequiredTrait(),
                new HttpHeaderTrait("x-org-id"))
        .putMember("dimension", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("position", PreludeSchemas.INTEGER,
                new RequiredTrait())
        .putMember("schema", PreludeSchemas.DOCUMENT,
                new RequiredTrait())
        .putMember("function_name", PreludeSchemas.STRING)
        .putMember("dependencies", SharedSchemas.DEPENDENCIES)
        .putMember("description", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("change_reason", PreludeSchemas.STRING,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_WORKSPACE_ID = $SCHEMA.member("workspace_id");
    private static final Schema $SCHEMA_ORG_ID = $SCHEMA.member("org_id");
    private static final Schema $SCHEMA_DIMENSION = $SCHEMA.member("dimension");
    private static final Schema $SCHEMA_POSITION = $SCHEMA.member("position");
    private static final Schema $SCHEMA_SCHEMA_MEMBER = $SCHEMA.member("schema");
    private static final Schema $SCHEMA_FUNCTION_NAME = $SCHEMA.member("function_name");
    private static final Schema $SCHEMA_DEPENDENCIES = $SCHEMA.member("dependencies");
    private static final Schema $SCHEMA_DESCRIPTION = $SCHEMA.member("description");
    private static final Schema $SCHEMA_CHANGE_REASON = $SCHEMA.member("change_reason");

    private final transient String workspaceId;
    private final transient String orgId;
    private final transient String dimension;
    private final transient int position;
    private final transient Document schemaMember;
    private final transient String functionName;
    private final transient List<String> dependencies;
    private final transient String description;
    private final transient String changeReason;

    private CreateDimensionInput(Builder builder) {
        this.workspaceId = builder.workspaceId;
        this.orgId = builder.orgId;
        this.dimension = builder.dimension;
        this.position = builder.position;
        this.schemaMember = builder.schemaMember;
        this.functionName = builder.functionName;
        this.dependencies = builder.dependencies == null ? null : Collections.unmodifiableList(builder.dependencies);
        this.description = builder.description;
        this.changeReason = builder.changeReason;
    }

    public String workspaceId() {
        return workspaceId;
    }

    public String orgId() {
        return orgId;
    }

    public String dimension() {
        return dimension;
    }

    public int position() {
        return position;
    }

    public Document schemaMember() {
        return schemaMember;
    }

    public String functionName() {
        return functionName;
    }

    public List<String> dependencies() {
        if (dependencies == null) {
            return Collections.emptyList();
        }
        return dependencies;
    }

    public boolean hasDependencies() {
        return dependencies != null;
    }

    public String description() {
        return description;
    }

    public String changeReason() {
        return changeReason;
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
        CreateDimensionInput that = (CreateDimensionInput) other;
        return Objects.equals(this.workspaceId, that.workspaceId)
               && Objects.equals(this.orgId, that.orgId)
               && Objects.equals(this.dimension, that.dimension)
               && this.position == that.position
               && Objects.equals(this.schemaMember, that.schemaMember)
               && Objects.equals(this.functionName, that.functionName)
               && Objects.equals(this.dependencies, that.dependencies)
               && Objects.equals(this.description, that.description)
               && Objects.equals(this.changeReason, that.changeReason);
    }

    @Override
    public int hashCode() {
        return Objects.hash(workspaceId, orgId, dimension, position, schemaMember, functionName, dependencies, description, changeReason);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_WORKSPACE_ID, workspaceId);
        serializer.writeString($SCHEMA_ORG_ID, orgId);
        serializer.writeString($SCHEMA_DIMENSION, dimension);
        serializer.writeInteger($SCHEMA_POSITION, position);
        serializer.writeDocument($SCHEMA_SCHEMA_MEMBER, schemaMember);
        if (functionName != null) {
            serializer.writeString($SCHEMA_FUNCTION_NAME, functionName);
        }
        if (dependencies != null) {
            serializer.writeList($SCHEMA_DEPENDENCIES, dependencies, dependencies.size(), SharedSerde.DependenciesSerializer.INSTANCE);
        }
        serializer.writeString($SCHEMA_DESCRIPTION, description);
        serializer.writeString($SCHEMA_CHANGE_REASON, changeReason);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, workspaceId);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_DIMENSION, member, dimension);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_POSITION, member, position);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_SCHEMA_MEMBER, member, schemaMember);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, description);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, changeReason);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, orgId);
            case 7 -> (T) SchemaUtils.validateSameMember($SCHEMA_FUNCTION_NAME, member, functionName);
            case 8 -> (T) SchemaUtils.validateSameMember($SCHEMA_DEPENDENCIES, member, dependencies);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link CreateDimensionInput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.workspaceId(this.workspaceId);
        builder.orgId(this.orgId);
        builder.dimension(this.dimension);
        builder.position(this.position);
        builder.schemaMember(this.schemaMember);
        builder.functionName(this.functionName);
        builder.dependencies(this.dependencies);
        builder.description(this.description);
        builder.changeReason(this.changeReason);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link CreateDimensionInput}.
     */
    public static final class Builder implements ShapeBuilder<CreateDimensionInput> {
        private static final String ORG_ID_DEFAULT = "juspay";
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String workspaceId;
        private String orgId = ORG_ID_DEFAULT;
        private String dimension;
        private int position;
        private Document schemaMember;
        private String functionName;
        private List<String> dependencies;
        private String description;
        private String changeReason;

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
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder dimension(String dimension) {
            this.dimension = Objects.requireNonNull(dimension, "dimension cannot be null");
            tracker.setMember($SCHEMA_DIMENSION);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder position(int position) {
            this.position = position;
            tracker.setMember($SCHEMA_POSITION);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder schemaMember(Document schemaMember) {
            this.schemaMember = Objects.requireNonNull(schemaMember, "schemaMember cannot be null");
            tracker.setMember($SCHEMA_SCHEMA_MEMBER);
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder functionName(String functionName) {
            this.functionName = functionName;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder dependencies(List<String> dependencies) {
            this.dependencies = dependencies;
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder description(String description) {
            this.description = Objects.requireNonNull(description, "description cannot be null");
            tracker.setMember($SCHEMA_DESCRIPTION);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder changeReason(String changeReason) {
            this.changeReason = Objects.requireNonNull(changeReason, "changeReason cannot be null");
            tracker.setMember($SCHEMA_CHANGE_REASON);
            return this;
        }

        @Override
        public CreateDimensionInput build() {
            tracker.validate();
            return new CreateDimensionInput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> workspaceId((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, value));
                case 1 -> dimension((String) SchemaUtils.validateSameMember($SCHEMA_DIMENSION, member, value));
                case 2 -> position((int) SchemaUtils.validateSameMember($SCHEMA_POSITION, member, value));
                case 3 -> schemaMember((Document) SchemaUtils.validateSameMember($SCHEMA_SCHEMA_MEMBER, member, value));
                case 4 -> description((String) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, value));
                case 5 -> changeReason((String) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, value));
                case 6 -> orgId((String) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, value));
                case 7 -> functionName((String) SchemaUtils.validateSameMember($SCHEMA_FUNCTION_NAME, member, value));
                case 8 -> dependencies((List<String>) SchemaUtils.validateSameMember($SCHEMA_DEPENDENCIES, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<CreateDimensionInput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_WORKSPACE_ID)) {
                workspaceId("");
            }
            if (!tracker.checkMember($SCHEMA_DIMENSION)) {
                dimension("");
            }
            if (!tracker.checkMember($SCHEMA_POSITION)) {
                tracker.setMember($SCHEMA_POSITION);
            }
            if (!tracker.checkMember($SCHEMA_SCHEMA_MEMBER)) {
                tracker.setMember($SCHEMA_SCHEMA_MEMBER);
            }
            if (!tracker.checkMember($SCHEMA_DESCRIPTION)) {
                description("");
            }
            if (!tracker.checkMember($SCHEMA_CHANGE_REASON)) {
                changeReason("");
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
                    case 1 -> builder.dimension(de.readString(member));
                    case 2 -> builder.position(de.readInteger(member));
                    case 3 -> builder.schemaMember(de.readDocument());
                    case 4 -> builder.description(de.readString(member));
                    case 5 -> builder.changeReason(de.readString(member));
                    case 6 -> builder.orgId(de.readString(member));
                    case 7 -> builder.functionName(de.readString(member));
                    case 8 -> builder.dependencies(SharedSerde.deserializeDependencies(member, de));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

