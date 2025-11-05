
package io.juspay.superposition.model;

import java.time.Instant;
import java.util.Collections;
import java.util.List;
import java.util.Map;
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
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class UpdateDimensionOutput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#DimensionResponse");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("dimension", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("position", PreludeSchemas.INTEGER,
                new RequiredTrait())
        .putMember("schema", SharedSchemas.OBJECT,
                new RequiredTrait())
        .putMember("function_name", PreludeSchemas.STRING)
        .putMember("description", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("change_reason", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("last_modified_at", SharedSchemas.DATE_TIME,
                new RequiredTrait())
        .putMember("last_modified_by", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("created_at", SharedSchemas.DATE_TIME,
                new RequiredTrait())
        .putMember("created_by", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("dependency_graph", SharedSchemas.DEPENDENCY_GRAPH,
                new RequiredTrait())
        .putMember("dimension_type", DimensionType.$SCHEMA,
                new RequiredTrait())
        .putMember("autocomplete_function_name", PreludeSchemas.STRING)
        .putMember("mandatory", PreludeSchemas.BOOLEAN,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_DIMENSION = $SCHEMA.member("dimension");
    private static final Schema $SCHEMA_POSITION = $SCHEMA.member("position");
    private static final Schema $SCHEMA_SCHEMA_MEMBER = $SCHEMA.member("schema");
    private static final Schema $SCHEMA_FUNCTION_NAME = $SCHEMA.member("function_name");
    private static final Schema $SCHEMA_DESCRIPTION = $SCHEMA.member("description");
    private static final Schema $SCHEMA_CHANGE_REASON = $SCHEMA.member("change_reason");
    private static final Schema $SCHEMA_LAST_MODIFIED_AT = $SCHEMA.member("last_modified_at");
    private static final Schema $SCHEMA_LAST_MODIFIED_BY = $SCHEMA.member("last_modified_by");
    private static final Schema $SCHEMA_CREATED_AT = $SCHEMA.member("created_at");
    private static final Schema $SCHEMA_CREATED_BY = $SCHEMA.member("created_by");
    private static final Schema $SCHEMA_DEPENDENCY_GRAPH = $SCHEMA.member("dependency_graph");
    private static final Schema $SCHEMA_DIMENSION_TYPE = $SCHEMA.member("dimension_type");
    private static final Schema $SCHEMA_AUTOCOMPLETE_FUNCTION_NAME = $SCHEMA.member("autocomplete_function_name");
    private static final Schema $SCHEMA_MANDATORY = $SCHEMA.member("mandatory");

    private final transient String dimension;
    private final transient int position;
    private final transient Map<String, Document> schemaMember;
    private final transient String functionName;
    private final transient String description;
    private final transient String changeReason;
    private final transient Instant lastModifiedAt;
    private final transient String lastModifiedBy;
    private final transient Instant createdAt;
    private final transient String createdBy;
    private final transient Map<String, List<String>> dependencyGraph;
    private final transient DimensionType dimensionType;
    private final transient String autocompleteFunctionName;
    private final transient boolean mandatory;

    private UpdateDimensionOutput(Builder builder) {
        this.dimension = builder.dimension;
        this.position = builder.position;
        this.schemaMember = Collections.unmodifiableMap(builder.schemaMember);
        this.functionName = builder.functionName;
        this.description = builder.description;
        this.changeReason = builder.changeReason;
        this.lastModifiedAt = builder.lastModifiedAt;
        this.lastModifiedBy = builder.lastModifiedBy;
        this.createdAt = builder.createdAt;
        this.createdBy = builder.createdBy;
        this.dependencyGraph = Collections.unmodifiableMap(builder.dependencyGraph);
        this.dimensionType = builder.dimensionType;
        this.autocompleteFunctionName = builder.autocompleteFunctionName;
        this.mandatory = builder.mandatory;
    }

    public String dimension() {
        return dimension;
    }

    public int position() {
        return position;
    }

    public Map<String, Document> schemaMember() {
        return schemaMember;
    }

    public boolean hasSchemaMember() {
        return true;
    }

    public String functionName() {
        return functionName;
    }

    public String description() {
        return description;
    }

    public String changeReason() {
        return changeReason;
    }

    public Instant lastModifiedAt() {
        return lastModifiedAt;
    }

    public String lastModifiedBy() {
        return lastModifiedBy;
    }

    public Instant createdAt() {
        return createdAt;
    }

    public String createdBy() {
        return createdBy;
    }

    public Map<String, List<String>> dependencyGraph() {
        return dependencyGraph;
    }

    public boolean hasDependencyGraph() {
        return true;
    }

    public DimensionType dimensionType() {
        return dimensionType;
    }

    public String autocompleteFunctionName() {
        return autocompleteFunctionName;
    }

    public boolean mandatory() {
        return mandatory;
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
        UpdateDimensionOutput that = (UpdateDimensionOutput) other;
        return Objects.equals(this.dimension, that.dimension)
               && this.position == that.position
               && Objects.equals(this.schemaMember, that.schemaMember)
               && Objects.equals(this.functionName, that.functionName)
               && Objects.equals(this.description, that.description)
               && Objects.equals(this.changeReason, that.changeReason)
               && Objects.equals(this.lastModifiedAt, that.lastModifiedAt)
               && Objects.equals(this.lastModifiedBy, that.lastModifiedBy)
               && Objects.equals(this.createdAt, that.createdAt)
               && Objects.equals(this.createdBy, that.createdBy)
               && Objects.equals(this.dependencyGraph, that.dependencyGraph)
               && Objects.equals(this.dimensionType, that.dimensionType)
               && Objects.equals(this.autocompleteFunctionName, that.autocompleteFunctionName)
               && this.mandatory == that.mandatory;
    }

    @Override
    public int hashCode() {
        return Objects.hash(dimension, position, schemaMember, functionName, description, changeReason, lastModifiedAt, lastModifiedBy, createdAt, createdBy, dependencyGraph, dimensionType, autocompleteFunctionName, mandatory);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_DIMENSION, dimension);
        serializer.writeInteger($SCHEMA_POSITION, position);
        serializer.writeMap($SCHEMA_SCHEMA_MEMBER, schemaMember, schemaMember.size(), SharedSerde.ObjectShapeSerializer.INSTANCE);
        if (functionName != null) {
            serializer.writeString($SCHEMA_FUNCTION_NAME, functionName);
        }
        serializer.writeString($SCHEMA_DESCRIPTION, description);
        serializer.writeString($SCHEMA_CHANGE_REASON, changeReason);
        serializer.writeTimestamp($SCHEMA_LAST_MODIFIED_AT, lastModifiedAt);
        serializer.writeString($SCHEMA_LAST_MODIFIED_BY, lastModifiedBy);
        serializer.writeTimestamp($SCHEMA_CREATED_AT, createdAt);
        serializer.writeString($SCHEMA_CREATED_BY, createdBy);
        serializer.writeMap($SCHEMA_DEPENDENCY_GRAPH, dependencyGraph, dependencyGraph.size(), SharedSerde.DependencyGraphSerializer.INSTANCE);
        if (dimensionType != null) {
            serializer.writeStruct($SCHEMA_DIMENSION_TYPE, dimensionType);
        }
        if (autocompleteFunctionName != null) {
            serializer.writeString($SCHEMA_AUTOCOMPLETE_FUNCTION_NAME, autocompleteFunctionName);
        }
        serializer.writeBoolean($SCHEMA_MANDATORY, mandatory);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_DIMENSION, member, dimension);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_POSITION, member, position);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_SCHEMA_MEMBER, member, schemaMember);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, description);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, changeReason);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_AT, member, lastModifiedAt);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_BY, member, lastModifiedBy);
            case 7 -> (T) SchemaUtils.validateSameMember($SCHEMA_CREATED_AT, member, createdAt);
            case 8 -> (T) SchemaUtils.validateSameMember($SCHEMA_CREATED_BY, member, createdBy);
            case 9 -> (T) SchemaUtils.validateSameMember($SCHEMA_DEPENDENCY_GRAPH, member, dependencyGraph);
            case 10 -> (T) SchemaUtils.validateSameMember($SCHEMA_DIMENSION_TYPE, member, dimensionType);
            case 11 -> (T) SchemaUtils.validateSameMember($SCHEMA_MANDATORY, member, mandatory);
            case 12 -> (T) SchemaUtils.validateSameMember($SCHEMA_FUNCTION_NAME, member, functionName);
            case 13 -> (T) SchemaUtils.validateSameMember($SCHEMA_AUTOCOMPLETE_FUNCTION_NAME, member, autocompleteFunctionName);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link UpdateDimensionOutput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.dimension(this.dimension);
        builder.position(this.position);
        builder.schemaMember(this.schemaMember);
        builder.functionName(this.functionName);
        builder.description(this.description);
        builder.changeReason(this.changeReason);
        builder.lastModifiedAt(this.lastModifiedAt);
        builder.lastModifiedBy(this.lastModifiedBy);
        builder.createdAt(this.createdAt);
        builder.createdBy(this.createdBy);
        builder.dependencyGraph(this.dependencyGraph);
        builder.dimensionType(this.dimensionType);
        builder.autocompleteFunctionName(this.autocompleteFunctionName);
        builder.mandatory(this.mandatory);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link UpdateDimensionOutput}.
     */
    public static final class Builder implements ShapeBuilder<UpdateDimensionOutput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String dimension;
        private int position;
        private Map<String, Document> schemaMember;
        private String functionName;
        private String description;
        private String changeReason;
        private Instant lastModifiedAt;
        private String lastModifiedBy;
        private Instant createdAt;
        private String createdBy;
        private Map<String, List<String>> dependencyGraph;
        private DimensionType dimensionType;
        private String autocompleteFunctionName;
        private boolean mandatory;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
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
        public Builder schemaMember(Map<String, Document> schemaMember) {
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

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder lastModifiedAt(Instant lastModifiedAt) {
            this.lastModifiedAt = Objects.requireNonNull(lastModifiedAt, "lastModifiedAt cannot be null");
            tracker.setMember($SCHEMA_LAST_MODIFIED_AT);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder lastModifiedBy(String lastModifiedBy) {
            this.lastModifiedBy = Objects.requireNonNull(lastModifiedBy, "lastModifiedBy cannot be null");
            tracker.setMember($SCHEMA_LAST_MODIFIED_BY);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder createdAt(Instant createdAt) {
            this.createdAt = Objects.requireNonNull(createdAt, "createdAt cannot be null");
            tracker.setMember($SCHEMA_CREATED_AT);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder createdBy(String createdBy) {
            this.createdBy = Objects.requireNonNull(createdBy, "createdBy cannot be null");
            tracker.setMember($SCHEMA_CREATED_BY);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder dependencyGraph(Map<String, List<String>> dependencyGraph) {
            this.dependencyGraph = Objects.requireNonNull(dependencyGraph, "dependencyGraph cannot be null");
            tracker.setMember($SCHEMA_DEPENDENCY_GRAPH);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder dimensionType(DimensionType dimensionType) {
            this.dimensionType = Objects.requireNonNull(dimensionType, "dimensionType cannot be null");
            tracker.setMember($SCHEMA_DIMENSION_TYPE);
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder autocompleteFunctionName(String autocompleteFunctionName) {
            this.autocompleteFunctionName = autocompleteFunctionName;
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder mandatory(boolean mandatory) {
            this.mandatory = mandatory;
            tracker.setMember($SCHEMA_MANDATORY);
            return this;
        }

        @Override
        public UpdateDimensionOutput build() {
            tracker.validate();
            return new UpdateDimensionOutput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> dimension((String) SchemaUtils.validateSameMember($SCHEMA_DIMENSION, member, value));
                case 1 -> position((int) SchemaUtils.validateSameMember($SCHEMA_POSITION, member, value));
                case 2 -> schemaMember((Map<String, Document>) SchemaUtils.validateSameMember($SCHEMA_SCHEMA_MEMBER, member, value));
                case 3 -> description((String) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, value));
                case 4 -> changeReason((String) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, value));
                case 5 -> lastModifiedAt((Instant) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_AT, member, value));
                case 6 -> lastModifiedBy((String) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_BY, member, value));
                case 7 -> createdAt((Instant) SchemaUtils.validateSameMember($SCHEMA_CREATED_AT, member, value));
                case 8 -> createdBy((String) SchemaUtils.validateSameMember($SCHEMA_CREATED_BY, member, value));
                case 9 -> dependencyGraph((Map<String, List<String>>) SchemaUtils.validateSameMember($SCHEMA_DEPENDENCY_GRAPH, member, value));
                case 10 -> dimensionType((DimensionType) SchemaUtils.validateSameMember($SCHEMA_DIMENSION_TYPE, member, value));
                case 11 -> mandatory((boolean) SchemaUtils.validateSameMember($SCHEMA_MANDATORY, member, value));
                case 12 -> functionName((String) SchemaUtils.validateSameMember($SCHEMA_FUNCTION_NAME, member, value));
                case 13 -> autocompleteFunctionName((String) SchemaUtils.validateSameMember($SCHEMA_AUTOCOMPLETE_FUNCTION_NAME, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<UpdateDimensionOutput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_DIMENSION)) {
                dimension("");
            }
            if (!tracker.checkMember($SCHEMA_POSITION)) {
                tracker.setMember($SCHEMA_POSITION);
            }
            if (!tracker.checkMember($SCHEMA_SCHEMA_MEMBER)) {
                schemaMember(Collections.emptyMap());
            }
            if (!tracker.checkMember($SCHEMA_DESCRIPTION)) {
                description("");
            }
            if (!tracker.checkMember($SCHEMA_CHANGE_REASON)) {
                changeReason("");
            }
            if (!tracker.checkMember($SCHEMA_LAST_MODIFIED_AT)) {
                lastModifiedAt(Instant.EPOCH);
            }
            if (!tracker.checkMember($SCHEMA_LAST_MODIFIED_BY)) {
                lastModifiedBy("");
            }
            if (!tracker.checkMember($SCHEMA_CREATED_AT)) {
                createdAt(Instant.EPOCH);
            }
            if (!tracker.checkMember($SCHEMA_CREATED_BY)) {
                createdBy("");
            }
            if (!tracker.checkMember($SCHEMA_DEPENDENCY_GRAPH)) {
                dependencyGraph(Collections.emptyMap());
            }
            if (!tracker.checkMember($SCHEMA_DIMENSION_TYPE)) {
                tracker.setMember($SCHEMA_DIMENSION_TYPE);
            }
            if (!tracker.checkMember($SCHEMA_MANDATORY)) {
                tracker.setMember($SCHEMA_MANDATORY);
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
                    case 0 -> builder.dimension(de.readString(member));
                    case 1 -> builder.position(de.readInteger(member));
                    case 2 -> builder.schemaMember(SharedSerde.deserializeObjectShape(member, de));
                    case 3 -> builder.description(de.readString(member));
                    case 4 -> builder.changeReason(de.readString(member));
                    case 5 -> builder.lastModifiedAt(de.readTimestamp(member));
                    case 6 -> builder.lastModifiedBy(de.readString(member));
                    case 7 -> builder.createdAt(de.readTimestamp(member));
                    case 8 -> builder.createdBy(de.readString(member));
                    case 9 -> builder.dependencyGraph(SharedSerde.deserializeDependencyGraph(member, de));
                    case 10 -> builder.dimensionType(DimensionType.builder().deserializeMember(de, member).build());
                    case 11 -> builder.mandatory(de.readBoolean(member));
                    case 12 -> builder.functionName(de.readString(member));
                    case 13 -> builder.autocompleteFunctionName(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

