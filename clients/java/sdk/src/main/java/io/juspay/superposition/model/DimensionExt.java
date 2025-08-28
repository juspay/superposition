
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
public final class DimensionExt implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#DimensionExt");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("dimension", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("position", PreludeSchemas.INTEGER,
                new RequiredTrait())
        .putMember("schema", PreludeSchemas.DOCUMENT,
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
        .putMember("dependencies", SharedSchemas.DEPENDENCIES,
                new RequiredTrait())
        .putMember("dependents", SharedSchemas.DEPENDENTS,
                new RequiredTrait())
        .putMember("dependency_graph", SharedSchemas.OBJECT,
                new RequiredTrait())
        .putMember("autocomplete_function_name", PreludeSchemas.STRING)
        .putMember("dimension_type", DimensionType.$SCHEMA,
                new RequiredTrait())
        .putMember("cohort_based_on", PreludeSchemas.STRING)
        .putMember("mandatory", PreludeSchemas.BOOLEAN)
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
    private static final Schema $SCHEMA_DEPENDENCIES = $SCHEMA.member("dependencies");
    private static final Schema $SCHEMA_DEPENDENTS = $SCHEMA.member("dependents");
    private static final Schema $SCHEMA_DEPENDENCY_GRAPH = $SCHEMA.member("dependency_graph");
    private static final Schema $SCHEMA_AUTOCOMPLETE_FUNCTION_NAME = $SCHEMA.member("autocomplete_function_name");
    private static final Schema $SCHEMA_DIMENSION_TYPE = $SCHEMA.member("dimension_type");
    private static final Schema $SCHEMA_COHORT_BASED_ON = $SCHEMA.member("cohort_based_on");
    private static final Schema $SCHEMA_MANDATORY = $SCHEMA.member("mandatory");

    private final transient String dimension;
    private final transient int position;
    private final transient Document schemaMember;
    private final transient String functionName;
    private final transient String description;
    private final transient String changeReason;
    private final transient Instant lastModifiedAt;
    private final transient String lastModifiedBy;
    private final transient Instant createdAt;
    private final transient String createdBy;
    private final transient List<String> dependencies;
    private final transient List<String> dependents;
    private final transient Map<String, Document> dependencyGraph;
    private final transient String autocompleteFunctionName;
    private final transient DimensionType dimensionType;
    private final transient String cohortBasedOn;
    private final transient Boolean mandatory;

    private DimensionExt(Builder builder) {
        this.dimension = builder.dimension;
        this.position = builder.position;
        this.schemaMember = builder.schemaMember;
        this.functionName = builder.functionName;
        this.description = builder.description;
        this.changeReason = builder.changeReason;
        this.lastModifiedAt = builder.lastModifiedAt;
        this.lastModifiedBy = builder.lastModifiedBy;
        this.createdAt = builder.createdAt;
        this.createdBy = builder.createdBy;
        this.dependencies = Collections.unmodifiableList(builder.dependencies);
        this.dependents = Collections.unmodifiableList(builder.dependents);
        this.dependencyGraph = Collections.unmodifiableMap(builder.dependencyGraph);
        this.autocompleteFunctionName = builder.autocompleteFunctionName;
        this.dimensionType = builder.dimensionType;
        this.cohortBasedOn = builder.cohortBasedOn;
        this.mandatory = builder.mandatory;
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

    public List<String> dependencies() {
        return dependencies;
    }

    public boolean hasDependencies() {
        return true;
    }

    public List<String> dependents() {
        return dependents;
    }

    public boolean hasDependents() {
        return true;
    }

    public Map<String, Document> dependencyGraph() {
        return dependencyGraph;
    }

    public boolean hasDependencyGraph() {
        return true;
    }

    public String autocompleteFunctionName() {
        return autocompleteFunctionName;
    }

    public DimensionType dimensionType() {
        return dimensionType;
    }

    public String cohortBasedOn() {
        return cohortBasedOn;
    }

    public Boolean mandatory() {
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
        DimensionExt that = (DimensionExt) other;
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
               && Objects.equals(this.dependencies, that.dependencies)
               && Objects.equals(this.dependents, that.dependents)
               && Objects.equals(this.dependencyGraph, that.dependencyGraph)
               && Objects.equals(this.autocompleteFunctionName, that.autocompleteFunctionName)
               && Objects.equals(this.dimensionType, that.dimensionType)
               && Objects.equals(this.cohortBasedOn, that.cohortBasedOn)
               && Objects.equals(this.mandatory, that.mandatory);
    }

    @Override
    public int hashCode() {
        return Objects.hash(dimension, position, schemaMember, functionName, description, changeReason, lastModifiedAt, lastModifiedBy, createdAt, createdBy, dependencies, dependents, dependencyGraph, autocompleteFunctionName, dimensionType, cohortBasedOn, mandatory);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_DIMENSION, dimension);
        serializer.writeInteger($SCHEMA_POSITION, position);
        serializer.writeDocument($SCHEMA_SCHEMA_MEMBER, schemaMember);
        if (functionName != null) {
            serializer.writeString($SCHEMA_FUNCTION_NAME, functionName);
        }
        serializer.writeString($SCHEMA_DESCRIPTION, description);
        serializer.writeString($SCHEMA_CHANGE_REASON, changeReason);
        serializer.writeTimestamp($SCHEMA_LAST_MODIFIED_AT, lastModifiedAt);
        serializer.writeString($SCHEMA_LAST_MODIFIED_BY, lastModifiedBy);
        serializer.writeTimestamp($SCHEMA_CREATED_AT, createdAt);
        serializer.writeString($SCHEMA_CREATED_BY, createdBy);
        serializer.writeList($SCHEMA_DEPENDENCIES, dependencies, dependencies.size(), SharedSerde.DependenciesSerializer.INSTANCE);
        serializer.writeList($SCHEMA_DEPENDENTS, dependents, dependents.size(), SharedSerde.DependentsSerializer.INSTANCE);
        serializer.writeMap($SCHEMA_DEPENDENCY_GRAPH, dependencyGraph, dependencyGraph.size(), SharedSerde.ObjectShapeSerializer.INSTANCE);
        if (autocompleteFunctionName != null) {
            serializer.writeString($SCHEMA_AUTOCOMPLETE_FUNCTION_NAME, autocompleteFunctionName);
        }
        serializer.writeString($SCHEMA_DIMENSION_TYPE, dimensionType.value());
        if (cohortBasedOn != null) {
            serializer.writeString($SCHEMA_COHORT_BASED_ON, cohortBasedOn);
        }
        if (mandatory != null) {
            serializer.writeBoolean($SCHEMA_MANDATORY, mandatory);
        }
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
            case 9 -> (T) SchemaUtils.validateSameMember($SCHEMA_DEPENDENCIES, member, dependencies);
            case 10 -> (T) SchemaUtils.validateSameMember($SCHEMA_DEPENDENTS, member, dependents);
            case 11 -> (T) SchemaUtils.validateSameMember($SCHEMA_DEPENDENCY_GRAPH, member, dependencyGraph);
            case 12 -> (T) SchemaUtils.validateSameMember($SCHEMA_DIMENSION_TYPE, member, dimensionType);
            case 13 -> (T) SchemaUtils.validateSameMember($SCHEMA_FUNCTION_NAME, member, functionName);
            case 14 -> (T) SchemaUtils.validateSameMember($SCHEMA_AUTOCOMPLETE_FUNCTION_NAME, member, autocompleteFunctionName);
            case 15 -> (T) SchemaUtils.validateSameMember($SCHEMA_COHORT_BASED_ON, member, cohortBasedOn);
            case 16 -> (T) SchemaUtils.validateSameMember($SCHEMA_MANDATORY, member, mandatory);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link DimensionExt}.
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
        builder.dependencies(this.dependencies);
        builder.dependents(this.dependents);
        builder.dependencyGraph(this.dependencyGraph);
        builder.autocompleteFunctionName(this.autocompleteFunctionName);
        builder.dimensionType(this.dimensionType);
        builder.cohortBasedOn(this.cohortBasedOn);
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
     * Builder for {@link DimensionExt}.
     */
    public static final class Builder implements ShapeBuilder<DimensionExt> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String dimension;
        private int position;
        private Document schemaMember;
        private String functionName;
        private String description;
        private String changeReason;
        private Instant lastModifiedAt;
        private String lastModifiedBy;
        private Instant createdAt;
        private String createdBy;
        private List<String> dependencies;
        private List<String> dependents;
        private Map<String, Document> dependencyGraph;
        private String autocompleteFunctionName;
        private DimensionType dimensionType;
        private String cohortBasedOn;
        private Boolean mandatory;

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
        public Builder dependencies(List<String> dependencies) {
            this.dependencies = Objects.requireNonNull(dependencies, "dependencies cannot be null");
            tracker.setMember($SCHEMA_DEPENDENCIES);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder dependents(List<String> dependents) {
            this.dependents = Objects.requireNonNull(dependents, "dependents cannot be null");
            tracker.setMember($SCHEMA_DEPENDENTS);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder dependencyGraph(Map<String, Document> dependencyGraph) {
            this.dependencyGraph = Objects.requireNonNull(dependencyGraph, "dependencyGraph cannot be null");
            tracker.setMember($SCHEMA_DEPENDENCY_GRAPH);
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
        public Builder dimensionType(DimensionType dimensionType) {
            this.dimensionType = Objects.requireNonNull(dimensionType, "dimensionType cannot be null");
            tracker.setMember($SCHEMA_DIMENSION_TYPE);
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder cohortBasedOn(String cohortBasedOn) {
            this.cohortBasedOn = cohortBasedOn;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder mandatory(boolean mandatory) {
            this.mandatory = mandatory;
            return this;
        }

        @Override
        public DimensionExt build() {
            tracker.validate();
            return new DimensionExt(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> dimension((String) SchemaUtils.validateSameMember($SCHEMA_DIMENSION, member, value));
                case 1 -> position((int) SchemaUtils.validateSameMember($SCHEMA_POSITION, member, value));
                case 2 -> schemaMember((Document) SchemaUtils.validateSameMember($SCHEMA_SCHEMA_MEMBER, member, value));
                case 3 -> description((String) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, value));
                case 4 -> changeReason((String) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, value));
                case 5 -> lastModifiedAt((Instant) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_AT, member, value));
                case 6 -> lastModifiedBy((String) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_BY, member, value));
                case 7 -> createdAt((Instant) SchemaUtils.validateSameMember($SCHEMA_CREATED_AT, member, value));
                case 8 -> createdBy((String) SchemaUtils.validateSameMember($SCHEMA_CREATED_BY, member, value));
                case 9 -> dependencies((List<String>) SchemaUtils.validateSameMember($SCHEMA_DEPENDENCIES, member, value));
                case 10 -> dependents((List<String>) SchemaUtils.validateSameMember($SCHEMA_DEPENDENTS, member, value));
                case 11 -> dependencyGraph((Map<String, Document>) SchemaUtils.validateSameMember($SCHEMA_DEPENDENCY_GRAPH, member, value));
                case 12 -> dimensionType((DimensionType) SchemaUtils.validateSameMember($SCHEMA_DIMENSION_TYPE, member, value));
                case 13 -> functionName((String) SchemaUtils.validateSameMember($SCHEMA_FUNCTION_NAME, member, value));
                case 14 -> autocompleteFunctionName((String) SchemaUtils.validateSameMember($SCHEMA_AUTOCOMPLETE_FUNCTION_NAME, member, value));
                case 15 -> cohortBasedOn((String) SchemaUtils.validateSameMember($SCHEMA_COHORT_BASED_ON, member, value));
                case 16 -> mandatory((boolean) SchemaUtils.validateSameMember($SCHEMA_MANDATORY, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<DimensionExt> errorCorrection() {
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
                tracker.setMember($SCHEMA_SCHEMA_MEMBER);
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
            if (!tracker.checkMember($SCHEMA_DEPENDENCIES)) {
                dependencies(Collections.emptyList());
            }
            if (!tracker.checkMember($SCHEMA_DEPENDENTS)) {
                dependents(Collections.emptyList());
            }
            if (!tracker.checkMember($SCHEMA_DEPENDENCY_GRAPH)) {
                dependencyGraph(Collections.emptyMap());
            }
            if (!tracker.checkMember($SCHEMA_DIMENSION_TYPE)) {
                dimensionType(DimensionType.unknown(""));
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
                    case 2 -> builder.schemaMember(de.readDocument());
                    case 3 -> builder.description(de.readString(member));
                    case 4 -> builder.changeReason(de.readString(member));
                    case 5 -> builder.lastModifiedAt(de.readTimestamp(member));
                    case 6 -> builder.lastModifiedBy(de.readString(member));
                    case 7 -> builder.createdAt(de.readTimestamp(member));
                    case 8 -> builder.createdBy(de.readString(member));
                    case 9 -> builder.dependencies(SharedSerde.deserializeDependencies(member, de));
                    case 10 -> builder.dependents(SharedSerde.deserializeDependents(member, de));
                    case 11 -> builder.dependencyGraph(SharedSerde.deserializeObjectShape(member, de));
                    case 12 -> builder.dimensionType(DimensionType.builder().deserializeMember(de, member).build());
                    case 13 -> builder.functionName(de.readString(member));
                    case 14 -> builder.autocompleteFunctionName(de.readString(member));
                    case 15 -> builder.cohortBasedOn(de.readString(member));
                    case 16 -> builder.mandatory(de.readBoolean(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

