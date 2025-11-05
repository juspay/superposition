
package io.juspay.superposition.model;

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
public final class DimensionInfo implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#DimensionInfo");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("schema", SharedSchemas.OBJECT,
                new RequiredTrait())
        .putMember("position", PreludeSchemas.INTEGER,
                new RequiredTrait())
        .putMember("dimension_type", DimensionType.$SCHEMA,
                new RequiredTrait())
        .putMember("dependency_graph", SharedSchemas.DEPENDENCY_GRAPH,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_SCHEMA_MEMBER = $SCHEMA.member("schema");
    private static final Schema $SCHEMA_POSITION = $SCHEMA.member("position");
    private static final Schema $SCHEMA_DIMENSION_TYPE = $SCHEMA.member("dimension_type");
    private static final Schema $SCHEMA_DEPENDENCY_GRAPH = $SCHEMA.member("dependency_graph");

    private final transient Map<String, Document> schemaMember;
    private final transient int position;
    private final transient DimensionType dimensionType;
    private final transient Map<String, List<String>> dependencyGraph;

    private DimensionInfo(Builder builder) {
        this.schemaMember = Collections.unmodifiableMap(builder.schemaMember);
        this.position = builder.position;
        this.dimensionType = builder.dimensionType;
        this.dependencyGraph = Collections.unmodifiableMap(builder.dependencyGraph);
    }

    public Map<String, Document> schemaMember() {
        return schemaMember;
    }

    public boolean hasSchemaMember() {
        return true;
    }

    public int position() {
        return position;
    }

    public DimensionType dimensionType() {
        return dimensionType;
    }

    public Map<String, List<String>> dependencyGraph() {
        return dependencyGraph;
    }

    public boolean hasDependencyGraph() {
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
        DimensionInfo that = (DimensionInfo) other;
        return Objects.equals(this.schemaMember, that.schemaMember)
               && this.position == that.position
               && Objects.equals(this.dimensionType, that.dimensionType)
               && Objects.equals(this.dependencyGraph, that.dependencyGraph);
    }

    @Override
    public int hashCode() {
        return Objects.hash(schemaMember, position, dimensionType, dependencyGraph);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeMap($SCHEMA_SCHEMA_MEMBER, schemaMember, schemaMember.size(), SharedSerde.ObjectShapeSerializer.INSTANCE);
        serializer.writeInteger($SCHEMA_POSITION, position);
        if (dimensionType != null) {
            serializer.writeStruct($SCHEMA_DIMENSION_TYPE, dimensionType);
        }
        serializer.writeMap($SCHEMA_DEPENDENCY_GRAPH, dependencyGraph, dependencyGraph.size(), SharedSerde.DependencyGraphSerializer.INSTANCE);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_SCHEMA_MEMBER, member, schemaMember);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_POSITION, member, position);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_DIMENSION_TYPE, member, dimensionType);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_DEPENDENCY_GRAPH, member, dependencyGraph);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link DimensionInfo}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.schemaMember(this.schemaMember);
        builder.position(this.position);
        builder.dimensionType(this.dimensionType);
        builder.dependencyGraph(this.dependencyGraph);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link DimensionInfo}.
     */
    public static final class Builder implements ShapeBuilder<DimensionInfo> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private Map<String, Document> schemaMember;
        private int position;
        private DimensionType dimensionType;
        private Map<String, List<String>> dependencyGraph;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
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
        public Builder dimensionType(DimensionType dimensionType) {
            this.dimensionType = Objects.requireNonNull(dimensionType, "dimensionType cannot be null");
            tracker.setMember($SCHEMA_DIMENSION_TYPE);
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

        @Override
        public DimensionInfo build() {
            tracker.validate();
            return new DimensionInfo(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> schemaMember((Map<String, Document>) SchemaUtils.validateSameMember($SCHEMA_SCHEMA_MEMBER, member, value));
                case 1 -> position((int) SchemaUtils.validateSameMember($SCHEMA_POSITION, member, value));
                case 2 -> dimensionType((DimensionType) SchemaUtils.validateSameMember($SCHEMA_DIMENSION_TYPE, member, value));
                case 3 -> dependencyGraph((Map<String, List<String>>) SchemaUtils.validateSameMember($SCHEMA_DEPENDENCY_GRAPH, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<DimensionInfo> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_SCHEMA_MEMBER)) {
                schemaMember(Collections.emptyMap());
            }
            if (!tracker.checkMember($SCHEMA_POSITION)) {
                tracker.setMember($SCHEMA_POSITION);
            }
            if (!tracker.checkMember($SCHEMA_DIMENSION_TYPE)) {
                tracker.setMember($SCHEMA_DIMENSION_TYPE);
            }
            if (!tracker.checkMember($SCHEMA_DEPENDENCY_GRAPH)) {
                dependencyGraph(Collections.emptyMap());
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
                    case 0 -> builder.schemaMember(SharedSerde.deserializeObjectShape(member, de));
                    case 1 -> builder.position(de.readInteger(member));
                    case 2 -> builder.dimensionType(DimensionType.builder().deserializeMember(de, member).build());
                    case 3 -> builder.dependencyGraph(SharedSerde.deserializeDependencyGraph(member, de));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

