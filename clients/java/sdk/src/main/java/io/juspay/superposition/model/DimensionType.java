
package io.juspay.superposition.model;

import java.util.Objects;
import software.amazon.smithy.java.core.schema.PreludeSchemas;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.java.core.schema.SchemaUtils;
import software.amazon.smithy.java.core.schema.SerializableStruct;
import software.amazon.smithy.java.core.schema.ShapeBuilder;
import software.amazon.smithy.java.core.schema.Unit;
import software.amazon.smithy.java.core.serde.ShapeDeserializer;
import software.amazon.smithy.java.core.serde.ShapeSerializer;
import software.amazon.smithy.java.core.serde.ToStringSerializer;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public abstract class DimensionType implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#DimensionType");

    public static final Schema $SCHEMA = Schema.unionBuilder($ID)
        .putMember("REGULAR", Unit.SCHEMA)
        .putMember("LOCAL_COHORT", PreludeSchemas.STRING)
        .putMember("REMOTE_COHORT", PreludeSchemas.STRING)
        .build();

    private static final Schema $SCHEMA_REGULA_R = $SCHEMA.member("REGULAR");
    private static final Schema $SCHEMA_LOCAL_COHORT = $SCHEMA.member("LOCAL_COHORT");
    private static final Schema $SCHEMA_REMOTE_COHORT = $SCHEMA.member("REMOTE_COHORT");

    private final Type type;

    private DimensionType(Type type) {
        this.type = type;
    }

    public Type type() {
        return type;
    }

    /**
     * Enum representing the possible variants of {@link DimensionType}.
     */
    public enum Type {
        $UNKNOWN,
        regulaR,
        localCohort,
        remoteCohort
    }

    @Override
    public String toString() {
        return ToStringSerializer.serialize(this);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public <T> T getMemberValue(Schema member) {
        return SchemaUtils.validateMemberInSchema($SCHEMA, member, getValue());
    }

    public abstract <T> T getValue();

    @SmithyGenerated
    public static final class RegulaRMember extends DimensionType {

        public RegulaRMember() {
            super(Type.regulaR);
        }

        @Override
        public void serializeMembers(ShapeSerializer serializer) {
            serializer.writeStruct($SCHEMA_REGULA_R, Unit.getInstance());
        }

        @Override
        @SuppressWarnings("unchecked")
        public <T> T getValue() {
            return (T) null;
        }
    }

    @SmithyGenerated
    public static final class LocalCohortMember extends DimensionType {
        private final transient String value;

        public LocalCohortMember(String value) {
            super(Type.localCohort);
            this.value = Objects.requireNonNull(value, "Union value cannot be null");
        }

        @Override
        public void serializeMembers(ShapeSerializer serializer) {
            serializer.writeString($SCHEMA_LOCAL_COHORT, value);
        }

        public String localCohort() {
            return value;
        }

        @Override
        @SuppressWarnings("unchecked")
        public <T> T getValue() {
            return (T) value;
        }
    }

    @SmithyGenerated
    public static final class RemoteCohortMember extends DimensionType {
        private final transient String value;

        public RemoteCohortMember(String value) {
            super(Type.remoteCohort);
            this.value = Objects.requireNonNull(value, "Union value cannot be null");
        }

        @Override
        public void serializeMembers(ShapeSerializer serializer) {
            serializer.writeString($SCHEMA_REMOTE_COHORT, value);
        }

        public String remoteCohort() {
            return value;
        }

        @Override
        @SuppressWarnings("unchecked")
        public <T> T getValue() {
            return (T) value;
        }
    }

    public static final class $UnknownMember extends DimensionType {
        private final String memberName;

        public $UnknownMember(String memberName) {
            super(Type.$UNKNOWN);
            this.memberName = memberName;
        }

        public String memberName() {
            return memberName;
        }

        @Override
        public void serialize(ShapeSerializer serializer) {
            throw new UnsupportedOperationException("Cannot serialize union with unknown member " + this.memberName);
        }

        @Override
        public void serializeMembers(ShapeSerializer serializer) {}

        @Override
        @SuppressWarnings("unchecked")
        public <T> T getValue() {
            return (T) memberName;
        }
    }

    @Override
    public int hashCode() {
        return Objects.hash(type, getValue());
    }

    @Override
    public boolean equals(Object other) {
        if (other == this) {
            return true;
        }
        if (other == null || getClass() != other.getClass()) {
            return false;
        }
        return Objects.equals(getValue(), ((DimensionType) other).getValue());
    }

    public interface BuildStage {
        DimensionType build();
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link DimensionType}.
     */
    public static final class Builder implements ShapeBuilder<DimensionType>, BuildStage {
        private DimensionType value;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        public BuildStage regulaR(Unit value) {
            return setValue(new RegulaRMember());
        }

        public BuildStage localCohort(String value) {
            return setValue(new LocalCohortMember(value));
        }

        public BuildStage remoteCohort(String value) {
            return setValue(new RemoteCohortMember(value));
        }

        public BuildStage $unknownMember(String memberName) {
            return setValue(new $UnknownMember(memberName));
        }

        private BuildStage setValue(DimensionType value) {
            if (this.value != null) {
                if (this.value.type() == Type.$UNKNOWN) {
                    throw new IllegalArgumentException("Cannot change union from unknown to known variant");
                }
                throw new IllegalArgumentException("Only one value may be set for unions");
            }
            this.value = value;
            return this;
        }

        @Override
        public DimensionType build() {
            return Objects.requireNonNull(value, "no union value set");
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> regulaR((Unit) SchemaUtils.validateSameMember($SCHEMA_REGULA_R, member, value));
                case 1 -> localCohort((String) SchemaUtils.validateSameMember($SCHEMA_LOCAL_COHORT, member, value));
                case 2 -> remoteCohort((String) SchemaUtils.validateSameMember($SCHEMA_REMOTE_COHORT, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
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
                    case 0 -> builder.regulaR(Unit.builder().deserializeMember(de, member).build());
                    case 1 -> builder.localCohort(de.readString(member));
                    case 2 -> builder.remoteCohort(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }

            @Override
            public void unknownMember(Builder builder, String memberName) {
                builder.$unknownMember(memberName);
            }
        }
    }
}

