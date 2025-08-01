
package io.juspay.superposition.model;

import java.util.Objects;
import software.amazon.smithy.java.core.schema.PreludeSchemas;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.java.core.schema.SchemaUtils;
import software.amazon.smithy.java.core.schema.SerializableStruct;
import software.amazon.smithy.java.core.schema.ShapeBuilder;
import software.amazon.smithy.java.core.serde.ShapeDeserializer;
import software.amazon.smithy.java.core.serde.ShapeSerializer;
import software.amazon.smithy.java.core.serde.ToStringSerializer;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public abstract class ContextActionOut implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ContextActionOut");

    public static final Schema $SCHEMA = Schema.unionBuilder($ID)
        .putMember("PUT", ContextResponse.$SCHEMA)
        .putMember("REPLACE", ContextResponse.$SCHEMA)
        .putMember("DELETE", PreludeSchemas.STRING)
        .putMember("MOVE", ContextResponse.$SCHEMA)
        .build();

    private static final Schema $SCHEMA_PU_T = $SCHEMA.member("PUT");
    private static final Schema $SCHEMA_REPLAC_E = $SCHEMA.member("REPLACE");
    private static final Schema $SCHEMA_DELET_E = $SCHEMA.member("DELETE");
    private static final Schema $SCHEMA_MOV_E = $SCHEMA.member("MOVE");

    private final Type type;

    private ContextActionOut(Type type) {
        this.type = type;
    }

    public Type type() {
        return type;
    }

    /**
     * Enum representing the possible variants of {@link ContextActionOut}.
     */
    public enum Type {
        $UNKNOWN,
        puT,
        replacE,
        deletE,
        movE
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
    public static final class PuTMember extends ContextActionOut {
        private final transient ContextResponse value;

        public PuTMember(ContextResponse value) {
            super(Type.puT);
            this.value = Objects.requireNonNull(value, "Union value cannot be null");
        }

        @Override
        public void serializeMembers(ShapeSerializer serializer) {
            serializer.writeStruct($SCHEMA_PU_T, value);
        }

        public ContextResponse puT() {
            return value;
        }

        @Override
        @SuppressWarnings("unchecked")
        public <T> T getValue() {
            return (T) value;
        }
    }

    @SmithyGenerated
    public static final class ReplacEMember extends ContextActionOut {
        private final transient ContextResponse value;

        public ReplacEMember(ContextResponse value) {
            super(Type.replacE);
            this.value = Objects.requireNonNull(value, "Union value cannot be null");
        }

        @Override
        public void serializeMembers(ShapeSerializer serializer) {
            serializer.writeStruct($SCHEMA_REPLAC_E, value);
        }

        public ContextResponse replacE() {
            return value;
        }

        @Override
        @SuppressWarnings("unchecked")
        public <T> T getValue() {
            return (T) value;
        }
    }

    @SmithyGenerated
    public static final class DeletEMember extends ContextActionOut {
        private final transient String value;

        public DeletEMember(String value) {
            super(Type.deletE);
            this.value = Objects.requireNonNull(value, "Union value cannot be null");
        }

        @Override
        public void serializeMembers(ShapeSerializer serializer) {
            serializer.writeString($SCHEMA_DELET_E, value);
        }

        public String deletE() {
            return value;
        }

        @Override
        @SuppressWarnings("unchecked")
        public <T> T getValue() {
            return (T) value;
        }
    }

    @SmithyGenerated
    public static final class MovEMember extends ContextActionOut {
        private final transient ContextResponse value;

        public MovEMember(ContextResponse value) {
            super(Type.movE);
            this.value = Objects.requireNonNull(value, "Union value cannot be null");
        }

        @Override
        public void serializeMembers(ShapeSerializer serializer) {
            serializer.writeStruct($SCHEMA_MOV_E, value);
        }

        public ContextResponse movE() {
            return value;
        }

        @Override
        @SuppressWarnings("unchecked")
        public <T> T getValue() {
            return (T) value;
        }
    }

    public static final class $UnknownMember extends ContextActionOut {
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
        return Objects.equals(getValue(), ((ContextActionOut) other).getValue());
    }

    public interface BuildStage {
        ContextActionOut build();
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link ContextActionOut}.
     */
    public static final class Builder implements ShapeBuilder<ContextActionOut>, BuildStage {
        private ContextActionOut value;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        public BuildStage puT(ContextResponse value) {
            return setValue(new PuTMember(value));
        }

        public BuildStage replacE(ContextResponse value) {
            return setValue(new ReplacEMember(value));
        }

        public BuildStage deletE(String value) {
            return setValue(new DeletEMember(value));
        }

        public BuildStage movE(ContextResponse value) {
            return setValue(new MovEMember(value));
        }

        public BuildStage $unknownMember(String memberName) {
            return setValue(new $UnknownMember(memberName));
        }

        private BuildStage setValue(ContextActionOut value) {
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
        public ContextActionOut build() {
            return Objects.requireNonNull(value, "no union value set");
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> puT((ContextResponse) SchemaUtils.validateSameMember($SCHEMA_PU_T, member, value));
                case 1 -> replacE((ContextResponse) SchemaUtils.validateSameMember($SCHEMA_REPLAC_E, member, value));
                case 2 -> deletE((String) SchemaUtils.validateSameMember($SCHEMA_DELET_E, member, value));
                case 3 -> movE((ContextResponse) SchemaUtils.validateSameMember($SCHEMA_MOV_E, member, value));
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
                    case 0 -> builder.puT(ContextResponse.builder().deserializeMember(de, member).build());
                    case 1 -> builder.replacE(ContextResponse.builder().deserializeMember(de, member).build());
                    case 2 -> builder.deletE(de.readString(member));
                    case 3 -> builder.movE(ContextResponse.builder().deserializeMember(de, member).build());
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

