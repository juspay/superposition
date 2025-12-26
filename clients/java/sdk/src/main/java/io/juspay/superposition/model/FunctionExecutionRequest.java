
package io.juspay.superposition.model;

import java.util.Objects;
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
public abstract class FunctionExecutionRequest implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#FunctionExecutionRequest");

    public static final Schema $SCHEMA = Schema.unionBuilder($ID)
        .putMember("value_validate", ValueValidationFunctionRequest.$SCHEMA)
        .putMember("value_compute", ValueComputeFunctionRequest.$SCHEMA)
        .putMember("context_validate", ContextValidationFunctionRequest.$SCHEMA)
        .putMember("change_reason_validate", ChangeReasonValidationFunctionRequest.$SCHEMA)
        .build();

    private static final Schema $SCHEMA_VALUE_VALIDATE = $SCHEMA.member("value_validate");
    private static final Schema $SCHEMA_VALUE_COMPUTE = $SCHEMA.member("value_compute");
    private static final Schema $SCHEMA_CONTEXT_VALIDATE = $SCHEMA.member("context_validate");
    private static final Schema $SCHEMA_CHANGE_REASON_VALIDATE = $SCHEMA.member("change_reason_validate");

    private final Type type;

    private FunctionExecutionRequest(Type type) {
        this.type = type;
    }

    public Type type() {
        return type;
    }

    /**
     * Enum representing the possible variants of {@link FunctionExecutionRequest}.
     */
    public enum Type {
        $UNKNOWN,
        valueValidate,
        valueCompute,
        contextValidate,
        changeReasonValidate
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
    public static final class ValueValidateMember extends FunctionExecutionRequest {
        private final transient ValueValidationFunctionRequest value;

        public ValueValidateMember(ValueValidationFunctionRequest value) {
            super(Type.valueValidate);
            this.value = Objects.requireNonNull(value, "Union value cannot be null");
        }

        @Override
        public void serializeMembers(ShapeSerializer serializer) {
            serializer.writeStruct($SCHEMA_VALUE_VALIDATE, value);
        }

        public ValueValidationFunctionRequest valueValidate() {
            return value;
        }

        @Override
        @SuppressWarnings("unchecked")
        public <T> T getValue() {
            return (T) value;
        }
    }

    @SmithyGenerated
    public static final class ValueComputeMember extends FunctionExecutionRequest {
        private final transient ValueComputeFunctionRequest value;

        public ValueComputeMember(ValueComputeFunctionRequest value) {
            super(Type.valueCompute);
            this.value = Objects.requireNonNull(value, "Union value cannot be null");
        }

        @Override
        public void serializeMembers(ShapeSerializer serializer) {
            serializer.writeStruct($SCHEMA_VALUE_COMPUTE, value);
        }

        public ValueComputeFunctionRequest valueCompute() {
            return value;
        }

        @Override
        @SuppressWarnings("unchecked")
        public <T> T getValue() {
            return (T) value;
        }
    }

    @SmithyGenerated
    public static final class ContextValidateMember extends FunctionExecutionRequest {
        private final transient ContextValidationFunctionRequest value;

        public ContextValidateMember(ContextValidationFunctionRequest value) {
            super(Type.contextValidate);
            this.value = Objects.requireNonNull(value, "Union value cannot be null");
        }

        @Override
        public void serializeMembers(ShapeSerializer serializer) {
            serializer.writeStruct($SCHEMA_CONTEXT_VALIDATE, value);
        }

        public ContextValidationFunctionRequest contextValidate() {
            return value;
        }

        @Override
        @SuppressWarnings("unchecked")
        public <T> T getValue() {
            return (T) value;
        }
    }

    @SmithyGenerated
    public static final class ChangeReasonValidateMember extends FunctionExecutionRequest {
        private final transient ChangeReasonValidationFunctionRequest value;

        public ChangeReasonValidateMember(ChangeReasonValidationFunctionRequest value) {
            super(Type.changeReasonValidate);
            this.value = Objects.requireNonNull(value, "Union value cannot be null");
        }

        @Override
        public void serializeMembers(ShapeSerializer serializer) {
            serializer.writeStruct($SCHEMA_CHANGE_REASON_VALIDATE, value);
        }

        public ChangeReasonValidationFunctionRequest changeReasonValidate() {
            return value;
        }

        @Override
        @SuppressWarnings("unchecked")
        public <T> T getValue() {
            return (T) value;
        }
    }

    public static final class $UnknownMember extends FunctionExecutionRequest {
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
        return Objects.equals(getValue(), ((FunctionExecutionRequest) other).getValue());
    }

    public interface BuildStage {
        FunctionExecutionRequest build();
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link FunctionExecutionRequest}.
     */
    public static final class Builder implements ShapeBuilder<FunctionExecutionRequest>, BuildStage {
        private FunctionExecutionRequest value;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        public BuildStage valueValidate(ValueValidationFunctionRequest value) {
            return setValue(new ValueValidateMember(value));
        }

        public BuildStage valueCompute(ValueComputeFunctionRequest value) {
            return setValue(new ValueComputeMember(value));
        }

        public BuildStage contextValidate(ContextValidationFunctionRequest value) {
            return setValue(new ContextValidateMember(value));
        }

        public BuildStage changeReasonValidate(ChangeReasonValidationFunctionRequest value) {
            return setValue(new ChangeReasonValidateMember(value));
        }

        public BuildStage $unknownMember(String memberName) {
            return setValue(new $UnknownMember(memberName));
        }

        private BuildStage setValue(FunctionExecutionRequest value) {
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
        public FunctionExecutionRequest build() {
            return Objects.requireNonNull(value, "no union value set");
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> valueValidate((ValueValidationFunctionRequest) SchemaUtils.validateSameMember($SCHEMA_VALUE_VALIDATE, member, value));
                case 1 -> valueCompute((ValueComputeFunctionRequest) SchemaUtils.validateSameMember($SCHEMA_VALUE_COMPUTE, member, value));
                case 2 -> contextValidate((ContextValidationFunctionRequest) SchemaUtils.validateSameMember($SCHEMA_CONTEXT_VALIDATE, member, value));
                case 3 -> changeReasonValidate((ChangeReasonValidationFunctionRequest) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON_VALIDATE, member, value));
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
                    case 0 -> builder.valueValidate(ValueValidationFunctionRequest.builder().deserializeMember(de, member).build());
                    case 1 -> builder.valueCompute(ValueComputeFunctionRequest.builder().deserializeMember(de, member).build());
                    case 2 -> builder.contextValidate(ContextValidationFunctionRequest.builder().deserializeMember(de, member).build());
                    case 3 -> builder.changeReasonValidate(ChangeReasonValidationFunctionRequest.builder().deserializeMember(de, member).build());
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

