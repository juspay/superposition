
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
        .putMember("ValueValidationFunctionRequest", ValueValidationFunctionRequest.$SCHEMA)
        .putMember("ValueComputeFunctionRequest", ValueComputeFunctionRequest.$SCHEMA)
        .putMember("ContextValidationFunctionRequest", ContextValidationFunctionRequest.$SCHEMA)
        .build();

    private static final Schema $SCHEMA_VALUE_VALIDATION_FUNCTION_REQUEST = $SCHEMA.member("ValueValidationFunctionRequest");
    private static final Schema $SCHEMA_VALUE_COMPUTE_FUNCTION_REQUEST = $SCHEMA.member("ValueComputeFunctionRequest");
    private static final Schema $SCHEMA_CONTEXT_VALIDATION_FUNCTION_REQUEST = $SCHEMA.member("ContextValidationFunctionRequest");

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
        valueValidationFunctionRequest,
        valueComputeFunctionRequest,
        contextValidationFunctionRequest
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
    public static final class ValueValidationFunctionRequestMember extends FunctionExecutionRequest {
        private final transient ValueValidationFunctionRequest value;

        public ValueValidationFunctionRequestMember(ValueValidationFunctionRequest value) {
            super(Type.valueValidationFunctionRequest);
            this.value = Objects.requireNonNull(value, "Union value cannot be null");
        }

        @Override
        public void serializeMembers(ShapeSerializer serializer) {
            serializer.writeStruct($SCHEMA_VALUE_VALIDATION_FUNCTION_REQUEST, value);
        }

        public ValueValidationFunctionRequest valueValidationFunctionRequest() {
            return value;
        }

        @Override
        @SuppressWarnings("unchecked")
        public <T> T getValue() {
            return (T) value;
        }
    }

    @SmithyGenerated
    public static final class ValueComputeFunctionRequestMember extends FunctionExecutionRequest {
        private final transient ValueComputeFunctionRequest value;

        public ValueComputeFunctionRequestMember(ValueComputeFunctionRequest value) {
            super(Type.valueComputeFunctionRequest);
            this.value = Objects.requireNonNull(value, "Union value cannot be null");
        }

        @Override
        public void serializeMembers(ShapeSerializer serializer) {
            serializer.writeStruct($SCHEMA_VALUE_COMPUTE_FUNCTION_REQUEST, value);
        }

        public ValueComputeFunctionRequest valueComputeFunctionRequest() {
            return value;
        }

        @Override
        @SuppressWarnings("unchecked")
        public <T> T getValue() {
            return (T) value;
        }
    }

    @SmithyGenerated
    public static final class ContextValidationFunctionRequestMember extends FunctionExecutionRequest {
        private final transient ContextValidationFunctionRequest value;

        public ContextValidationFunctionRequestMember(ContextValidationFunctionRequest value) {
            super(Type.contextValidationFunctionRequest);
            this.value = Objects.requireNonNull(value, "Union value cannot be null");
        }

        @Override
        public void serializeMembers(ShapeSerializer serializer) {
            serializer.writeStruct($SCHEMA_CONTEXT_VALIDATION_FUNCTION_REQUEST, value);
        }

        public ContextValidationFunctionRequest contextValidationFunctionRequest() {
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

        public BuildStage valueValidationFunctionRequest(ValueValidationFunctionRequest value) {
            return setValue(new ValueValidationFunctionRequestMember(value));
        }

        public BuildStage valueComputeFunctionRequest(ValueComputeFunctionRequest value) {
            return setValue(new ValueComputeFunctionRequestMember(value));
        }

        public BuildStage contextValidationFunctionRequest(ContextValidationFunctionRequest value) {
            return setValue(new ContextValidationFunctionRequestMember(value));
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
                case 0 -> valueValidationFunctionRequest((ValueValidationFunctionRequest) SchemaUtils.validateSameMember($SCHEMA_VALUE_VALIDATION_FUNCTION_REQUEST, member, value));
                case 1 -> valueComputeFunctionRequest((ValueComputeFunctionRequest) SchemaUtils.validateSameMember($SCHEMA_VALUE_COMPUTE_FUNCTION_REQUEST, member, value));
                case 2 -> contextValidationFunctionRequest((ContextValidationFunctionRequest) SchemaUtils.validateSameMember($SCHEMA_CONTEXT_VALIDATION_FUNCTION_REQUEST, member, value));
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
                    case 0 -> builder.valueValidationFunctionRequest(ValueValidationFunctionRequest.builder().deserializeMember(de, member).build());
                    case 1 -> builder.valueComputeFunctionRequest(ValueComputeFunctionRequest.builder().deserializeMember(de, member).build());
                    case 2 -> builder.contextValidationFunctionRequest(ContextValidationFunctionRequest.builder().deserializeMember(de, member).build());
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

