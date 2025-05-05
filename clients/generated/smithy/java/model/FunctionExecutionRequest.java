
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
        .putMember("ValidateFunctionRequest", ValidateFunctionRequest.$SCHEMA)
        .putMember("AutocompleteFunctionRequest", AutocompleteFunctionRequest.$SCHEMA)
        .build();

    private static final Schema $SCHEMA_VALIDATE_FUNCTION_REQUEST = $SCHEMA.member("ValidateFunctionRequest");
    private static final Schema $SCHEMA_AUTOCOMPLETE_FUNCTION_REQUEST = $SCHEMA.member("AutocompleteFunctionRequest");

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
        validateFunctionRequest,
        autocompleteFunctionRequest
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
    public static final class ValidateFunctionRequestMember extends FunctionExecutionRequest {
        private final transient ValidateFunctionRequest value;

        public ValidateFunctionRequestMember(ValidateFunctionRequest value) {
            super(Type.validateFunctionRequest);
            this.value = Objects.requireNonNull(value, "Union value cannot be null");
        }

        @Override
        public void serializeMembers(ShapeSerializer serializer) {
            serializer.writeStruct($SCHEMA_VALIDATE_FUNCTION_REQUEST, value);
        }

        public ValidateFunctionRequest validateFunctionRequest() {
            return value;
        }

        @Override
        @SuppressWarnings("unchecked")
        public <T> T getValue() {
            return (T) value;
        }
    }

    @SmithyGenerated
    public static final class AutocompleteFunctionRequestMember extends FunctionExecutionRequest {
        private final transient AutocompleteFunctionRequest value;

        public AutocompleteFunctionRequestMember(AutocompleteFunctionRequest value) {
            super(Type.autocompleteFunctionRequest);
            this.value = Objects.requireNonNull(value, "Union value cannot be null");
        }

        @Override
        public void serializeMembers(ShapeSerializer serializer) {
            serializer.writeStruct($SCHEMA_AUTOCOMPLETE_FUNCTION_REQUEST, value);
        }

        public AutocompleteFunctionRequest autocompleteFunctionRequest() {
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

        public BuildStage validateFunctionRequest(ValidateFunctionRequest value) {
            return setValue(new ValidateFunctionRequestMember(value));
        }

        public BuildStage autocompleteFunctionRequest(AutocompleteFunctionRequest value) {
            return setValue(new AutocompleteFunctionRequestMember(value));
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
                case 0 -> validateFunctionRequest((ValidateFunctionRequest) SchemaUtils.validateSameMember($SCHEMA_VALIDATE_FUNCTION_REQUEST, member, value));
                case 1 -> autocompleteFunctionRequest((AutocompleteFunctionRequest) SchemaUtils.validateSameMember($SCHEMA_AUTOCOMPLETE_FUNCTION_REQUEST, member, value));
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
                    case 0 -> builder.validateFunctionRequest(ValidateFunctionRequest.builder().deserializeMember(de, member).build());
                    case 1 -> builder.autocompleteFunctionRequest(AutocompleteFunctionRequest.builder().deserializeMember(de, member).build());
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

