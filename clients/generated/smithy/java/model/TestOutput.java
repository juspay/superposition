
package io.juspay.superposition.model;

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
public final class TestOutput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#FunctionExecutionResponse");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("fn_output", PreludeSchemas.DOCUMENT,
                new RequiredTrait())
        .putMember("stdout", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("function_type", FunctionTypes.$SCHEMA,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_FN_OUTPUT = $SCHEMA.member("fn_output");
    private static final Schema $SCHEMA_STDOUT = $SCHEMA.member("stdout");
    private static final Schema $SCHEMA_FUNCTION_TYPE = $SCHEMA.member("function_type");

    private final transient Document fnOutput;
    private final transient String stdout;
    private final transient FunctionTypes functionType;

    private TestOutput(Builder builder) {
        this.fnOutput = builder.fnOutput;
        this.stdout = builder.stdout;
        this.functionType = builder.functionType;
    }

    public Document fnOutput() {
        return fnOutput;
    }

    public String stdout() {
        return stdout;
    }

    public FunctionTypes functionType() {
        return functionType;
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
        TestOutput that = (TestOutput) other;
        return Objects.equals(this.fnOutput, that.fnOutput)
               && Objects.equals(this.stdout, that.stdout)
               && Objects.equals(this.functionType, that.functionType);
    }

    @Override
    public int hashCode() {
        return Objects.hash(fnOutput, stdout, functionType);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeDocument($SCHEMA_FN_OUTPUT, fnOutput);
        serializer.writeString($SCHEMA_STDOUT, stdout);
        serializer.writeString($SCHEMA_FUNCTION_TYPE, functionType.value());
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_FN_OUTPUT, member, fnOutput);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_STDOUT, member, stdout);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_FUNCTION_TYPE, member, functionType);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link TestOutput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.fnOutput(this.fnOutput);
        builder.stdout(this.stdout);
        builder.functionType(this.functionType);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link TestOutput}.
     */
    public static final class Builder implements ShapeBuilder<TestOutput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private Document fnOutput;
        private String stdout;
        private FunctionTypes functionType;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder fnOutput(Document fnOutput) {
            this.fnOutput = Objects.requireNonNull(fnOutput, "fnOutput cannot be null");
            tracker.setMember($SCHEMA_FN_OUTPUT);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder stdout(String stdout) {
            this.stdout = Objects.requireNonNull(stdout, "stdout cannot be null");
            tracker.setMember($SCHEMA_STDOUT);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder functionType(FunctionTypes functionType) {
            this.functionType = Objects.requireNonNull(functionType, "functionType cannot be null");
            tracker.setMember($SCHEMA_FUNCTION_TYPE);
            return this;
        }

        @Override
        public TestOutput build() {
            tracker.validate();
            return new TestOutput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> fnOutput((Document) SchemaUtils.validateSameMember($SCHEMA_FN_OUTPUT, member, value));
                case 1 -> stdout((String) SchemaUtils.validateSameMember($SCHEMA_STDOUT, member, value));
                case 2 -> functionType((FunctionTypes) SchemaUtils.validateSameMember($SCHEMA_FUNCTION_TYPE, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<TestOutput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_FN_OUTPUT)) {
                tracker.setMember($SCHEMA_FN_OUTPUT);
            }
            if (!tracker.checkMember($SCHEMA_STDOUT)) {
                stdout("");
            }
            if (!tracker.checkMember($SCHEMA_FUNCTION_TYPE)) {
                functionType(FunctionTypes.unknown(""));
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
                    case 0 -> builder.fnOutput(de.readDocument());
                    case 1 -> builder.stdout(de.readString(member));
                    case 2 -> builder.functionType(FunctionTypes.builder().deserializeMember(de, member).build());
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

