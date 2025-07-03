
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
import software.amazon.smithy.model.node.Node;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.DefaultTrait;
import software.amazon.smithy.model.traits.HttpHeaderTrait;
import software.amazon.smithy.model.traits.HttpLabelTrait;
import software.amazon.smithy.model.traits.HttpPayloadTrait;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class TestInput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#TestInput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("workspace_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-tenant"),
                new RequiredTrait())
        .putMember("org_id", PreludeSchemas.STRING,
                new DefaultTrait(Node.from("juspay")),
                new RequiredTrait(),
                new HttpHeaderTrait("x-org-id"))
        .putMember("function_name", PreludeSchemas.STRING,
                new HttpLabelTrait(),
                new RequiredTrait())
        .putMember("stage", Stage.$SCHEMA,
                new RequiredTrait(),
                new HttpLabelTrait())
        .putMember("request", FunctionExecutionRequest.$SCHEMA,
                new RequiredTrait(),
                new HttpPayloadTrait())
        .build();

    private static final Schema $SCHEMA_WORKSPACE_ID = $SCHEMA.member("workspace_id");
    private static final Schema $SCHEMA_ORG_ID = $SCHEMA.member("org_id");
    private static final Schema $SCHEMA_FUNCTION_NAME = $SCHEMA.member("function_name");
    private static final Schema $SCHEMA_STAGE = $SCHEMA.member("stage");
    private static final Schema $SCHEMA_REQUEST = $SCHEMA.member("request");

    private final transient String workspaceId;
    private final transient String orgId;
    private final transient String functionName;
    private final transient Stage stage;
    private final transient FunctionExecutionRequest request;

    private TestInput(Builder builder) {
        this.workspaceId = builder.workspaceId;
        this.orgId = builder.orgId;
        this.functionName = builder.functionName;
        this.stage = builder.stage;
        this.request = builder.request;
    }

    public String workspaceId() {
        return workspaceId;
    }

    public String orgId() {
        return orgId;
    }

    public String functionName() {
        return functionName;
    }

    public Stage stage() {
        return stage;
    }

    public FunctionExecutionRequest request() {
        return request;
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
        TestInput that = (TestInput) other;
        return Objects.equals(this.workspaceId, that.workspaceId)
               && Objects.equals(this.orgId, that.orgId)
               && Objects.equals(this.functionName, that.functionName)
               && Objects.equals(this.stage, that.stage)
               && Objects.equals(this.request, that.request);
    }

    @Override
    public int hashCode() {
        return Objects.hash(workspaceId, orgId, functionName, stage, request);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_WORKSPACE_ID, workspaceId);
        serializer.writeString($SCHEMA_ORG_ID, orgId);
        serializer.writeString($SCHEMA_FUNCTION_NAME, functionName);
        serializer.writeString($SCHEMA_STAGE, stage.value());
        if (request != null) {
            serializer.writeStruct($SCHEMA_REQUEST, request);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, workspaceId);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_FUNCTION_NAME, member, functionName);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_STAGE, member, stage);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_REQUEST, member, request);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, orgId);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link TestInput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.workspaceId(this.workspaceId);
        builder.orgId(this.orgId);
        builder.functionName(this.functionName);
        builder.stage(this.stage);
        builder.request(this.request);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link TestInput}.
     */
    public static final class Builder implements ShapeBuilder<TestInput> {
        private static final String ORG_ID_DEFAULT = "juspay";
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String workspaceId;
        private String orgId = ORG_ID_DEFAULT;
        private String functionName;
        private Stage stage;
        private FunctionExecutionRequest request;

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
        public Builder functionName(String functionName) {
            this.functionName = Objects.requireNonNull(functionName, "functionName cannot be null");
            tracker.setMember($SCHEMA_FUNCTION_NAME);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder stage(Stage stage) {
            this.stage = Objects.requireNonNull(stage, "stage cannot be null");
            tracker.setMember($SCHEMA_STAGE);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder request(FunctionExecutionRequest request) {
            this.request = Objects.requireNonNull(request, "request cannot be null");
            tracker.setMember($SCHEMA_REQUEST);
            return this;
        }

        @Override
        public TestInput build() {
            tracker.validate();
            return new TestInput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> workspaceId((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, value));
                case 1 -> functionName((String) SchemaUtils.validateSameMember($SCHEMA_FUNCTION_NAME, member, value));
                case 2 -> stage((Stage) SchemaUtils.validateSameMember($SCHEMA_STAGE, member, value));
                case 3 -> request((FunctionExecutionRequest) SchemaUtils.validateSameMember($SCHEMA_REQUEST, member, value));
                case 4 -> orgId((String) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<TestInput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_WORKSPACE_ID)) {
                workspaceId("");
            }
            if (!tracker.checkMember($SCHEMA_FUNCTION_NAME)) {
                functionName("");
            }
            if (!tracker.checkMember($SCHEMA_STAGE)) {
                stage(Stage.unknown(""));
            }
            if (!tracker.checkMember($SCHEMA_REQUEST)) {
                tracker.setMember($SCHEMA_REQUEST);
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
                    case 1 -> builder.functionName(de.readString(member));
                    case 2 -> builder.stage(Stage.builder().deserializeMember(de, member).build());
                    case 3 -> builder.request(FunctionExecutionRequest.builder().deserializeMember(de, member).build());
                    case 4 -> builder.orgId(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

