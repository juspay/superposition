
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
public abstract class GroupedDefaultConfig implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#GroupedDefaultConfig");

    public static final Schema $SCHEMA = Schema.unionBuilder($ID)
        .putMember("Group", PreludeSchemas.STRING)
        .putMember("Config", DefaultConfigResponse.$SCHEMA)
        .build();

    private static final Schema $SCHEMA_GROUP = $SCHEMA.member("Group");
    private static final Schema $SCHEMA_CONFIG = $SCHEMA.member("Config");

    private final Type type;

    private GroupedDefaultConfig(Type type) {
        this.type = type;
    }

    public Type type() {
        return type;
    }

    /**
     * Enum representing the possible variants of {@link GroupedDefaultConfig}.
     */
    public enum Type {
        $UNKNOWN,
        group,
        config
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
    public static final class GroupMember extends GroupedDefaultConfig {
        private final transient String value;

        public GroupMember(String value) {
            super(Type.group);
            this.value = Objects.requireNonNull(value, "Union value cannot be null");
        }

        @Override
        public void serializeMembers(ShapeSerializer serializer) {
            serializer.writeString($SCHEMA_GROUP, value);
        }

        public String group() {
            return value;
        }

        @Override
        @SuppressWarnings("unchecked")
        public <T> T getValue() {
            return (T) value;
        }
    }

    @SmithyGenerated
    public static final class ConfigMember extends GroupedDefaultConfig {
        private final transient DefaultConfigResponse value;

        public ConfigMember(DefaultConfigResponse value) {
            super(Type.config);
            this.value = Objects.requireNonNull(value, "Union value cannot be null");
        }

        @Override
        public void serializeMembers(ShapeSerializer serializer) {
            serializer.writeStruct($SCHEMA_CONFIG, value);
        }

        public DefaultConfigResponse config() {
            return value;
        }

        @Override
        @SuppressWarnings("unchecked")
        public <T> T getValue() {
            return (T) value;
        }
    }

    public static final class $UnknownMember extends GroupedDefaultConfig {
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
        return Objects.equals(getValue(), ((GroupedDefaultConfig) other).getValue());
    }

    public interface BuildStage {
        GroupedDefaultConfig build();
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link GroupedDefaultConfig}.
     */
    public static final class Builder implements ShapeBuilder<GroupedDefaultConfig>, BuildStage {
        private GroupedDefaultConfig value;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        public BuildStage group(String value) {
            return setValue(new GroupMember(value));
        }

        public BuildStage config(DefaultConfigResponse value) {
            return setValue(new ConfigMember(value));
        }

        public BuildStage $unknownMember(String memberName) {
            return setValue(new $UnknownMember(memberName));
        }

        private BuildStage setValue(GroupedDefaultConfig value) {
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
        public GroupedDefaultConfig build() {
            return Objects.requireNonNull(value, "no union value set");
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> group((String) SchemaUtils.validateSameMember($SCHEMA_GROUP, member, value));
                case 1 -> config((DefaultConfigResponse) SchemaUtils.validateSameMember($SCHEMA_CONFIG, member, value));
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
                    case 0 -> builder.group(de.readString(member));
                    case 1 -> builder.config(DefaultConfigResponse.builder().deserializeMember(de, member).build());
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

