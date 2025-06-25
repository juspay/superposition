
package io.juspay.superposition.model;

import java.time.Instant;
import java.util.Objects;
import software.amazon.smithy.java.core.schema.PreludeSchemas;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.java.core.schema.SchemaUtils;
import software.amazon.smithy.java.core.schema.SerializableStruct;
import software.amazon.smithy.java.core.schema.ShapeBuilder;
import software.amazon.smithy.java.core.serde.ShapeDeserializer;
import software.amazon.smithy.java.core.serde.ShapeSerializer;
import software.amazon.smithy.java.core.serde.ToStringSerializer;
import software.amazon.smithy.java.core.serde.document.Document;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class AuditLogFull implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#AuditLogFull");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("table_name", PreludeSchemas.STRING)
        .putMember("user_name", PreludeSchemas.STRING)
        .putMember("timestamp", SharedSchemas.DATE_TIME)
        .putMember("action", PreludeSchemas.STRING)
        .putMember("original_data", PreludeSchemas.DOCUMENT)
        .putMember("new_data", PreludeSchemas.DOCUMENT)
        .putMember("query", PreludeSchemas.STRING)
        .build();

    private static final Schema $SCHEMA_TABLE_NAME = $SCHEMA.member("table_name");
    private static final Schema $SCHEMA_USER_NAME = $SCHEMA.member("user_name");
    private static final Schema $SCHEMA_TIMESTAMP = $SCHEMA.member("timestamp");
    private static final Schema $SCHEMA_ACTION = $SCHEMA.member("action");
    private static final Schema $SCHEMA_ORIGINAL_DATA = $SCHEMA.member("original_data");
    private static final Schema $SCHEMA_NEW_DATA = $SCHEMA.member("new_data");
    private static final Schema $SCHEMA_QUERY = $SCHEMA.member("query");

    private final transient String tableName;
    private final transient String userName;
    private final transient Instant timestamp;
    private final transient String action;
    private final transient Document originalData;
    private final transient Document newData;
    private final transient String query;

    private AuditLogFull(Builder builder) {
        this.tableName = builder.tableName;
        this.userName = builder.userName;
        this.timestamp = builder.timestamp;
        this.action = builder.action;
        this.originalData = builder.originalData;
        this.newData = builder.newData;
        this.query = builder.query;
    }

    public String tableName() {
        return tableName;
    }

    public String userName() {
        return userName;
    }

    public Instant timestamp() {
        return timestamp;
    }

    public String action() {
        return action;
    }

    public Document originalData() {
        return originalData;
    }

    public Document newData() {
        return newData;
    }

    public String query() {
        return query;
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
        AuditLogFull that = (AuditLogFull) other;
        return Objects.equals(this.tableName, that.tableName)
               && Objects.equals(this.userName, that.userName)
               && Objects.equals(this.timestamp, that.timestamp)
               && Objects.equals(this.action, that.action)
               && Objects.equals(this.originalData, that.originalData)
               && Objects.equals(this.newData, that.newData)
               && Objects.equals(this.query, that.query);
    }

    @Override
    public int hashCode() {
        return Objects.hash(tableName, userName, timestamp, action, originalData, newData, query);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        if (tableName != null) {
            serializer.writeString($SCHEMA_TABLE_NAME, tableName);
        }
        if (userName != null) {
            serializer.writeString($SCHEMA_USER_NAME, userName);
        }
        if (timestamp != null) {
            serializer.writeTimestamp($SCHEMA_TIMESTAMP, timestamp);
        }
        if (action != null) {
            serializer.writeString($SCHEMA_ACTION, action);
        }
        if (originalData != null) {
            serializer.writeDocument($SCHEMA_ORIGINAL_DATA, originalData);
        }
        if (newData != null) {
            serializer.writeDocument($SCHEMA_NEW_DATA, newData);
        }
        if (query != null) {
            serializer.writeString($SCHEMA_QUERY, query);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_TABLE_NAME, member, tableName);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_USER_NAME, member, userName);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_TIMESTAMP, member, timestamp);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_ACTION, member, action);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_ORIGINAL_DATA, member, originalData);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_NEW_DATA, member, newData);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_QUERY, member, query);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link AuditLogFull}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.tableName(this.tableName);
        builder.userName(this.userName);
        builder.timestamp(this.timestamp);
        builder.action(this.action);
        builder.originalData(this.originalData);
        builder.newData(this.newData);
        builder.query(this.query);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link AuditLogFull}.
     */
    public static final class Builder implements ShapeBuilder<AuditLogFull> {
        private String tableName;
        private String userName;
        private Instant timestamp;
        private String action;
        private Document originalData;
        private Document newData;
        private String query;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * @return this builder.
         */
        public Builder tableName(String tableName) {
            this.tableName = tableName;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder userName(String userName) {
            this.userName = userName;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder timestamp(Instant timestamp) {
            this.timestamp = timestamp;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder action(String action) {
            this.action = action;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder originalData(Document originalData) {
            this.originalData = originalData;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder newData(Document newData) {
            this.newData = newData;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder query(String query) {
            this.query = query;
            return this;
        }

        @Override
        public AuditLogFull build() {
            return new AuditLogFull(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> tableName((String) SchemaUtils.validateSameMember($SCHEMA_TABLE_NAME, member, value));
                case 1 -> userName((String) SchemaUtils.validateSameMember($SCHEMA_USER_NAME, member, value));
                case 2 -> timestamp((Instant) SchemaUtils.validateSameMember($SCHEMA_TIMESTAMP, member, value));
                case 3 -> action((String) SchemaUtils.validateSameMember($SCHEMA_ACTION, member, value));
                case 4 -> originalData((Document) SchemaUtils.validateSameMember($SCHEMA_ORIGINAL_DATA, member, value));
                case 5 -> newData((Document) SchemaUtils.validateSameMember($SCHEMA_NEW_DATA, member, value));
                case 6 -> query((String) SchemaUtils.validateSameMember($SCHEMA_QUERY, member, value));
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
                    case 0 -> builder.tableName(de.readString(member));
                    case 1 -> builder.userName(de.readString(member));
                    case 2 -> builder.timestamp(de.readTimestamp(member));
                    case 3 -> builder.action(de.readString(member));
                    case 4 -> builder.originalData(de.readDocument());
                    case 5 -> builder.newData(de.readDocument());
                    case 6 -> builder.query(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

