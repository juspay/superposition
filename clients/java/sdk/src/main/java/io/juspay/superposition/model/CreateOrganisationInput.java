
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
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class CreateOrganisationInput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#CreateOrganisationRequest");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("country_code", PreludeSchemas.STRING)
        .putMember("contact_email", PreludeSchemas.STRING)
        .putMember("contact_phone", PreludeSchemas.STRING)
        .putMember("admin_email", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("sector", PreludeSchemas.STRING)
        .putMember("name", PreludeSchemas.STRING,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_COUNTRY_CODE = $SCHEMA.member("country_code");
    private static final Schema $SCHEMA_CONTACT_EMAIL = $SCHEMA.member("contact_email");
    private static final Schema $SCHEMA_CONTACT_PHONE = $SCHEMA.member("contact_phone");
    private static final Schema $SCHEMA_ADMIN_EMAIL = $SCHEMA.member("admin_email");
    private static final Schema $SCHEMA_SECTOR = $SCHEMA.member("sector");
    private static final Schema $SCHEMA_NAME = $SCHEMA.member("name");

    private final transient String countryCode;
    private final transient String contactEmail;
    private final transient String contactPhone;
    private final transient String adminEmail;
    private final transient String sector;
    private final transient String name;

    private CreateOrganisationInput(Builder builder) {
        this.countryCode = builder.countryCode;
        this.contactEmail = builder.contactEmail;
        this.contactPhone = builder.contactPhone;
        this.adminEmail = builder.adminEmail;
        this.sector = builder.sector;
        this.name = builder.name;
    }

    public String countryCode() {
        return countryCode;
    }

    public String contactEmail() {
        return contactEmail;
    }

    public String contactPhone() {
        return contactPhone;
    }

    public String adminEmail() {
        return adminEmail;
    }

    public String sector() {
        return sector;
    }

    public String name() {
        return name;
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
        CreateOrganisationInput that = (CreateOrganisationInput) other;
        return Objects.equals(this.countryCode, that.countryCode)
               && Objects.equals(this.contactEmail, that.contactEmail)
               && Objects.equals(this.contactPhone, that.contactPhone)
               && Objects.equals(this.adminEmail, that.adminEmail)
               && Objects.equals(this.sector, that.sector)
               && Objects.equals(this.name, that.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(countryCode, contactEmail, contactPhone, adminEmail, sector, name);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        if (countryCode != null) {
            serializer.writeString($SCHEMA_COUNTRY_CODE, countryCode);
        }
        if (contactEmail != null) {
            serializer.writeString($SCHEMA_CONTACT_EMAIL, contactEmail);
        }
        if (contactPhone != null) {
            serializer.writeString($SCHEMA_CONTACT_PHONE, contactPhone);
        }
        serializer.writeString($SCHEMA_ADMIN_EMAIL, adminEmail);
        if (sector != null) {
            serializer.writeString($SCHEMA_SECTOR, sector);
        }
        serializer.writeString($SCHEMA_NAME, name);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_ADMIN_EMAIL, member, adminEmail);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_NAME, member, name);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_COUNTRY_CODE, member, countryCode);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONTACT_EMAIL, member, contactEmail);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONTACT_PHONE, member, contactPhone);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_SECTOR, member, sector);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link CreateOrganisationInput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.countryCode(this.countryCode);
        builder.contactEmail(this.contactEmail);
        builder.contactPhone(this.contactPhone);
        builder.adminEmail(this.adminEmail);
        builder.sector(this.sector);
        builder.name(this.name);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link CreateOrganisationInput}.
     */
    public static final class Builder implements ShapeBuilder<CreateOrganisationInput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String countryCode;
        private String contactEmail;
        private String contactPhone;
        private String adminEmail;
        private String sector;
        private String name;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * @return this builder.
         */
        public Builder countryCode(String countryCode) {
            this.countryCode = countryCode;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder contactEmail(String contactEmail) {
            this.contactEmail = contactEmail;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder contactPhone(String contactPhone) {
            this.contactPhone = contactPhone;
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder adminEmail(String adminEmail) {
            this.adminEmail = Objects.requireNonNull(adminEmail, "adminEmail cannot be null");
            tracker.setMember($SCHEMA_ADMIN_EMAIL);
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder sector(String sector) {
            this.sector = sector;
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder name(String name) {
            this.name = Objects.requireNonNull(name, "name cannot be null");
            tracker.setMember($SCHEMA_NAME);
            return this;
        }

        @Override
        public CreateOrganisationInput build() {
            tracker.validate();
            return new CreateOrganisationInput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> adminEmail((String) SchemaUtils.validateSameMember($SCHEMA_ADMIN_EMAIL, member, value));
                case 1 -> name((String) SchemaUtils.validateSameMember($SCHEMA_NAME, member, value));
                case 2 -> countryCode((String) SchemaUtils.validateSameMember($SCHEMA_COUNTRY_CODE, member, value));
                case 3 -> contactEmail((String) SchemaUtils.validateSameMember($SCHEMA_CONTACT_EMAIL, member, value));
                case 4 -> contactPhone((String) SchemaUtils.validateSameMember($SCHEMA_CONTACT_PHONE, member, value));
                case 5 -> sector((String) SchemaUtils.validateSameMember($SCHEMA_SECTOR, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<CreateOrganisationInput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_ADMIN_EMAIL)) {
                adminEmail("");
            }
            if (!tracker.checkMember($SCHEMA_NAME)) {
                name("");
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
                    case 0 -> builder.adminEmail(de.readString(member));
                    case 1 -> builder.name(de.readString(member));
                    case 2 -> builder.countryCode(de.readString(member));
                    case 3 -> builder.contactEmail(de.readString(member));
                    case 4 -> builder.contactPhone(de.readString(member));
                    case 5 -> builder.sector(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

