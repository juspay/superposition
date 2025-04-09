$version: "2.0"

namespace io.superposition

resource Organisation {
    identifiers: {
        id: String
    }
    properties: {
        name: String
        country_code: String
        contact_email: String
        contact_phone: String
        created_by: String
        admin_email: String
        status: OrgStatus
        sector: String
        created_at: DateTime
        updated_at: DateTime
        updated_by: String
    }
    create: CreateOrganisation
    read: GetOrganisation
    list: ListOrganisation
    put: UpdateOrganisation
}

enum OrgStatus {
    Active = "Active"
    Inactive = "Inactive"
    PendingKyb = "PendingKyb"
}

@mixin
structure OrganisationFields for Organisation {
    $country_code
    $contact_email
    $contact_phone
    $admin_email
    $sector
}

structure CreateOrganisationRequest for Organisation with [OrganisationFields] {
    @required
    $admin_email

    @required
    $name
}

structure UpdateOrganisationRequest for Organisation with [OrganisationFields] {
    @httpLabel
    @required
    $id

    $status
}

structure OrganisationResponse for Organisation {
    @required
    $id

    @required
    $name

    $country_code

    $contact_email

    $contact_phone

    @required
    $created_by

    @required
    $admin_email

    @required
    $status

    $sector

    @required
    $created_at

    @required
    $updated_at

    @required
    $updated_by
}

list OrganisationList {
    member: OrganisationResponse
}

@httpError(404)
@error("client")
structure OrganisationNotFound {}

// Operations
@http(method: "POST", uri: "/superposition/organisations")
operation CreateOrganisation {
    input: CreateOrganisationRequest
    output: OrganisationResponse
}

@readonly
@http(method: "GET", uri: "/superposition/organisations/{id}")
operation GetOrganisation {
    input := for Organisation {
        @httpLabel
        @required
        $id
    }

    output: OrganisationResponse

    errors: [
        OrganisationNotFound
    ]
}

@idempotent
@http(method: "PUT", uri: "/superposition/organisations/{id}")
operation UpdateOrganisation {
    input: UpdateOrganisationRequest
    output: OrganisationResponse
    errors: [
        OrganisationNotFound
    ]
}

@readonly
@http(method: "GET", uri: "/superposition/organisations")
operation ListOrganisation {
    input := with [PaginationParams] {}
    output := with [PaginatedResponse] {
        data: OrganisationList
    }
}
