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
@documentation("Creates a new organisation with specified details including name, admin contact, and organisational information.")
@http(method: "POST", uri: "/superposition/organisations")
operation CreateOrganisation {
    input: CreateOrganisationRequest
    output: OrganisationResponse
}

@documentation("Retrieves detailed information about a specific organisation including its status, contact details, and administrative metadata.")
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

@documentation("Updates an existing organisation's information including contact details, status, and administrative properties.")
@idempotent
@http(method: "PUT", uri: "/superposition/organisations/{id}")
operation UpdateOrganisation {
    input: UpdateOrganisationRequest
    output: OrganisationResponse
    errors: [
        OrganisationNotFound
    ]
}

@documentation("Retrieves a paginated list of all organisations with their basic information and status details.")
@readonly
@http(method: "GET", uri: "/superposition/organisations")
operation ListOrganisation {
    input := with [PaginationParams] {}
    output := with [PaginatedResponse] {
        data: OrganisationList
    }
}
