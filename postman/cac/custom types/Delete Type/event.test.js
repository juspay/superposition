/* global pm */

pm.test('expect response be 200', function () {
    pm.response.to.be.ok;
    const host = pm.variables.get("host");
    const org_id = pm.variables.get("org_id");
    const request = {
        url: `${host}/types`,
        method: 'GET',
        header: {
            'Content-Type': 'application/json',
            'x-tenant': 'test',
            'x-org-id': `${org_id}`
        }
    }
    pm.sendRequest(request, (error, response) => {
        if (error) {
            console.log("Failed to fetch types");
            throw error;
        }
        const resp = response.json();
        const modified_response = resp.map(({ created_at, last_modified, description, change_reason, ...rest }) => rest).sort((a, b) => {
            return a.type_name > b.type_name;
        });
        pm.expect(JSON.stringify(modified_response)).to.be.eq(JSON.stringify([
            {
                "created_by": "user@superposition.io",
                "type_name": "Boolean",
                "type_schema": {
                    "type": "boolean"
                }
            },
            {
                "created_by": "user@superposition.io",
                "type_name": "Decimal",
                "type_schema": {
                    "type": "number"
                }
            },
            {
                "created_by": "user@superposition.io",
                "type_name": "Enum",
                "type_schema": {
                    "enum": ["android", "ios"],
                    "type": "string"
                }
            },
            {
                "created_by": "user@superposition.io",
                "type_name": "Number",
                "type_schema": {
                    "type": "integer"
                }
            },
            {
                "created_by": "user@superposition.io",
                "type_name": "Pattern",
                "type_schema": {
                    "pattern": ".*",
                    "type": "string"
                }
            }
        ]))
    })
});
