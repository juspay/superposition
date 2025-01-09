/* global pm */

pm.test('expect response be 200', function () {
    pm.response.to.be.ok;
    const response = pm.response.json();
    const modified_response = response.data.map(({ created_at, last_modified_at, last_modified_by, description, change_reason, ...rest }) => rest);
    console.log("The API returned the response", modified_response);
    pm.expect(JSON.stringify(modified_response)).to.be.eq(JSON.stringify([
        {
            "type_name": "Pattern",
            "type_schema": {
                "pattern": ".*",
                "type": "string"
            },
            "created_by": "user@superposition.io",
            "description": "",
            "change_reason": ""
        },
        {
            "type_name": "Enum",
            "type_schema": {
                "enum": ["android", "ios"],
                "type": "string"
            },
            "created_by": "user@superposition.io",
            "description": "",
            "change_reason": ""
        },
        {
            "type_name": "Boolean",
            "type_schema": {
                "type": "boolean"
            },
            "created_by": "user@superposition.io",
            "description": "",
            "change_reason": ""
        },
        {
            "type_name": "Decimal",
            "type_schema": {
                "type": "number"
            },
            "created_by": "user@superposition.io",
            "description": "",
            "change_reason": ""
        },
        {
            "type_name": "Number",
            "type_schema": {
                "type": "integer"
            },
            "created_by": "user@superposition.io",
            "description": "",
            "change_reason": ""
        }
    ]))
});
