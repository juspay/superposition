/* global pm */

pm.test('expect response be 200', function () {
    pm.response.to.be.ok;
    const response = pm.response.json();
    const modified_response = response.data.map(({ created_at, last_modified, ...rest }) => rest);
    console.log("this is the wrong thing", modified_response);
    pm.expect(JSON.stringify(modified_response)).to.be.eq(JSON.stringify([
        {
            created_by: "user@superposition.io",
            type_name: "Number",
            type_schema: {
                type: "number"
            }
        },
        {
            created_by: "user@superposition.io",
            type_name: "Decimal",
            type_schema: {
                type: "number"
            }
        },
        {
            created_by: "user@superposition.io",
            type_name: "Boolean",
            type_schema: {
                type: "boolean"
            }
        },
        {
            created_by: "user@superposition.io",
            type_name: "String (Enum)",
            type_schema: {
                enum: "$replacement",
                type: "string"
            }
        },
        {
            created_by: "user@superposition.io",
            type_name: "String (Regex)",
            type_schema: {
                pattern: "$replacement",
                type: "string"
            }
        }
    ]))
});
