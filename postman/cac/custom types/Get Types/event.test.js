/* global pm */

pm.test('expect response be 200', function () {
    pm.response.to.be.ok;
    const response = pm.response.json();
    const modified_response = response.map(({ created_at, id, last_modified, ...rest }) => rest);
    console.log("this is the wrong thing", modified_response);
    pm.expect(JSON.stringify(modified_response)).to.be.eq(JSON.stringify([
        {
            created_by: "user@superposition.io",
            display_name: "Number",
            type_name: "number",
            type_schema: {
                type: "number"
            }
        },
        {
            created_by: "user@superposition.io",
            display_name: "Decimal",
            type_name: "number",
            type_schema: {
                type: "number"
            }
        },
        {
            created_by: "user@superposition.io",
            display_name: "Boolean",
            type_name: "boolean",
            type_schema: {
                type: "boolean"
            }
        },
        {
            created_by: "user@superposition.io",
            display_name: "String (Enum)",
            type_name: "enum",
            type_schema: {
                enum: "$replacement",
                type: "string"
            }
        },
        {
            created_by: "user@superposition.io",
            display_name: "String (Regex)",
            type_name: "pattern",
            type_schema: {
                pattern: "$replacement",
                type: "string"
            }
        }
    ]))
});
