/* global pm */

pm.test("expect response be 200", function () {
    pm.response.to.be.ok;
    const response = pm.response.json();
    const modified_response = response.data.map(
        ({ created_at, last_modified_at, last_modified_by, ...rest }) => rest
    );
    console.log("The API returned the response", modified_response);
    pm.expect(JSON.stringify(modified_response)).to.be.eq(
        JSON.stringify([
            {
                type_name: "Pattern",
                type_schema: {
                    pattern: ".*",
                    type: "string",
                },
                created_by: "user@superposition.io",
                description:
                    "Pattern type is used to represent a string that matches a specific pattern",
                change_reason: "initial setup",
            },
            {
                type_name: "Enum",
                type_schema: {
                    enum: ["android", "ios"],
                    type: "string",
                },
                created_by: "user@superposition.io",
                description:
                    "Enum type is used to represent a fixed set of values",
                change_reason: "initial setup",
            },
            {
                type_name: "Boolean",
                type_schema: {
                    type: "boolean",
                },
                created_by: "user@superposition.io",
                description:
                    "Boolean type is used to represent true/false values",
                change_reason: "initial setup",
            },
            {
                type_name: "Decimal",
                type_schema: {
                    type: "number",
                },
                created_by: "user@superposition.io",
                description: "Decimal type is used to represent decimal values",
                change_reason: "initial setup",
            },
            {
                type_name: "Number",
                type_schema: {
                    type: "integer",
                },
                created_by: "user@superposition.io",
                description: "Number type is used to represent numeric values",
                change_reason: "initial setup",
            },
        ])
    );
});
