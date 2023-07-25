const host = pm.variables.get("host");

function getConfigAndTest(context_id, override_id, expected_condition, expected_override) {
    const getRequest = {
        url: `${host}/config`,
        method: 'GET',
        header: {
            'Content-Type': 'application/json',
        }
    };

    pm.sendRequest(getRequest, (error, response) => {
        if(error) {
            console.log("Failed to fetch config");
            throw error;
        }

        const resp_obj = response.json();
        const contexts = resp_obj.contexts;
        const overrides = resp_obj.overrides;

        console.log(`Checking if context=${context_id} contexts list.`);
        const available_context_ids = contexts.map((context) => context.id);
        pm.expect(available_context_ids).to.include(context_id);

        const context = contexts.find((context) => context.id === context_id);

        console.log(`Checking if context condition matches.`);
        const context_condition = context.condition;
        console.log(`Expected => ${JSON.stringify(expected_condition)}`);
        console.log(`Actual => ${JSON.stringify(context_condition)}`);
        pm.expect(JSON.stringify(context_condition)).to.be.eq(JSON.stringify(expected_condition));

        console.log(`Checking if context=${context_id} uses override=${override_id}`);
        const context_override_ids = context.override_with_keys;
        pm.expect(context_override_ids).to.include(override_id);

        
        console.log(`Checking override=${override_id} in overrides object`);
        const override = overrides[override_id];        
        console.log(`Expected => ${JSON.stringify(expected_override)}`);
        console.log(`Actual => ${JSON.stringify(override)}`);
        pm.expect(JSON.stringify(expected_override)).to.be.eq(JSON.stringify(override));
    });
}

pm.test("200 check", function () {
    const response = pm.response.json();
    const context_id = response.context_id;
    const override_id = response.override_id;

    pm.environment.set("context_id", context_id);
    pm.environment.set("override_id", override_id);

    pm.response.to.have.status(200);
})

pm.test("Check if context is added", function () {
    const response = pm.response.json();
    const context_id = response.context_id;
    const override_id = response.override_id;

    const condition = {
        "==": [
            {
                "var": "clientId"
            },
            "piyaz"
        ]
    };
    const override = {
        "key1": "value2"
    };


    getConfigAndTest(context_id, override_id, condition, override);
});
