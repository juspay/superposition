const host = pm.environment.get("host");
const token = pm.environment.get("token");
const org_id = pm.environment.get("org_id");


function fetch_context_n_test(context_id, expected_override_id, expected_override, expected_variant_context) {
    const getRequest = {
        url: `${host}/context/${context_id}`,
        method: 'GET',
        header: {
            'Content-Type': 'application/json',
            'x-tenant': 'test',
            'x-org-id': `${org_id}`
        }
    };


    pm.sendRequest(getRequest, (error, response) => {
        if(error) {
            console.log("Failed to fetch context");
            throw error;
        }

        const context = response.json();

        /*********** checking contexts created in CAC **********/;


        const variant_override_id = context.override_id;
        const varaint_context = context.value;
        const variant_override = context.override;

        console.log("Testing variant override id");
        console.log("Override from CAC: \n", variant_override_id);
        console.log("Expected Context: \n", expected_override_id);
        pm.expect(variant_override_id).to.be.eq(expected_override_id);

        console.log("Testing variant override");
        console.log("Override from CAC: \n", JSON.stringify(variant_override, null, 2));
        console.log("Expected Context: \n", JSON.stringify(expected_override, null, 2));
        pm.expect(JSON.stringify(variant_override)).to.be.eq(JSON.stringify(expected_override));

        console.log("Testing variant context");
        console.log("Context from CAC: \n", JSON.stringify(varaint_context, null, 2));
        console.log("Expected Context: \n", JSON.stringify(expected_variant_context, null, 2));
        pm.expect(JSON.stringify(varaint_context)).to.be.eq(JSON.stringify(expected_variant_context));
    });
}

function fetch_experiment_n_test(experiment_id, expected_context, expected_varaints, expected_variant_contexts) {
    const options = {
        'method': 'GET',
        'url': `${host}/experiments/${experiment_id}`,
        "header": {
            'Authorization': `Bearer ${token}`,
            'Content-Type': 'application/json',
            'x-tenant': 'test',
            'x-org-id': `${org_id}`
        }
    };

    pm.sendRequest(options, function(error, response) {
        if(error) {
            console.log("Failed to fetch experiment");
            throw error;
        }

        const experiment = response.json();

        const context = experiment.context;
        console.log("Testing Context of Experiment");
        console.log(`Expected: ${JSON.stringify(expected_context, null, 2)}`);
        console.log(`Actual: ${JSON.stringify(context, null, 2)}`);
        pm.expect(JSON.stringify(context)).to.be.eq(JSON.stringify(expected_context));

        const variants = experiment.variants;
        for(const variant of variants) {
            const variant_id = variant.id;

            console.log(`TESTING variant: ${variant_id}`);

            // check if the variant present in the expected_variants
            const variant_cpy = JSON.parse(JSON.stringify(variant));
            delete variant_cpy.override_id;
            delete variant_cpy.context_id;

            const expected_variant = expected_varaints.find((ev) => ev.id === variant_id);
            console.log("Actual Variant:", JSON.stringify(variant_cpy, null, 4));
            console.log("Expected Variant:", JSON.stringify(expected_variant, null, 4));
            pm.expect(JSON.stringify(variant_cpy)).to.be.eq(JSON.stringify(expected_variant));

            /*********/

            const expected_context_id = variant.context_id;
            const expected_override_id = variant.override_id;
            const expected_override = variant.overrides;
            const expected_variant_context = expected_variant_contexts.find(evc => evc.vid === variant_id)?.context;

            fetch_context_n_test(expected_context_id, expected_override_id, expected_override, expected_variant_context);
        }
    });
}

// check experiment creation in experiment
pm.test("200 OK", function () {
    const response = pm.response.json();
    const experiment_id = response.id;

    pm.environment.set("experiment_id", experiment_id);
    pm.response.to.have.status(200);
});


// check for contexts in CAC
pm.test("Test created contexts", function() {
    const response = pm.response.json();
    const experiment_id = response.id;


    const expected_context = {
      "and": [
        {
          "==": [
            {
              "var": "os"
            },
            "ios"
          ]
        },
        {
          "==": [
            {
              "var": "client"
            },
            "testClientCac1"
          ]
        }
      ]
    };
    const expected_varaints = [
        {
            "id": `${experiment_id}-control`,
            "overrides": {
                "pmTestKey1": "value1-control",
                "pmTestKey2": "value1-control"
            },
            "variant_type": "CONTROL",
            "description": "description",
            "change_reason": "change_reason"
        },
        {
            "id": `${experiment_id}-test1`,
            "overrides": {
                "pmTestKey1": "value2-test",
                "pmTestKey2": "value2-test"
            },
            "variant_type": "EXPERIMENTAL",
            "description": "description",
            "change_reason": "change_reason"
        }
    ];
    const expected_variant_contexts = [
        {
            "vid": `${experiment_id}-control`,
            "context": {
                "and": [
                    {
                        "==": [
                            {
                                "var": "os"
                            },
                            "ios"
                        ]
                    },
                    {
                        "==": [
                            {
                                "var": "client"
                            },
                            "testClientCac1"
                        ]
                    },
                    {
                        "in": [
                            `${experiment_id}-control`,
                            {
                                "var": "variantIds"
                            }
                        ]
                    }
                ],
                "description": "description",
                "change_reason": "change_reason"
            }
        },
        {
            "vid": `${experiment_id}-test1`,
            "context": {
                "and": [
                    {
                        "==": [
                            {
                                "var": "os"
                            },
                            "ios"
                        ]
                    },
                    {
                        "==": [
                            {
                                "var": "client"
                            },
                            "testClientCac1"
                        ]
                    },
                    {
                        "in": [
                            `${experiment_id}-test1`,
                            {
                                "var": "variantIds"
                            }
                        ]
                    }
                ],
                "description": "description",
                "change_reason": "change_reason"
            }
        }
    ];

    fetch_experiment_n_test(experiment_id, expected_context, expected_varaints, expected_variant_contexts);
});
