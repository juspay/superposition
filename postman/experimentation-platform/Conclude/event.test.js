const host = pm.environment.get("host");
const token = pm.environment.get("token");

const experiment_id = pm.environment.get("experiment_id");

function fetch_config_n_test(variants, winner_variant_id) {
    const options = {
        'method': 'GET',
        'url': `${host}/config`,
        'header': {
            'Authorization': `Bearer ${token}`,
            'Contet-Type': 'application/json'
        }
    };

    pm.sendRequest(options, function(error, response) {
        if(error) {
            console.log("Failed to fetch config");
            throw error;
        }

        const config = response.json();
        const contexts = config.contexts;
        const overrides = config.overrides;

        const winner_variant = variants.find(variant => variant.id === winner_variant_id);
        const winner_variant_override_id = winner_variant.override_id;
        
        // there should be only one context with the winner variant override id
        const contexts_with_winner_variant_override = contexts.filter((context) => context.override_with_keys.includes(winner_variant_override_id));
        console.log("Context with winner variant override");
        console.log(JSON.stringify(contexts_with_winner_variant_override, null, 4));
        pm.expect(contexts_with_winner_variant_override.length).to.be.eq(1);

        // there should be 0 contexts with variant as a dimension
        const contexts_with_variant_dim = contexts
            .filter(
                (context) => 
                    context.condition.and
                        ?.map(
                            (condition) => 
                                Object.keys(condition)
                                    .map((k) => condition[k][0].var === "variant")
                                    .reduce((p, c) => p || c, false))
                        .reduce((p, c) => p || c, false)
            );
        pm.expect(contexts_with_variant_dim.length).to.be.eq(0);

        // checking if winner override exists and is same as the expected override
        const winner_variant_context = contexts_with_winner_variant_override[0]; 
        pm.expect(winner_variant_context.override_with_keys.length).to.be.eq(1);
        pm.expect(JSON.stringify(winner_variant_context.override_with_keys[0])).to.be.eq(JSON.stringify(winner_variant_override_id));

        // checking if all the discarded overrides are removed
        const discarded_variants = variants.filter(variant => variant.id !== winner_variant_id);
        const discarded_variants_override_ids = discarded_variants.map(dv => dv.override_id);
        const available_overrides = Object.keys(overrides);
        for(const ao of available_overrides) {
            pm.expect(discarded_variants_override_ids).to.not.include(ao);
        }
    });
}

function fetch_experiment_n_test(experiment_id, winner_variant_id, expected_status) {
    const options = {
        'method': 'GET',
        'url': `${host}/experiments/${experiment_id}`,
        "header": {
            'Authorization': `Bearer ${token}`,
            'Content-Type': 'application/json'
        }
    };

    pm.sendRequest(options, function(error, response) {
        if(error) {
            console.log("Failed to fetch experiment");
            throw error;
        }
        
        const experiment = response.json();

        const status = experiment.status;
        pm.expect(status).to.be.eq(expected_status);

        const variants = experiment.variants;
        fetch_config_n_test(variants, winner_variant_id);
    });
}

pm.test("200 OK", function() {
    pm.response.to.have.status(200);
});

pm.test("Conclude correctness", function() {
    const winner_variant_id = `${experiment_id}-control`;
    fetch_experiment_n_test(experiment_id, winner_variant_id, "CONCLUDED")
})