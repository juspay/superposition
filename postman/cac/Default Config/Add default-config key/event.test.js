const host = pm.variables.get("host");
const org_id = pm.variables.get("org_id");

function getConfigAndTest(key, value) {
    const getRequest = {
        url: `${host}/config`,
        method: 'GET',
        header: {
            'Content-Type': 'application/json',
            'x-tenant': 'test',
            'x-org-id': `${org_id}`
        }
    };
    console.log(getRequest)
    pm.sendRequest(getRequest, (error, response) => {
        if(error) {
            console.log("Failed to fetch config");
            throw error;
        }
        const resp_obj = response.json();
        const default_configs = resp_obj.default_configs;
        
        // Original checks for key-value
        console.log(`Checking if key=${key} with value=${value} in default_configs`);
        pm.expect(default_configs).to.have.property(key);
        pm.expect(default_configs[key]).to.be.eq(value);
    });
}

// Original test cases
pm.test("201 check", function () {
    pm.response.to.have.status(200);
});

pm.test("Check if key added to default config", function () {
    const key = "key1", value = "value1";
    getConfigAndTest(key, value);
});

// New test case for description and comment fields
pm.test("Check if description and comment fields exist", function () {
    const getRequest = {
        url: `${host}/config`,
        method: 'GET',
        header: {
            'Content-Type': 'application/json',
            'x-tenant': 'test',
        }
    };

    pm.sendRequest(getRequest, (error, response) => {
        if(error) {
            console.log("Failed to fetch config");
            throw error;
        }
        const resp_obj = response.json();
        console.log("Response:", JSON.stringify(resp_obj, null, 2));
        
        // Verify the structure of the response
        pm.expect(resp_obj).to.have.property('default_configs');
        const configItem = resp_obj.default_configs.key1;
        
        // If the config is returned as an object
        if (typeof configItem === 'object') {
            pm.expect(configItem).to.have.property('value');
            pm.expect(configItem).to.have.property('description');
            pm.expect(configItem).to.have.property('change_reason');
        }
        // If it's just the value directly
        else {
            // The original format is maintained
            pm.expect(resp_obj.default_configs).to.have.property('description');
            pm.expect(resp_obj.default_configs).to.have.property('change_reason');
        }
    });
});