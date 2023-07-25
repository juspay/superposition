const host = pm.variables.get("host");

function getConfigAndTest(key, value) {
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
        const default_configs = resp_obj.default_configs;

        console.log(`Checking if key=${key} with value=${value} in default_configs`);
        pm.expect(default_configs[key]).to.be.eq(value);
    });
}

pm.test("201 check", function () {
    pm.response.to.have.status(201);
})

pm.test("Check if key added to default config", function () {
    const key = "key1", value = "value1";
    getConfigAndTest(key, value);
});
