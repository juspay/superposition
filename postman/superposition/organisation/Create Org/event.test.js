const host = pm.variables.get("host");

function getOrgAndTest(org_id) {
    const getRequest = {
        url: `${host}/superposition/organisations/${org_id}`,
        method: 'GET',
        header: {
            'Content-Type': 'application/json',
            'x-tenant': 'test'
        }
    };

    pm.sendRequest(getRequest, (error, response) => {
        if(error) {
            console.log("Failed to fetch organisation");
            throw error;
        }

        const resp_obj = response.json();
        pm.expect(resp_obj.name).to.be.eq("testorg");
    });
}

pm.test("200 check", function () {
    console.log(pm.response)
    const response = pm.response.json();

    pm.environment.set("org_id", response.id);
    pm.variables.set("org_id", response.id);
    pm.response.to.have.status(200);
})

pm.test("Check if org is added", function () {
    const response = pm.response.json();
    const org_id = response.id;
    getOrgAndTest(org_id);
});
