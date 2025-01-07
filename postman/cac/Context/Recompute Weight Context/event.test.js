const host = pm.variables.get("host");
const context_id = pm.environment.get("context_id");
const org_id = pm.variables.get("org_id");


function getContextAndTest() {
    const getContext = {
        url : `${host}/context/${context_id}`,
        method: 'GET',
        header: {
            'Content-Type': 'application/json',
            'x-tenant': 'test',
            'x-org-id': `${org_id}`
        }
    
    };
    pm.sendRequest(getContext, (error, response) => {
        if(error) {
            console.log("Failed to fetch context");
            throw error;
        }
        console.log(response.json())
        pm.expect(response.json().weight).to.be.eq("2");

    })
    
}

pm.test("200 check", function () {
    pm.response.to.have.status(200);
});


pm.test("Check weight update", function () {
    getContextAndTest()
});