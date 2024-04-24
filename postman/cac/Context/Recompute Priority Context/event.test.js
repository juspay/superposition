const host = pm.variables.get("host");
const context_id = pm.environment.get("context_id");


function getContextAndTest() {
    const getContext = {
        url : `${host}/context/${context_id}`,
        method: 'GET',
        header: {
            'Content-Type': 'application/json',
            'x-tenant': 'test',
        }
    
    };
    pm.sendRequest(getContext, (error, response) => {
        if(error) {
            console.log("Failed to fetch context");
            throw error;
        }
        console.log(response.json())
        pm.expect(response.json().priority).to.be.eq(200);

    })
    
}

pm.test("200 check", function () {
    pm.response.to.have.status(200);
});


pm.test("Check priority update", function () {
    getContextAndTest()
});