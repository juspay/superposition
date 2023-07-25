const host = pm.variables.get("host");
const context_id = pm.environment.get("context_id");

pm.test("204 check", function() {
    pm.response.to.have.status(204);
})

pm.test("Fetch for context should fail with 404", function () {
    const getRequest = {
        url: `${host}/context/${context_id}`,
        method: 'GET',
        header: {
            'Content-Type': 'application/json',
        }
    };

    pm.sendRequest(getRequest, (error, response) => {
        if(error) {
            console.log("Failed to fetch config");
            console.log(`alloo ${error}`);
            throw error;
        }

        pm.expect(response.code).to.be.eq(404);
    });
})
