const host = pm.variables.get("host");
const token = pm.variables.get("token");

pm.test("204 check", function () {
    pm.response.to.have.status(204);
})

pm.test("404 check", function () {
    const deleteRequest = {
        url: `${host}/dimension/dim1`,
        method: 'DELETE',
        header: {
            'Content-Type': 'application/json',
            'x-tenant': 'test',
            'Authorization': `Bearer ${token}`
        }
    };

    pm.sendRequest(deleteRequest, (error, response) => {
        response.to.have.status(404);
    });
})
