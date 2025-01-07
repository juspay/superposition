const host = pm.variables.get("host");
const token = pm.variables.get("token");
const org_id = pm.variables.get("org_id");

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
            'Authorization': `Bearer ${token}`,
            'x-org-id': `${org_id}`
        }
    };

    pm.sendRequest(deleteRequest, (error, response) => {
        response.to.have.status(404);
    });
})
