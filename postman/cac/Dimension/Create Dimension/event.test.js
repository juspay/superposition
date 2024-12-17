pm.test("201 Check", function () {
    pm.environment.set("dimension_name", "clientId");
    pm.response.to.have.status(201);
})