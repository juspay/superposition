
pm.test("200 check", function () {
    console.log(pm.response)
    console.log(pm.request)
    const response = pm.response.json();
    pm.response.to.have.status(200);
    pm.environment.set("x_tenant", response.workspace_name);
})
