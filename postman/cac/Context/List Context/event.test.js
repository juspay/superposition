pm.test("200 check", function() {
    pm.response.to.have.status(200);
})


pm.test("Response validation", function() {
    const response = pm.response.json();
    if (response.length == 0) {
        throw "list context should return at least one context now"
    }
});
