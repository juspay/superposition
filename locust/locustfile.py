from locust import HttpUser, task, constant_pacing
import random

tenants = ["mjos", "sdkbuildconfig", "sdkunified", "dashboard"]

class SuperpositionLoad(HttpUser):
    # wait_time = constant_pacing(1)

    def call_endpoint(self, tenant):
        config = self.client.get("/config", name="get config for {}".format(tenant), headers={'x-tenant': tenant, 'x-feature': ''}).json
        # print("config for {}\n {}".format(tenant, config))

    def call_endpoint_fast(self, tenant):
        config = self.client.get("/config/fast", name="get cached config for {}".format(tenant), headers={'x-tenant': tenant, 'x-feature': ''}).json
        # print("config for {}\n {}".format(tenant, config))

    # @task
    # def load_config(self):
    #     tenant = random.choice(tenants)
    #     self.call_endpoint(tenant)

    @task
    def load_config_fast(self):
        tenant = random.choice(tenants)
        self.call_endpoint_fast(tenant)

