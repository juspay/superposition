local CacClient = require("cacclient.CacClient")
local ExperimentationClient = require("expclient.ExperimentationClient")

local tenant_name = "dev"
local polling_frequency = 1
local cac_host_name = "http://localhost:8080"

local exp_client = ExperimentationClient:new(tenant_name, polling_frequency, cac_host_name)
local response = exp_client:create_new_experimentation_client()
print(exp_client:get_satisfied_experiments("{}",""))

local cac_client = CacClient:new(tenant_name, polling_frequency, cac_host_name)
local response = cac_client:create_new_cac_client()
print(cac_client:get_last_modified())