import requests
import json
import os


context_url = os.getenv("CONTEXT_URL","http://localhost:8080/context")
config_url = os.getenv("CONFIG_URL","http://localhost:8080/config")
cac_config = requests.get(url=config_url, headers={"x-tenant": "mjos"}).json()

print(cac_config)

for i in cac_config["contexts"]:
    context_id = i["id"]
    move_context_data = {
        "context": i["condition"]
    }

    update_context_data = {
        "context": i["condition"],
        "override": cac_config["overrides"][i["override_with_keys"][0]]
    }
    move_context = requests.put(url=context_url + "/move/" + str(context_id), json=move_context_data, headers={"x-tenant": "mjos", "Authorisation": "12345678"})
    if move_context.status_code == 200:
        print("move context request success\n")
        print(move_context.json())
    else:
        print("move context request failed\n")
        print(json.dumps(move_context_data))
    update_context = requests.put(url=context_url, json=update_context_data, headers={"x-tenant": "mjos", "Authorisation": "12345678"})
    if update_context.status_code == 200:
        print("update context request success\n")
        print(update_context.json())
    else:
        print("update context request failed\n")
        print(json.dumps(update_context_data))
