import fetch from "node-fetch";
import cp from "child_process";

let host = "http://localhost:8080" // change it respective host

// Api calls
// Using a local api to get the app_name and asset from the key_name (getApp)

async function getConfig() {
  try {
    const response = await fetch(`${host}/config`, {
      method: "GET",
      headers: {
        "Content-Type": "application/json",
        "Authorization": "Bearer 12345678"
      },
    });
    let config = await response.json();
    switch(response.status) {
      case 200:  
        return config;
      default:
        const errorText = await response.text();
        throw new Error(`Request failed with status ${response.status}: ${errorText}`);
    }
  } catch (error) {
    console.error(error);
    throw error;
  }
}

async function getApp(key, val) {
  try {
    let body = {"key": key, "val": val};
    const response = await fetch(`http://localhost:8080/config/getapp`, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        "Authorization": "Bearer 12345678"
      },
      body: JSON.stringify(body),
    });
    switch(response.status) {
      case 200:  
        let config = await response.json();
        return config;
      default:
        const errorText = await response.text();
        throw new Error(`Request failed with status ${response.status}: ${errorText}`);
    }
  } catch (error) {
    console.error(error);
    throw error;
  }
}

async function updateRegex(key, isArray, pattern) {
  try {
    let body = {};

    if (isArray) {
      body = {"schema":{"type": ["null", "array"],"items": {"type": "string", "pattern": pattern}}};
    } else {
      body = {"schema":{"type": ["null", "string"], "pattern": pattern}};
    }

    const response = await fetch(`${host}/default-config/${key}`, {
      method: "PUT",
      headers: {
        "Content-Type": "application/json",
        "Authorization": "Bearer 12345678"
      },
      body: JSON.stringify(body),
    });
    switch(response.status) {
      case 200:  
        let config = await response.text();
        return config;
      default:
        const errorText = await response.text();
        throw new Error(`Request failed with status ${response.status}: ${errorText}`);
    }
  } catch (error) {
    console.error(error);
    throw error;
  }
}

async function getRegex(app, asset) {
  try {
    let resp = cp.execSync(`runghc regex.hs ${app} ${asset}`, { encoding: 'utf8', stdio: ['pipe', 'pipe', 'ignore'] });
    return resp;
  } catch (error) {
    console.error(error);
    throw error;
  }
}

async function getRegexForValue(key, value) {
  let regex = "";
  console.log(key);
  if (typeof value !== "string") {
    regex = "^(default_array_ignore_this|in.juspay.dotp|in.juspay.ec|in.juspay.escrow|in.juspay.flyer|in.juspay.hyperos|in.juspay.hyperos.placeholder|in.juspay.hyperpay|in.juspay.upiintent|in.juspay.arya|in.juspay.hypercredit|in.juspay.godel|in.juspay.godel.placeholder|in.juspay.hyperapi|in.juspay.hyperupi|in.juspay.inappupi|in.juspay.vies|net.openkochi.yatri|net.openkochi.yatripartner|in.juspay.hyperpay.placeholder)$";
    let update_response = await updateRegex(key, true, regex);
    console.log(update_response); //

    return Promise.resolve(update_response);
  } else {
    if (key.endsWith("etag")) {
      regex = "^default_str_ignore_this$|^(?=.*[a-zA-Z])(?=.*[0-9])[A-Za-z0-9]+$";
    } else {
      try {
        let resp = await getApp(key, value);
        let regex_from_galactus = await getRegex(resp.app_name, resp.asset);

        let add_default_regex = "^default_str_ignore_this$|" + JSON.parse(regex_from_galactus);
        regex = add_default_regex;
        
      } catch (error) {
        console.error(error);
        throw error;
      }
    }
    console.log(regex);
    let update_response = await updateRegex(key, false, regex);
    console.log(update_response); //

    return Promise.resolve(update_response);    
  }
  
}

async function decideRegex(i, default_configs) {
  if (i >= Object.keys(default_configs).length){
    return Promise.resolve("done");
  }
  let key = Object.keys(default_configs)[i];
  let value = Object.values(default_configs)[i];

  try {
    let regex_response = await getRegexForValue(key, value);
    if (regex_response) {
      return await decideRegex(i+1, default_configs);
    } else {
      return Promise.reject("notDone");
    }
  } catch (error) {
    console.error(error);
    throw error;
  } 
}
  
async function heartbeat(){
  let config = await getConfig();
  let default_configs = config.default_configs;

  let keysToFilter = ["pmTestKey1972", "pmTestKey1999"];
  keysToFilter.forEach(key => {
    delete default_configs[key];
  });

  try {
    await decideRegex(0, default_configs);
  } catch (error) {
    console.error(error);
    throw error;
  }
}

heartbeat();
