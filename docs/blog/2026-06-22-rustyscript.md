---
slug: js-sandbox-in-rust
title:  Rust JS Sandbox - Running Untrusted JavaScript Inside Superposition
description: The design and implementation of validation and value compute functions - running user defined JS functions inside rust that is seamless and isolated
tags: [technical deepdive, rust, js, functions]
---

TL;DR - We used RustyScript to sandbox user submitted JS code inside our rust application

[Superposition](https://github.com/juspay/superposition) is a context-aware configuration management system that ensures correct, safe and reliable config changes. One of the ways we've achieved this goal is with jsonschema types for all configuration values with a default value. We call this concept [Default Configs](/docs/basic-concepts/context-aware-config/default-config). We soon realized an issue - though the type is right; that does not guarantee the correctness of the actual value. 

# Where types aren't enough

Take the following configuration:

```json
{
    "image_url": "https://example.com/image/cat.png"
}
```

The most strict JSON schema for the value of image_url would be:
```
{
  "type": "string",
  "pattern": "^https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-zA-Z0-9()]{1,6}\\b([-a-zA-Z0-9()@:%_\\+.~#?&//=]*)$"
}
```
This schema validates the type and shape and that is one part of correctness, but we're missing the most critical aspect - does the URL exist? Does it return what we're expecting?

# User Defined Validation Functions!

We came up with functions - a way for any end user to write validation functions and link it to a Default Config or [Dimension](/docs/basic-concepts/context-aware-config/dimensions) to ensure the correctness of that value (should they want to do that). These functions are written in our frontend monaco editor and saved to a database with some other information. The function definition looks like this:

```
async function execute(payload) -> boolean
```

If the function returned false, the value trying to be set is invalid and Superposition would not allow it to be saved as a configuration change.

# First implementation - NodeJS isolates

Since we just needed a boolean return value, we wrapped the user code with Isolates and ran it as a nodeJS script. If the function returned false, the code forced the script to terminate with an custom exit code. When this was run through Rust's `std::process::Command`, we checked the exit code and deciphered the return value of the function. Users also asked for logs, so console.log would output to stdout, which we also read and returned to the user. Since this was NodeJS, any npm package like axios for HTTP requests we wanted to make available to the user could just be installed and used along with the script we we're running. 

When a function is being edited/created, we also ensured that the function the user provided is valid.

This was a simple solution we tried initially and it worked well - until users wanted more capabilities. One capability was **Value Compute Functions** - functions that returned a vector of values. Users wanted to select a value from a dynamic list of potential values that changed often which was fetched from internal systems at Juspay. Another downside to our nodeJS isolate solution was a lot of wrapper code to ensure that we properly isolated user code. We didn't validate if the code that was being stored was valid syntatically, and if node could not execute it we didn't return that error to the user.

# Other options

The team had previously looked into [`mlua`](https://crates.io/crates/mlua) and looked at ways to run javascript from within rust to have better interoperability. Two options we considered were [`rquickjs`](https://crates.io/crates/rquickjs) and [`rustyscript`](https://crates.io/crates/rustyscript); we went with rustyscript because of its [documentation](https://rscarson.github.io/rustyscript-book/), features and closer compatibility with nodeJS.

# Running Javascript in Rust with Rustyscript

Rustyscript is powerful and extremely customizable for running user code - it has a permissions model, functions that you can expose from rust to use in JS land and some very nice built in functionality like console, crypto, http, etc. 

Here's a function to valid if a link is valid by calling it and checking if an `OK` response code is sent back:

```javascript
async function execute(payload) {
    const { value_validate } = payload;
    // value_validate is an object that contains all arguments a validation function would need
    const { key, value, type, environment } = value_validate; 
    let response = await fetch(value);
    return response.status == '200';
}
```

All functions in Superposition are named `execute`. This is because during validating the user input, we wrap this code with some helpers and the final code that is passed to rustyscript looks like:

```javascript
async function execute(payload) {
    const { value_validate } = payload;
    // value_validate is an object that contains all arguments a validation function would need
    const { key, value, type, environment } = value_validate; 
    let response = await fetch(value);
    return response.status == '200';
}
export function typeCheck() {
    if (typeof execute === "undefined") {
        throw new Error("execute function is not defined");
    }
}
```
We validate this with `rustyscript::validate`, which takes a string and tells you if its valid JS syntax:

```rust
rustyscript::validate(&type_check_code).map_err(|err| {
    log::error!("Invalid function syntax: {:?}", err);
    bad_argument!("Invalid function syntax: {}", err)
})?;
```
This validates the syntax, but is `execute` a function? To run a function in rustyscript we need to:

- Create a basic runtime
- Load a javascript module,
- Call a function and the resulting value

We check `execute` by running and calling `typeCheck`:

```rust
// create a basic runtime
let module = Module::new("type_check.js", &type_check_code);
let runtime_options = RuntimeOptions {
    timeout: Duration::from_millis(1500),
    ..Default::default()
};

let mut runtime = Runtime::new(runtime_options).map_err(|e| {
    let err_str = e.to_string();
    validation_error!("Failed to create runtime: {}", err_str)
})?;

let module_handle = runtime.load_module(&module).map_err(|e| {
    let err_str = e.to_string();
    validation_error!("Failed to load module: {}", err_str)
})?;
// typeCheck is not an async function, but call_function_async is used as a 
// catch all for sync and async code
let tokio_runtime = runtime.tokio_runtime();
tokio_runtime
    .block_on(async {
        runtime
            .call_function_async::<()>(
                Some(&module_handle),
                "typeCheck",
                json_args!(),
            )
            .await
    })
    .map_err(|e| {
        let err_str = e.to_string();
        validation_error!("Function validation failed: {}", err_str)
    })?;

Ok(())
```

Now when this function needs to be run to validate any input or change, it is wrapped in the below JS code. Our wrapper code for user functions in rustyscript looks like this, we override console so that it can output logs to an array:

```javascript
let logBuffer = [];
const originalConsole = console;
const customConsole = {
    log: (...args) => {
        logBuffer.push("[log] " + args.map(a => typeof a === 'object' ? JSON.stringify(a) : String(a)).join(' '));
    },
    info: (...args) => {
        logBuffer.push("[info] " + args.map(a => typeof a === 'object' ? JSON.stringify(a) : String(a)).join(' '));
    },
    warn: (...args) => {
        logBuffer.push("[warn] " + args.map(a => typeof a === 'object' ? JSON.stringify(a) : String(a)).join(' '));
    },
    error: (...args) => {
        logBuffer.push("[error] " + args.map(a => typeof a === 'object' ? JSON.stringify(a) : String(a)).join(' '));
    },
    debug: (...args) => {
        logBuffer.push("[debug] " + args.map(a => typeof a === 'object' ? JSON.stringify(a) : String(a)).join(' '));
    }
};

Object.defineProperty(globalThis, 'console', {
    value: customConsole,
    writable: false,
    configurable: false
});

function getLogBuffer() {
    return logBuffer;
}

function clearLogBuffer() {
    logBuffer = [];
}

{replaceme-with-code}

export { execute, getLogBuffer, clearLogBuffer };
```

and this wrapped code is executed like this:


```rust
// wrap user function with code shown above
let wrapped_code = generate_wrapped_code(&code.0); 
let module = Module::new("function.js", &wrapped_code);

let runtime_options = RuntimeOptions {
    timeout: Duration::from_millis(1500),
    ..Default::default()
};

let mut runtime = Runtime::new(runtime_options).map_err(|e| {
    let err_str = e.to_string();
    (format!("Failed to create runtime: {}", err_str), None)
})?;

let payload = FunctionPayload {
    version: runtime_version,
    payload: args.clone(),
};
let module_handle = runtime
    .load_module(&module)
    .map_err(|err| (err.to_string(), None))?;

let tokio_runtime = runtime.tokio_runtime();
// execute function, fn_output takes the value of Value::Bool for the example
let fn_output = tokio_runtime
    .block_on(async {
        runtime
            .call_function_async::<serde_json::Value>(
                Some(&module_handle),
                "execute",
                json_args!(payload),
            )
            .await
    })
    .map_err(|err| (err.to_string(), None))?;

// get logs
let stdout = runtime
    .call_function::<Vec<String>>(Some(&module_handle), "getLogBuffer", json_args!())
    .map_err(|err| (err.to_string(), None))?
    .join("\n");

runtime
    .call_function::<()>(Some(&module_handle), "clearLogBuffer", json_args!())
    .map_err(|err| (err.to_string(), None))?;
```

# Conclusion

Rustyscript can do a lot more than what we've shared here - I recommend taking a look at the [documentation](https://rscarson.github.io/rustyscript-book/index.html) if you've never used it before. For Superposition, its ability to run isolated javascript seamlessly from within rust has enabled our team to provide more useful features to customers.

If you like the idea of Superposition, do take a look at our [docs](https://juspay.io/superposition/docs) to learn more. Have questions? Join our [slack](superpositionjp.slack.com) to interact with the team.