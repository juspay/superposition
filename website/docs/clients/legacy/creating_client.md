 
- [Contributing Guidelines](#contributing-guidelines)
- [Context Aware Config](#context-aware-config)
  - [Understanding the C structs and functions](#understanding-the-c-structs-and-functions)
    - [struct Arc\_Client](#struct-arc_client)
    - [int cac\_last\_error\_length(void)](#int-cac_last_error_lengthvoid)
    - [const char \*cac\_last\_error\_message(void)](#const-char-cac_last_error_messagevoid)
    - [void cac\_free\_string(char \*s)](#void-cac_free_stringchar-s)
    - [int cac\_new\_client(const char \*tenant, unsigned long update\_frequency, const char \*hostname)](#int-cac_new_clientconst-char-tenant-unsigned-long-update_frequency-const-char-hostname)
    - [void cac\_start\_polling\_update(const char \*tenant)](#void-cac_start_polling_updateconst-char-tenant)
    - [void cac\_free\_client(struct Arc\_Client \*ptr)](#void-cac_free_clientstruct-arc_client-ptr)
    - [struct Arc\_Client \*cac\_get\_client(const char \*tenant)](#struct-arc_client-cac_get_clientconst-char-tenant)
    - [const char \*cac\_get\_last\_modified(struct Arc\_Client \*client)](#const-char-cac_get_last_modifiedstruct-arc_client-client)
    - [const char \*cac\_get\_config(struct Arc\_Client \*client, const char \*filter\_query, const char \*filter\_prefix)](#const-char-cac_get_configstruct-arc_client-client-const-char-filter_query-const-char-filter_prefix)
    - [const char \*cac\_get\_resolved\_config(struct Arc\_Client \*client, const char \*query, const char \*filter\_keys, const char \*merge\_strategy)](#const-char-cac_get_resolved_configstruct-arc_client-client-const-char-query-const-char-filter_keys-const-char-merge_strategy)
    - [const char \*cac\_get\_default\_config(struct Arc\_Client \*client, const char \*filter\_keys)](#const-char-cac_get_default_configstruct-arc_client-client-const-char-filter_keys)
  - [Testing with an Example](#testing-with-an-example)
- [Experimentation](#experimentation)
  - [Understanding the C structs and functions](#understanding-the-c-structs-and-functions-1)
    - [struct Arc\_Client](#struct-arc_client-1)
    - [int expt\_last\_error\_length(void)](#int-expt_last_error_lengthvoid)
    - [const char \*expt\_last\_error\_message(void)](#const-char-expt_last_error_messagevoid)
    - [void expt\_free\_string(char \*s)](#void-expt_free_stringchar-s)
    - [int expt\_new\_client(const char \*tenant, unsigned long update\_frequency, const char \*hostname)](#int-expt_new_clientconst-char-tenant-unsigned-long-update_frequency-const-char-hostname)
    - [void expt\_start\_polling\_update(const char \*tenant)](#void-expt_start_polling_updateconst-char-tenant)
    - [void expt\_free\_client(struct Arc\_Client \*ptr)](#void-expt_free_clientstruct-arc_client-ptr)
    - [struct Arc\_Client \*expt\_get\_client(const char \*tenant)](#struct-arc_client-expt_get_clientconst-char-tenant)
    - [char \*expt\_get\_applicable\_variant(struct Arc\_Client \*client, const char \*c\_context, short toss)](#char-expt_get_applicable_variantstruct-arc_client-client-const-char-c_context-short-toss)
    - [char \*expt\_get\_satisfied\_experiments(struct Arc\_Client \*client, const char \*c\_context, const char \*filter\_prefix)](#char-expt_get_satisfied_experimentsstruct-arc_client-client-const-char-c_context-const-char-filter_prefix)
    - [char \*expt\_get\_filtered\_satisfied\_experiments(struct Arc\_Client \*client, const char \*c\_context, const char \*filter\_prefix)](#char-expt_get_filtered_satisfied_experimentsstruct-arc_client-client-const-char-c_context-const-char-filter_prefix)
    - [char \*expt\_get\_running\_experiments(struct Arc\_Client \*client)](#char-expt_get_running_experimentsstruct-arc_client-client)
  - [Testing with an Example](#testing-with-an-example-1)

Superposition and her clients for CAC and Experimentation are written in rust. Since servers/programs implemented in a particular language need libraries written in their own  language, we try to support as many languages as possible to make integrations easier. Some other reasons for providing language specific clients:

- Many languages have a paradigm like OOP or Functional and forcing them to adopt another paradigm may make it confusing to programmers
- Some engineers may get the implementation of the client wrong and may impact performance or memory usage
- It makes it faster to work with a client in a particular language

If you want to add support for a programming language, read on.

# Contributing Guidelines

Thank you for considering adding support for a new language in superposition clients! The following conditions must be met for your PR to be reviewed:

- Your client code should be formatted (You can mention the formatter used in the PR)
- Unit tests of your client code
- Documentation of your client functions is necessary ([See this for an example](client-context-aware-configuration.md))
- An example implementation of the client calling Superposition, loading the configurations and calling all exposed functions of your implemented client

# Context Aware Config

## Understanding the C structs and functions

All C structs and functions are generated automatically when the  `cac-client` written in rust is compiled. Superposition uses the crate `cbindgen` to do this. The compiler generates a `.h` header file and an object file specific to the target platform, for example a `.dll` for windows or `.so` for linux. Some tips for writing an FFI/ABI to these files:

- All memory operations should be done by the rust segment of the code. You should always free memory for strings and clients using the inbuilt `free_*` functions
- You don't need to store the client on your implementation's end. Always use `get_cac_client` to get a pointer to a client to pass to other functions 
- Remember, a client is created for a particular tenant
- Check the header files in the `headers` directory for further documentation
- Your client implementation should expect the object file that it dynamically links to be present in the same directory
- Try to use nix so that the process of dynamic linking dependencies becomes easier. You have to support non-nix setups as well

### struct Arc_Client

This is a pointer to `Arc<Client>` used by rust to manage asynchronous access to the `Client` struct. Most functions expect `Arc_Client` to know what client to operate on

### int cac_last_error_length(void)

Get the length of the error message that was produced during an operation. 

Returns greater than 0 if an error has occurred
Returns 0 if no error

### const char *cac_last_error_message(void)

This function returns a character pointer to the last error message produced by an operation. Use `cac_last_error_length` to get its length to initialize a string if your language requires it.

returns null pointer if no error is present

### void cac_free_string(char *s)

This function takes a character pointer as an arg and frees memory allocated to it. Use it whenever you want to free a string returned by function of `cac-client` so that the rust side can continue to handle memory management, preventing any unwanted memory leaks

### int cac_new_client(const char *tenant, unsigned long update_frequency, const char *hostname)

A function that takes a tenant name as string, the update frequency and the hostname of the Superposition Server as arguments and creates a client that is internally managed by rust. Use `cac_get_client` to get a reference to this client

Returns 0 if client was successfully initialized
Returns 1 if an error occurred, use `cac_last_error_message` to get the error

### void cac_start_polling_update(const char *tenant)

Start polling the superposition server for updates for the given tenant

### void cac_free_client(struct Arc_Client *ptr)

Free the memory allocated to the cac client. Always call this function instead of the implementing language's `free` memory function

### struct Arc_Client *cac_get_client(const char *tenant)

A function to fetch a previously created client by using the tenant argument. Use `cac_new_client` to create a new client.

returns a null pointer if an error occurred. Use `cac_last_error_message` to get the error

returns a pointer to Arc_Client that can be used to perform other client operations

### const char *cac_get_last_modified(struct Arc_Client *client)

A function to get the last modified time of a tenant's config.

returns a null pointer if an error occurred. Use `cac_last_error_message` to get the error

returns a string that represents the last modified time of your tenant's configs

### const char *cac_get_config(struct Arc_Client *client, const char *filter_query, const char *filter_prefix)

A function that returns the config for your tenant. Takes the client, a context (filter_query) and a config prefix to filter on. All configs that have the prefix will be returned.

returns a null pointer if an error occurred. Use `cac_last_error_message` to get the error

returns a string that represents the config of your tenant based on your client and filters

### const char *cac_get_resolved_config(struct Arc_Client *client, const char *query, const char *filter_keys, const char *merge_strategy)

Does the same thing as `cac_get_config` but does not return the entire config, rather the config filtered on the keys provided as arguments

returns a null pointer if an error occurred. Use `cac_last_error_message` to get the error

returns a string that represents the resolved config of your tenant based on your client and filters

### const char *cac_get_default_config(struct Arc_Client *client, const char *filter_keys)

Does the same thing as `cac_get_config` but returns the entire default config, filtered on the config keys provided

returns a null pointer if an error occurred. Use `cac_last_error_message` to get the error

returns a string that represents the default config of your tenant based on your client and filters

## Testing with an Example

Checkout the examples directory to understand how to create examples for `cac_client_integration_example` 

# Experimentation

## Understanding the C structs and functions

All C structs and functions are generated automatically when the  `exp-client` written in rust is compiled. Superposition uses the crate `cbindgen` to do this. The compiler generates a `.h` header file and an object file specific to the target platform, for example a `.dll` for windows or `.so` for linux. Some tips for writing an FFI/ABI to these files:

- All memory operations should be done by the rust segment of the code. You should always free memory for strings and clients using the inbuilt `free_*` functions
- You don't need to store the client on your implementation's end. Always use `get_exp_client` to get a pointer to a client to pass to other functions 
- Remember, a client is created for a particular tenant
- Check the header files in the `headers` directory for further documentation
- Your client implementation should expect the object file that it dynamically links to be present in the same directory
- Try to use nix so that the process of dynamic linking dependencies becomes easier. You have to support non-nix setups as well

### struct Arc_Client

This is a pointer to `Arc<Client>` used by rust to manage asynchronous access to the `Client` struct. Most functions expect `Arc_Client` to know what client to operate on

### int expt_last_error_length(void)

Get the length of the error message that was produced during an operation. 

Returns greater than 0 if an error has occurred
Returns 0 if no error

### const char *expt_last_error_message(void)

This function returns a character pointer to the last error message produced by an operation. Use `cac_last_error_length` to get its length to initialize a string if your language requires it.

returns null pointer if no error is present

### void expt_free_string(char *s)

This function takes a character pointer as an arg and frees memory allocated to it. Use it whenever you want to free a string returned by function of `exp-client` so that the rust side can continue to handle memory management, preventing any unwanted memory leaks

### int expt_new_client(const char *tenant, unsigned long update_frequency, const char *hostname)

A function that takes a tenant name as string, the update frequency and the hostname of the Superposition Server as arguments and creates a client that is internally managed by rust. Use `expt_get_client` to get a reference to this client

Returns 0 if client was successfully initialized
Returns 1 if an error occurred, use `expt_last_error_message` to get the error

### void expt_start_polling_update(const char *tenant)

Start polling the superposition server for updates for the given tenant

### void expt_free_client(struct Arc_Client *ptr)

Free the memory allocated to the cac client. Always call this function instead of the implementing language's `free` memory function

### struct Arc_Client *expt_get_client(const char *tenant)

A function to fetch a previously created client by using the tenant argument. Use `expt_new_client` to create a new client.

returns a null pointer if an error occurred. Use `expt_last_error_message` to get the error

returns a pointer to Arc_Client that can be used to perform other client operations

### char *expt_get_applicable_variant(struct Arc_Client *client, const char *c_context, short toss)

get the experiments that apply to a given context `c_context`. It also takes a number toss between 0 - 100 that is used to assign a variant IDs 

returns null pointer if no variant is found
returns a string formatted array of variant IDs that match the parameters passed

### char *expt_get_satisfied_experiments(struct Arc_Client *client, const char *c_context, const char *filter_prefix)

get the experiments that apply to a given context `c_context`. It also filters on config key prefix

returns null pointer if no variant is found
returns a string formatted array of experiments that match the parameters passed

### char *expt_get_filtered_satisfied_experiments(struct Arc_Client *client, const char *c_context, const char *filter_prefix)

get the experiments that apply to a given context `c_context`. It also filters on config key prefix

returns null pointer if no variant is found
returns a string formatted array of experiments that match the parameters passed

### char *expt_get_running_experiments(struct Arc_Client *client)

get all currently running experiments

returns null pointer if no variant is found
returns a string formatted array of experiments that match the parameters passed

## Testing with an Example

Checkout the examples directory to understand how to create examples for `exp_client_integration_example` 
