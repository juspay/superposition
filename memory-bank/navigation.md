>
**5. Project Organization, Navigation, and Data Flow**

This section describes the project's directory structure, how to navigate it, how subsystems interact, context switching, component lifecycles, and data management processes.

**Project Organization & Navigation:**

The project is a Rust workspace (monorepo) located under a root directory. The primary organizational unit is the **Cargo crate**. The main crates are found within a `crates/` subdirectory at the root of the workspace.

*   **Root Directory:**
    *   `Cargo.toml`: Defines the workspace members and common dependencies/lints.
    *   `crates/`: Contains all the individual library and binary crates.
        *   `superposition/`: The main binary crate that launches the Actix web server.
            *   `src/main.rs`: Entry point of the application. Initializes `AppState`, sets up Actix routes, and starts the HTTP server.
            *   `Cargo.toml`: Defines dependencies specific to the `superposition` application, including local path dependencies to other crates in the workspace.
        *   `context_aware_config/`: Crate responsible for all Context-Aware Configuration (CAC) logic and APIs.
            *   `src/lib.rs`: Crate root, declares modules (`api`, `db`, `helpers`, etc.).
            *   `src/api/`: Contains submodules for each CAC resource (e.g., `context`, `dimension`, `default_config`, `config`, `functions`, `type_templates`, `audit_log`). Each of these typically has:
                *   `handlers.rs`: Implements Actix request handlers.
                *   `types.rs`: Defines DTOs for requests and responses.
            *   `src/db/`: Contains Diesel schema definitions (`schema.rs`) and database models (`models.rs`).
            *   `Cargo.toml`: Defines dependencies for this crate.
        *   `experimentation_platform/`: Crate for the Experimentation Platform logic and APIs.
            *   `src/lib.rs`: Crate root, declares modules (`api`, `db`).
            *   `src/api/experiments/`: Contains `handlers.rs` and `types.rs` for experiment-related APIs.
            *   `src/db/`: Likely contains Diesel schema and models for experiments.
            *   `Cargo.toml`: Defines dependencies.
        *   `frontend/`: The Leptos-based frontend application crate.
            *   `src/app.rs`: Likely the root component of the Leptos UI.
            *   `src/components/`: (Expected) Directory for UI components.
            *   `styles/style.css`: Main CSS file.
            *   `assets/`: Directory for static assets like images.
            *   `Cargo.toml`: Defines dependencies for the frontend, including Leptos.
        *   `service_utils/`: A utility crate providing shared functionalities like database connection management (`PgSchemaManager`, `DbConnection`), shared types (`AppState`, `AppEnv`, `AppScope`), middleware, and helper functions.
        *   `superposition_types/`: Defines common types used across the workspace, particularly for `Result`/`AppError` and the `User` struct.
        *   `superposition_macros/`: Defines procedural macros, mainly for error handling boilerplate.
        *   `cac_client/`: Client library for interacting with the CAC system's resolution logic. Contains `eval_cac` and `eval_cac_with_reasoning`.
        *   `experimentation_client/`: (Expected) Client library for interacting with the Experimentation Platform.
        *   `caclang/`: Crate for parsing and evaluating a TOML-based configuration DSL.
            *   `src/lib.rs`: Contains parsing logic using `toml` and `evalexpr`.
    *   `examples/`: Contains example applications demonstrating client integration for `cac_client` and `experimentation_client`.
    *   `target/`: Standard Rust build output directory. `target/site` is specified as the Leptos build output for the frontend.

**Navigating the Project:**
1.  Start with the root `Cargo.toml` to see all workspace members.
2.  Examine `crates/superposition/src/main.rs` to understand server setup, middleware, and top-level routing.
3.  For specific API functionalities:
    *   CAC APIs: Navigate to `crates/context_aware_config/src/api/{resource_name}/`.
    *   Experimentation APIs: Navigate to `crates/experimentation_platform/src/api/experiments/`.
4.  For shared logic: `crates/service_utils/`, `crates/superposition_types/`, `crates/superposition_macros/`.
5.  For frontend UI: `crates/frontend/`.
6.  For TOML-based configuration parsing: `crates/caclang/`.

**Subsystem Interactions & Context Switching:**

*   **User Request Flow (API Call):**
    1.  An HTTP request hits the `superposition` Actix server.
    2.  **Tenant Middleware (`TenantMiddlewareFactory`):** Identifies the tenant based on the request (e.g., header, path). This tenant context is likely added to request extensions.
    3.  **App Scope Middleware (`AppExecutionScopeMiddlewareFactory`):** Determines if the request is for CAC (`AppScope::CAC`) or Experimentation (`AppScope::EXPERIMENTATION`) based on the URL path prefix. This scope is also likely added to request extensions.
    4.  The `PgSchemaManager` (from `service_utils`, part of `AppState`) uses the tenant and app scope information to provide a database connection (`DbConnection`) that is appropriately scoped (e.g., points to the correct PostgreSQL schema for that tenant/scope).
    5.  The request is routed to the appropriate handler in either `context_aware_config` or `experimentation_platform`.
    6.  Handlers use the scoped `DbConnection` to interact with the database.
    7.  `User` information (e.g., email) is extracted (likely from auth middleware not yet detailed, or a default is used) and used for audit logging.

*   **Context Switching (Tenant/Scope):**
    *   The `TenantMiddlewareFactory` and `AppExecutionScopeMiddlewareFactory` are crucial for context switching.
    *   The `PgSchemaManager` is the component that materializes this context switch at the database level by selecting the correct schema or applying appropriate row-level security policies based on the tenant and application scope derived by the middlewares.

*   **CAC and Experimentation Interaction:**
    *   The Experimentation Platform (`experimentation_platform`) likely defines experiments that result in specific configuration overrides. These overrides must be applied in conjunction with or in place of standard CAC overrides.
    *   This can happen in a few ways:
        1.  The Experimentation Platform might create/manage its own high-priority CAC `Contexts`.
        2.  The `cac_client::eval_cac` function (used by `/config/resolve` in `context_aware_config`) might be designed to also fetch and incorporate active experiment variant overrides for the given evaluation context. The `ExperimentationFlags` in `AppState` could control how these overrides interact (e.g., `ALLOW_SAME_KEYS_OVERLAPPING_CTX`).
    *   The resolved configuration from `/config/resolve` would then reflect both the base CAC rules and any active experimental modifications.

**Component Lifecycle (Example: Defining and Using a Context):**

1.  **Dimension Definition:** An admin defines a `Dimension` (e.g., "country") via the `PUT /dimension` API, specifying its name, schema (e.g., `{ "type": "string" }`), and priority. This is stored in the `dimensions` table.
2.  **Default Config Definition:** An admin defines a `DefaultConfig` (e.g., "welcomeMessage") via `PUT /default-config/welcomeMessage`, providing a default value (e.g., "Hello!") and a schema. This is stored in the `default_configs` table.
3.  **Context Creation:** An admin defines a `Context` via `PUT /context`.
    *   Request body: `{"context": {"country": "IN"}, "override": {"welcomeMessage": "Namaste!"}}`.
    *   The system hashes `{"country": "IN"}` to get `context_id` and `{"welcomeMessage": "Namaste!"}` to get `override_id`.
    *   It validates `{"country": "IN"}` against the "country" `Dimension`'s schema.
    *   It calculates priority based on the "country" `Dimension`'s priority.
    *   It validates `{"welcomeMessage": "Namaste!"}` against the "welcomeMessage" `DefaultConfig`'s schema.
    *   The `Context` is stored in the `contexts` table.
    *   An entry is made in the `event_log` table.
    *   A new `ConfigVersion` snapshot is created/updated in the `config_versions` table.
4.  **Configuration Resolution:** A client application requests configuration for a user in India: `GET /config?country=IN`.
    *   The `superposition` service routes to `context_aware_config::api::config::handlers::get`.
    *   The handler (or `cac_client`) fetches default configs and relevant contexts.
    *   The context `{"country": "IN"}` matches.
    *   Its override `{"welcomeMessage": "Namaste!"}` is applied.
    *   The resolved config `{"welcomeMessage": "Namaste!", ...other defaults...}` is returned.

**Data Fetching, Validation, Update, and Organization:**

*   **Fetching:**
    *   Primarily through Diesel queries in API handlers.
    *   Lists often support pagination (`page`, `count` query parameters).
    *   Specific items are fetched by their primary key (e.g., `/dimension/{name}`, `/context/{id}`).
    *   The `/config` endpoint fetches either a versioned snapshot or generates the current configuration from live tables.
*   **Validation:**
    *   **Request DTOs:** Serde handles basic type validation during deserialization.
    *   **Key/Name Formats:** Regex validation for `DefaultConfig` keys, `Dimension` names, `TypeTemplate` names.
    *   **JSON Schemas:**
        *   `jsonschema` crate validates `DefaultConfig.value` against `DefaultConfig.schema`.
        *   It validates `Dimension` values within context conditions against `Dimension.schema`.
        *   It validates `TypeTemplate.type_schema` and `Dimension.schema` against a `meta_schema`.
    *   **Custom Functions:** `compile_fn` (syntax check) and `execute_fn` (runtime test) for user-defined functions.
    *   **Business Logic:** Checks like `priority > 0` for dimensions, existence of referenced entities (e.g., function_name in `functions` table for `DefaultConfig`), preventing deletion of `DefaultConfig` keys used in context overrides.
*   **Updating:**
    *   Typically via `PUT` (for create/replace) or `PATCH` (for partial updates, e.g., `Function` draft).
    *   Diesel's `insert_into(...).on_conflict(...).do_update()` pattern is common for upserts.
    *   `diesel::update()` for targeted updates.
    *   All significant write operations are wrapped in database transactions and result in a new `ConfigVersion`.
*   **Organization (Database):**
    *   Data is stored in PostgreSQL tables, corresponding to the entities: `default_configs`, `dimensions`, `contexts`, `functions`, `type_templates`, `event_log`, `config_versions`.
    *   Experimentation data (experiments, variants) is stored in its own set of tables within the `experimentation_platform`'s schema.
    *   `PgSchemaManager` suggests that tenants and/or application scopes (CAC vs. Experimentation) might have their data segregated into different PostgreSQL schemas.
    *   JSON `Value` type is used to store flexible data like schemas, context conditions, and overrides.

**API Call List (Exhaustive - based on current exploration):**

This list focuses on the structure; specific path parameters like `{tenant}` or a global `{SERVICE_PREFIX}` are implied by `main.rs`.

**`context_aware_config` Subsystem:**
*   **Contexts (`/context`):**
    *   `PUT /`: Create or update (merge override) a context. (Handler: `put_handler`)
    *   `PUT /overrides`: Create or update (replace override) a context. (Handler: `update_override_handler`)
    *   `PUT /move/{ctx_id}`: Change the condition/ID of an existing context. (Handler: `move_handler`)
    *   `DELETE /{ctx_id}`: Delete a context. (Handler: `delete_context`)
    *   `POST /bulk-operations`: Perform multiple context actions (Put, Delete, Move). (Handler: `bulk_operations`)
    *   `GET /list`: List contexts with pagination. (Handler: `list_contexts`)
    *   `POST /get`: Get a context by its condition (hashed). (Handler: `get_context_from_condition`)
    *   `GET /{ctx_id}`: Get a context by its ID. (Handler: `get_context`)
    *   `PUT /priority/recompute`: Recompute priorities for all contexts. (Handler: `priority_recompute`)
*   **Dimensions (`/dimension`):**
    *   `PUT /`: Create or update (upsert) a dimension. (Handler: `create`)
    *   `GET /`: List all dimensions. (Handler: `get`)
*   **Default Configs (`/default-config`):**
    *   `PUT /{key}`: Create or update (upsert) a default configuration. (Handler: `create`)
    *   `GET /`: List all default configurations. (Handler: `get`)
    *   `DELETE /{key}`: Delete a default configuration (if not in use by contexts). (Handler: `delete`)
*   **Configuration Resolution (`/config`):**
    *   `GET /`: Get the effective configuration based on query parameters (evaluation context), optionally filtered by prefix or specific dimensions. Supports versioning and caching headers. (Handler: `get`)
    *   `GET /resolve`: Get the resolved configuration with optional reasoning, based on query parameters. Supports versioning and caching headers. (Handler: `get_resolved_config`)
    *   `PUT /reduce`: Analyze and potentially simplify/reduce context overrides (idempotent if `x-approve` is false, destructive if true). (Handler: `reduce_config`)
*   **Functions (`/function`):**
    *   `POST /`: Create a new function (draft). (Handler: `create`)
    *   `PATCH /{function_name}`: Update an existing function (draft). (Handler: `update`)
    *   `GET /{function_name}`: Get a specific function. (Handler: `get`)
    *   `GET /`: List all functions. (Handler: `list_functions`)
    *   `DELETE /{function_name}`: Delete a function. (Handler: `delete_function`)
    *   `PUT /{function_name}/{stage}/test`: Test a function (draft or published). (Handler: `test`)
    *   `PUT /{function_name}/publish`: Publish a draft function. (Handler: `publish`)
*   **Type Templates (`/types`):**
    *   `POST /`: Create a new type template. (Handler: `create_type`)
    *   `PUT /{type_name}`: Update an existing type template. (Handler: `update_type`)
    *   `DELETE /{type_name}`: Delete a type template. (Handler: `delete_type`)
    *   `GET /`: List all type templates with pagination. (Handler: `list_types`)
*   **Audit Logs (`/audit`):**
    *   `GET /`: Get audit logs with filtering and pagination. (Handler: `get_audit_logs`)

**`experimentation_platform` Subsystem (`/experiments` - types inferred):**
*   `POST /`: Create a new experiment. (Expected from `ExperimentCreateRequest`)
*   `GET /`: List experiments (with filtering and pagination). (Expected from `ListFilters` and `ExperimentsResponse`)
*   `GET /{experiment_id}`: Get a specific experiment. (Expected)
*   `PUT /{experiment_id}` or `PATCH /{experiment_id}`: Update an experiment (e.g., name, context, status). (Expected)
*   `DELETE /{experiment_id}`: Delete an experiment. (Expected)
*   `PUT /{experiment_id}/ramp`: Update traffic percentage. (Expected from `RampRequest`)
*   `POST /{experiment_id}/conclude`: Conclude an experiment, choosing a variant. (Expected from `ConcludeExperimentRequest`)
*   `PUT /{experiment_id}/variants` (or similar for `OverrideKeysUpdateRequest`): Update variant overrides. (Expected from `OverrideKeysUpdateRequest`)
*   (Potentially other endpoints for managing variants individually)

**Frontend Specific (`/fxn`):**
*   `/fxn/{tail:.*}`: Routes for Leptos server functions, handled by `leptos_actix::handle_server_fns()`.

**General:**
*   `/health`: Health check endpoint.
*   `/favicon.ico`: Serves favicon.
*   `/pkg/*`, `/assets/*`: Serve static frontend assets.

This provides a comprehensive view of the project's organization and data flow. The next step will be to dive deeper into the `experimentation_platform` handlers to confirm its API and then proceed to Task 6.
