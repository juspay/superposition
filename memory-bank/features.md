>
**6. Project Features and Behaviors**

This section details the features and behaviors of the Superposition project, explaining their logic, inputs, outputs, triggers, controls, and interactions with various subsystems.

**I. Context-Aware Configuration (CAC) Subsystem Features:**

1.  **Default Configuration Management:**
    *   **Feature:** Allows defining and managing the master list of all known configuration parameters (`keys`) for the system.
    *   **Logic:** Each default config has a `key`, a default `value`, a JSON `schema` for validation, and can optionally be linked to a `Function` for dynamic value generation or complex validation.
    *   **Input (API - `PUT /default-config/{key}`):** `key` (in path), `CreateReq` JSON body (`value`, `schema`, `function_name`).
    *   **Output (API):** Success message or error; returns the created/updated `DefaultConfig` model.
    *   **Trigger/Control:** Via API calls (e.g., from UI or scripts).
    *   **Interaction:**
        *   Stored in `default_configs` table.
        *   `schema` is validated against a `meta_schema` (from `AppState`).
        *   `value` is validated against its own `schema`.
        *   If `function_name` is provided, its existence is checked, and the `value` is validated against the function.
        *   Cannot be deleted if the `key` is used in any `Context` overrides.
        *   Changes trigger a new `ConfigVersion` and an `EventLog` entry.
        *   Used as the base for config resolution by the `/config` API and `cac_client`.

2.  **Dimension Management:**
    *   **Feature:** Allows defining and managing dimensions, which are the criteria used to define contexts.
    *   **Logic:** Each dimension has a unique `name`, a `priority` (integer), a JSON `schema` (to validate values provided for this dimension in context conditions), and an optional `function_name` (for advanced validation of dimension values).
    *   **Input (API - `PUT /dimension`):** `CreateReq` JSON body (`dimension` name, `priority`, `schema`, `function_name`).
    *   **Output (API):** Created/updated `Dimension` model.
    *   **Trigger/Control:** Via API calls.
    *   **Interaction:**
        *   Stored in `dimensions` table.
        *   `schema` is validated against `meta_schema`.
        *   `priority` must be > 0.
        *   `function_name` existence is checked if provided.
        *   Used by `Context` definitions to specify conditions.
        *   `priority` contributes to the calculated priority of `Contexts`.
        *   Changes trigger an `EventLog` entry (likely, though not explicitly seen for dimension creation itself, changes would affect context priority recomputation).

3.  **Context Definition and Management:**
    *   **Feature:** Allows defining specific conditions (contexts) under which configuration overrides apply.
    *   **Logic:**
        *   A context is defined by a `value` (a JSON object representing dimension-value pairs, e.g., `{"country": "IN", "user_type": "premium"}`) and an `override_` (a JSON object mapping `DefaultConfig` keys to their overridden values).
        *   The `id` of a context is a hash of its `value` (condition). The `override_id` is a hash of its `override_` content.
        *   A `priority` is calculated based on the sum of priorities of dimensions used in its condition.
        *   `validate_dimensions_and_calculate_priority` ensures dimensions in the context condition are valid and calculates priority.
        *   `validate_override_with_default_configs` ensures override keys exist in `DefaultConfig` and values conform to `DefaultConfig` schemas.
        *   `validate_condition_with_functions` and `validate_override_with_functions` allow custom validation using linked functions.
    *   **Input (API - e.g., `PUT /context`):** `PutReq` JSON body (`context` condition map, `r#override` map).
    *   **Output (API):** `PutResp` (context_id, override_id, priority).
    *   **Trigger/Control:** Via API calls (`/context`, `/context/overrides`, `/context/move`, `/context/bulk-operations`).
    *   **Interaction:**
        *   Stored in `contexts` table.
        *   Relies on `Dimensions` for validation and priority calculation.
        *   Relies on `DefaultConfigs` for validating override keys and values.
        *   Can be linked to `Functions` for advanced validation.
        *   Changes trigger a new `ConfigVersion` and an `EventLog` entry.
        *   Used by `/config` API and `cac_client` during config resolution.
        *   The `PUT /context/overrides` endpoint replaces the entire override map, while `PUT /context` (on conflict) merges overrides.
        *   `PUT /context/move/{ctx_id}` changes the `value` (condition) and thus `id` and `priority` of an existing context, keeping its overrides.
        *   `PUT /context/priority/recompute` recalculates and updates priorities for all contexts, useful if dimension priorities change.

4.  **Type Template Management:**
    *   **Feature:** Allows defining and managing named, reusable JSON schema templates.
    *   **Logic:** Each template has a `type_name` and a `type_schema` (a JSON schema definition).
    *   **Input (API - `POST /types`):** `TypeTemplateRequest` JSON body (`type_name`, `type_schema`).
    *   **Output (API):** Created/updated `TypeTemplates` DB model.
    *   **Trigger/Control:** Via API calls.
    *   **Interaction:**
        *   Stored in `type_templates` table.
        *   `type_schema` is validated to be a valid JSON schema.
        *   These templates can be referenced (e.g., via `$ref`) within the schemas of `DefaultConfigs` or `Dimensions` to promote reusability.
        *   Changes likely trigger an `EventLog` entry.

5.  **Custom Function Management:**
    *   **Feature:** Allows defining, testing, and publishing custom logic (e.g., JavaScript code executed in a Deno runtime) for advanced validation or dynamic value generation.
    *   **Logic:**
        *   Functions have a `function_name`, `description`, `runtime_version`, and actual `function` code.
        *   Code is stored Base64 encoded.
        *   Functions have a `Draft` and `Published` lifecycle (`status` field).
        *   `compile_fn` validates code syntax.
        *   `execute_fn` runs the function code against test data.
    *   **Input (API - e.g., `POST /function`):** `CreateFunctionRequest` (`function_name`, `function` code, `runtime_version`, `description`).
    *   **Output (API):** `FunctionResponse` or success/error messages.
    *   **Trigger/Control:** Via API calls (`/function`, `/function/{name}/publish`, `/function/{name}/{stage}/test`).
    *   **Interaction:**
        *   Stored in `functions` table.
        *   Referenced by `DefaultConfigs` and `Dimensions` via `function_name`.
        *   Used during validation steps in `DefaultConfig` and `Context` management (e.g., `validate_value_with_function`).
        *   Changes likely trigger `EventLog` entries.

6.  **Configuration Resolution:**
    *   **Feature:** Provides the resolved configuration values for a given evaluation context (set of dimension-value pairs).
    *   **Logic:**
        1.  Client provides an evaluation context (e.g., `{"country": "US", "device_type": "android"}`).
        2.  System fetches all `DefaultConfig` values.
        3.  System identifies all `Context` definitions whose conditions match the evaluation context (using `jsonlogic` or `evalexpr` on the context's `value` field).
        4.  The `override_` maps from all matching contexts are collected.
        5.  These overrides are merged based on `Context.priority` (higher priority wins). The `cac_client` crate's `eval_cac` (or `eval_cac_with_reasoning`) function, using a specified `MergeStrategy`, performs this.
        6.  The final merged overrides are applied on top of the default config values.
        7.  Functions associated with `DefaultConfigs` or `Dimensions` might be executed if their logic involves dynamic value computation based on the evaluation context.
    *   **Input (API - `GET /config` or `GET /config/resolve`):** Query parameters representing the evaluation context (e.g., `?country=US&device=android`). Optional `version` and `prefix` parameters. Optional `x-merge-strategy` header.
    *   **Output (API):** JSON object representing the fully resolved configuration. `GET /config` returns a `types::Config` struct containing applied contexts, final overrides, and base defaults. `GET /config/resolve` returns just the final key-value pairs (or with reasoning).
    *   **Trigger/Control:** Via API calls from client applications or services.
    *   **Interaction:**
        *   Reads from `default_configs`, `contexts`, `dimensions`, (potentially) `functions` and `config_versions` tables.
        *   Uses `cac_client` for core evaluation.
        *   Supports caching via `If-Modified-Since` header, comparing against max timestamp in `event_log`.
        *   Returns `X-Config-Version`, `X-Audit-Id`, and `Last-Modified` headers.

7.  **Configuration Versioning:**
    *   **Feature:** Maintains historical snapshots of the entire configuration.
    *   **Logic:** After any significant write operation to CAC entities (DefaultConfig, Context, Dimension, etc.), a new entry is created in the `config_versions` table. This entry stores a JSON blob (`config` field) representing the complete, generatable configuration state at that point (all default configs, all contexts, all overrides). The `add_config_version` helper function is responsible for this.
    *   **Input:** Triggered internally after successful DB transactions for relevant API handlers.
    *   **Output:** A new `config_versions` record. The version ID is returned in the `X-Config-Version` response header.
    *   **Trigger/Control:** Automatic on successful write operations.
    *   **Interaction:** The `/config` endpoint can use a `?version={id}` parameter to retrieve a specific historical configuration snapshot from this table.

8.  **Audit Logging:**
    *   **Feature:** Tracks changes to configuration entities.
    *   **Logic:** Most write operations (create, update, delete) on CAC entities (and likely Experimentation entities) insert a record into the `event_log` table. This record includes table name, action, user, timestamp, and optionally old/new values.
    *   **Input (API - `GET /audit`):** `AuditQueryFilters` (time range, table names, action types, username, pagination).
    *   **Output (API):** Paginated list of `EventLog` entries.
    *   **Trigger/Control:** API calls to `/audit` for retrieval. Logging is triggered internally by entity management APIs.
    *   **Interaction:** Reads from the `event_log` table. An `X-Audit-Id` header (latest event log ID for `contexts` table) is often included in `/config` and other responses.

9.  **Configuration Reduction (`PUT /config/reduce`):**
    *   **Feature:** Optimizes the stored configuration by identifying and removing redundant context overrides.
    *   **Logic:**
        *   Iterates through each default config key.
        *   For a given key, it identifies all contexts that override it.
        *   It compares more specific contexts with more general contexts (based on dimension subsets). If a specific context's override for the key is already covered by a more general, higher-priority context with the same override value, the override in the specific context is deemed reducible.
        *   If the `x-approve: true` header is present, these reducible overrides are removed. If a context becomes empty of overrides, it's deleted.
    *   **Input:** `x-approve` header (boolean). No request body.
    *   **Output:** The potentially "reduced" `Config` structure.
    *   **Trigger/Control:** Manual API call.
    *   **Interaction:** Reads `contexts`, `dimensions`, `default_configs`. If approved, modifies `contexts` (deleting or updating overrides) via internal API calls to `put` and `delete_context_api`, which in turn trigger new `ConfigVersion`s.

**II. Experimentation Platform Subsystem Features:**

1.  **Experiment Management (CRUD & Lifecycle):**
    *   **Feature:** Allows defining, listing, retrieving, updating, and deleting experiments.
    *   **Logic:**
        *   An `Experiment` has a `name`, targeting `context` (JSON condition), `variants`, `override_keys`, `status` (`CREATED`, `INPROGRESS`, `PAUSED`, `CONCLUDED`), `traffic_percentage`, and auditing fields.
        *   Each `Variant` has an `id`, `type` (`CONTROL`, `EXPERIMENTAL`), and `overrides` (map of config key to value).
        *   Creation involves validating inputs, checking for conflicts with existing experiments (based on context and override key overlaps, controlled by `ExperimentationFlags`), and creating associated CAC contexts for each variant.
    *   **Input (API - e.g., `POST /experiments`):** `ExperimentCreateRequest` (`name`, `context`, `variants`).
    *   **Output (API):** `ExperimentResponse` or `ExperimentCreateResponse`.
    *   **Trigger/Control:** Via API calls.
    *   **Interaction:**
        *   Stored in `experiments` table.
        *   Crucially interacts with the CAC subsystem via internal HTTP calls to `/context/bulk-operations` to create/delete/modify CAC `Contexts` that represent the experiment variants' configurations. The `variantIds` dimension is used to make these CAC contexts specific.
        *   Changes (create, update, conclude, ramp) are audited in the `event_log` table.
        *   `override_keys` are validated. Variants must override all specified keys.

2.  **Traffic Ramping (`PATCH /experiments/{id}/ramp`):**
    *   **Feature:** Adjusts the percentage of eligible traffic exposed to an experiment.
    *   **Logic:** Updates the `traffic_percentage` field of an experiment. If an experiment is ramped up from 0%, its status changes to `INPROGRESS`.
    *   **Input:** `experiment_id` (path), `RampRequest` JSON body (`traffic_percentage`).
    *   **Output:** Updated `ExperimentResponse`.
    *   **Trigger/Control:** API call.
    *   **Interaction:** Updates the `experiments` table. This value is used by the config resolution mechanism (likely within `cac_client` or how it's called by `/config/resolve`) to determine if an experiment's overrides should be considered for a given request.

3.  **Experiment Conclusion (`POST /experiments/{id}/conclude`):**
    *   **Feature:** Ends an experiment and designates a winning variant.
    *   **Logic:**
        *   Updates the experiment's `status` to `CONCLUDED` and records the `chosen_variant`.
        *   Modifies CAC:
            *   If global experiment (empty context): Winning variant's overrides become new `DefaultConfig` values.
            *   If targeted experiment: Winning variant's CAC context condition is made permanent (by removing the `variantIds` dimension).
            *   CAC contexts for losing/non-chosen variants are deleted.
    *   **Input:** `experiment_id` (path), `ConcludeExperimentRequest` JSON body (`chosen_variant` ID).
    *   **Output:** Updated `ExperimentResponse` and `X-Config-Version` from CAC.
    *   **Trigger/Control:** API call.
    *   **Interaction:** Updates `experiments` table. Makes significant changes to `contexts` and potentially `default_configs` in the CAC system via internal HTTP calls.

4.  **Experiment Audit Logging (`GET /experiments/audit`):**
    *   **Feature:** Allows querying audit logs related to experimentation entities.
    *   **Logic:** Similar to CAC audit logs, queries the `event_log` table with filters.
    *   **Input:** `AuditQueryFilters` (as query parameters).
    *   **Output:** Paginated list of relevant `EventLog` entries.
    *   **Interaction:** Reads from the shared `event_log` table.

**III. Shared/System-Level Features:**

1.  **Multi-Tenancy:**
    *   **Feature:** Isolates data and configuration management for different tenants.
    *   **Logic:** `TenantMiddlewareFactory` identifies the tenant. `PgSchemaManager` uses this to scope database operations, likely by setting the PostgreSQL `search_path` or using tenant-specific connection pools.
    *   **Trigger/Control:** Implicitly through API requests that include tenant identification (e.g., `x-tenant` header). Configured via `TENANTS` environment variable.
    *   **Interaction:** Affects all database interactions in both CAC and Experimentation subsystems.

2.  **Application Scoping (CAC vs. Experimentation):**
    *   **Feature:** Differentiates API calls intended for CAC versus Experimentation.
    *   **Logic:** `AppExecutionScopeMiddlewareFactory` sets an `AppScope` based on URL. This, along with tenant, is used by `PgSchemaManager` for DB scoping.
    *   **Trigger/Control:** Implicitly through the API endpoint path.
    *   **Interaction:** Affects database schema selection/access for all API handlers.

3.  **TOML-based Configuration Language (`caclang` crate):**
    *   **Feature:** Provides an alternative, file-based way to define dimensions, default configs, and context rules using a TOML DSL.
    *   **Logic:** `caclang::ContextAwareConfig::parse()` parses a TOML string. `get_config()` evaluates it against a runtime context.
    *   **Input:** A TOML formatted string/file.
    *   **Output:** A parsed `ContextAwareConfig` struct or resolved config values.
    *   **Trigger/Control:** Used programmatically if this crate is invoked. Its role in the live API-driven system is less direct but could be for initial seeding or batch updates managed by a separate process/tool that then uses the APIs.
    *   **Interaction:** Uses `evalexpr` for expression evaluation. Defines its own representation of CAC entities.

4.  **Health Check (`GET /health`):**
    *   **Feature:** Basic health check endpoint.
    *   **Output:** "Health is good :D"
    *   **Interaction:** Simple HTTP handler, no complex interactions.

**IV. Frontend UI Features (Leptos App - Inferred):**

*   **CRUD Operations for CAC Entities:** UIs to create, read, update, delete Default Configs, Dimensions, Contexts, Type Templates, and Functions.
*   **Experiment Management UI:** UIs for creating, viewing, updating, ramping, and concluding experiments and their variants.
*   **Audit Log Viewer:** UI to display and filter audit logs.
*   **Function Testing UI:** An interface to test custom functions.
*   **Tenant-Aware Views:** The UI likely adapts or filters views based on the selected/authenticated tenant.

This list covers the primary features and behaviors identifiable from the codebase structure and API definitions. The interaction between the CAC and Experimentation subsystems, particularly how experiment variant overrides are translated into and managed as CAC contexts, is a key architectural pattern.
