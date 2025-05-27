>
**2. Identified Subsystems**

Based on the codebase exploration so far, the "Superposition" project can be broken down into the following major subsystems:

1.  **Context-Aware Configuration (CAC) Management Subsystem:**
    *   **Description:** This is the core engine responsible for defining, storing, managing, and resolving configurations based on various contexts. It's the primary focus of the `context_aware_config` crate.
    *   **Key Components/Features:**
        *   Default Configuration Management (`/default-config` API)
        *   Dimension Management (`/dimension` API)
        *   Context Definition & Management (`/context` API)
        *   Type Template Management (`/types` API)
        *   Custom Function Management (`/function` API)
        *   Configuration Resolution Logic (`/config` API, utilizing `cac_client`)
        *   Configuration Versioning
        *   Audit Logging (`/audit` API)
    *   **Purpose:** To provide a flexible and dynamic way to serve tailored configurations to client applications.

2.  **Experimentation Platform Subsystem:**
    *   **Description:** This subsystem, primarily housed in the `experimentation_platform` crate, is responsible for managing and evaluating experiments (e.g., A/B tests, feature flags). It interacts closely with the CAC subsystem, as experiments often result in configuration changes for different user segments or conditions.
    *   **Key Components/Features (Inferred, needs further exploration):**
        *   Experiment Definition (name, variants, targeting rules/context)
        *   Variant Configuration (overrides specific to experiment variants)
        *   Experiment Lifecycle Management (draft, running, paused, completed)
        *   Assignment/Bucketing of users/requests to experiment variants.
        *   Integration with CAC for applying variant-specific configuration overrides.
        *   (Potentially) Metrics collection/integration points for experiment analysis.
    *   **Purpose:** To enable controlled testing of new features or configuration changes and measure their impact.

3.  **Frontend (Admin UI) Subsystem:**
    *   **Description:** This subsystem provides a web-based user interface for managing the CAC and Experimentation Platform. It's built using Leptos (Rust/WASM).
    *   **Key Components/Features (Inferred based on backend APIs):**
        *   UI for CRUD operations on Default Configs, Dimensions, Contexts, Type Templates, Functions.
        *   UI for managing Experiments and their variants.
        *   UI for viewing Audit Logs.
        *   UI for testing Functions.
        *   Tenant-specific views/management.
    *   **Purpose:** To offer a user-friendly way for administrators and developers to interact with and manage the Superposition platform.

4.  **Core Services & Utilities Subsystem:**
    *   **Description:** This encompasses shared functionalities and utilities used across the other subsystems, primarily located in the `service_utils` crate and potentially `superposition_types` and `superposition_macros`.
    *   **Key Components/Features:**
        *   Database interaction utilities (e.g., `PgSchemaManager`, `DbConnection` abstraction).
        *   Middleware (e.g., `TenantMiddlewareFactory`, `AppExecutionScopeMiddlewareFactory` for distinguishing CAC vs. Experimentation API calls).
        *   Shared data types (`superposition_types`).
        *   Common helper functions and macros (`superposition_macros`).
        *   Environment variable management.
        *   Snowflake ID generation.
    *   **Purpose:** To provide common infrastructure, reduce code duplication, and enforce consistent practices across the application.

5.  **API Gateway / Main Application Subsystem:**
    *   **Description:** The `superposition` crate itself acts as the main application, bringing together all other subsystems. It configures and runs the Actix-web server, sets up routing, initializes shared state (`AppState`), and serves both the backend APIs and the Leptos frontend.
    *   **Key Components/Features:**
        *   Actix-web server setup and configuration.
        *   API routing for CAC and Experimentation endpoints.
        *   Serving Leptos frontend and static assets.
        *   Initialization of global application state (`AppState`) including DB pools, shared schemas (meta-schema, default config validation schema).
        *   Request processing pipeline including middleware.
    *   **Purpose:** To be the central orchestrator and entry point for the entire Superposition application.

6.  **Client Libraries Subsystem:**
    *   **Description:** The `cac_client` and `experimentation_client` crates provide client-side libraries for other applications or services to interact programmatically with the CAC and Experimentation Platform functionalities respectively. The `cac_client` notably contains the `eval_cac` logic used by the `/config` API endpoint itself.
    *   **Purpose:** To facilitate integration of Superposition's capabilities into other systems.

These subsystems work in concert:
*   The **Frontend** allows users to manage entities within the **CAC** and **Experimentation** subsystems.
*   The **CAC Subsystem** defines the rules and structure for configurations.
*   The **Experimentation Platform** leverages and extends the CAC to apply experimental changes.
*   The **API Gateway** exposes these functionalities and uses the **CAC** (via `cac_client`) to resolve configurations for incoming requests.
*   All backend subsystems rely on **Core Services & Utilities** for common tasks like DB access and shared types.
*   **Client Libraries** enable external services to consume the resolved configurations and interact with experiments.

The `caclang` crate mentioned in the root `Cargo.toml` suggests it might be a Domain Specific Language parser/evaluator used within the CAC or Function subsystem, but its exact role is yet to be determined.
