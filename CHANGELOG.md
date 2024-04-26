# Changelog
All notable changes to this project will be documented in this file. See [conventional commits](https://www.conventionalcommits.org/) for commit guidelines.

- - -
## v0.41.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
- corrected env for DB_PASSWORD and default for AWS_REGION - (9770384) - Ritick Madaan
- improved log for env not found - (dfd4528) - Ritick Madaan
- base64 decoding kms cypher - (fb19e0c) - Ritick Madaan
- changed host to 0.0.0.0 - (58c4444) - Ritick Madaan
-  dimension Table scaffolding, PUT<>DELETE apis `/dimension` - (a57be1e) - Ritick Madaan
- storing pre calculated priority in contexts table - (d71107e) - Ritick Madaan
- removed delete dimension api - (9f3a771) - Ritick Madaan
-  DB password URI encoded. - (b52a121) - Shrey Bana
-  moved override inside contexts table - (83987a0) - Ritick Madaan
-  removed unecessary and wrap over conditions - (d31cb14) - Ritick Madaan
-  added search path for schema in database_url - (b649c9d) - Ritick Madaan
-  database schema url - (0c4fa3e) - Ritick Madaan
-  added moved tables to cac_v1 schema - (3802d7f) - Ritick Madaan
-  enabled override updates in PUT /context by deep_merge - (141455b) - Ritick Madaan
-  moved dimension_type to cac_v1 schema - (f62beaf) - Ritick Madaan
-  fixed ordering of /context endpoints - (e84811c) - Ritick Madaan
-  updated response-type of /context/bulk-operations api - (9fd2634) - Ritick Madaan
- fixed context overlap check logic - (c432c0d) - Shubhranshu Sanjeev
- added last_modified column and indexes - (aa7690a) - Shubhranshu Sanjeev
- moved tables and types under cac_v1 schema - (023f260) - Shubhranshu Sanjeev
- calling cac apis for creating context - (0cbdcb9) - Shubhranshu Sanjeev
-  updated last_modified in ramp - (9ed8a88) - ankit.mahato
-  minor fixes for exp client - (1281ec7) - Kartik Gajendra
- removed traffic-percentage from experiment create request - (ce297de) - Shubhranshu Sanjeev
-  added total items to list API response - (f051a02) - Kartik Gajendra
- using audit log tstamp for checking last-modified - (4f7e0e2) - Shubhranshu Sanjeev
-  sorting same priority contexts with created_at - (9cebcae) - Ritick Madaan
-  allowing cug users to fall under test variants - (f1b420d) - Ritick Madaan
-  removed unwanted parameter to prevent warning - (7446853) - Ritick Madaan
- added middleware to insert version in response headers - (f5a3f74) - Shubhranshu Sanjeev
-  logged env variable's value before kms decrypting - (532a6d2) - Ritick Madaan
#### Build system
-  installing ca-certificates for ssl verification - (91b5cc9) - Ritick Madaan
- added version tag to docker images - (6b06dfe) - Shubhranshu Sanjeev
#### Continuous Integration
-  Created pipeline for automated-deployment - (7dd03a8) - Shrey Bana
-  Commented out prod docker push. - (8bf3a27) - Shrey Bana
-  Enabling production docker image push. - (d0ca79d) - Shrey Bana
- moved nixpkgs to nixos-22.11 as the unstable one had broken rustfmt - (8e308ab) - Ritick Madaan
-  Upgraded nixos to 23.05. - (b99f5d6) - Shrey Bana
- automated newman test setup - (ee76ab5) - Natarajan Kannan
-  switch to using newman - (cac419c) - Natarajan Kannan
- regenerated schema.patch with latest schema.rs - (73a4eff) - Ritick Madaan
-  enabling tests in pr builds - (40a9a07) - Ritick Madaan
-  udpated `docker container ls` filter - (b985286) - Ritick Madaan
- fix newman dev dependency ref - (131f37c) - Natarajan Kannan
- added postman collection for experimentation-platform - (4b8fdfc) - Shubhranshu Sanjeev
- added cocogitto config for automatic versioning - (caa805f) - Shubhranshu Sanjeev
#### Features
- context/add api along with db setup - (b04ac3f) - Ritick Madaan
-  Added context fetch API - (f8e49b0) - Shrey Bana
-  Added health-check endpoint. - (b8e0b0d) - Shrey Bana
-  added localstack setup along with kms - (18342c8) - Ritick Madaan
- /default-config/<key> PUT api - (acb9df3) - Ritick Madaan
- GET /config api - (0cdbc3c) - Ritick Madaan
-  Added context list API - (a89b485) - Shrey Bana
-  Added support for validation via JSON schema. - (c542af8) - Shrey Bana
-  rust library - (8004510) - Ritick Madaan
-  removed properties constraint on objects in schema - (ba3020e) - Ritick Madaan
-  Added authentication. - (7120d61) - Shrey Bana
-  added DELETE /context/{ctx_id} api - (53ae341) - Ritick Madaan
-  PUT /context/move/{ctx_id} api - (c838b96) - Ritick Madaan
- added 304 <> last-modified for GET /config - (76cabf4) - Saurav Suman
-  added list experiments API - (bd20bdf) - Kartik Gajendra
- added conclude functionality for experiments - (e7e5eba) - Shubhranshu Sanjeev
-  added experimentation client with few fixes - (be0b70f) - Kartik Gajendra
-  add support for last - (34ba4d4) - Kartik Gajendra
- added log table for all cac_v1 tables - (6115759) - Shubhranshu Sanjeev
-  added support for CUG in super position client - (7caaabc) - Kartik Gajendra
-  Added Catch all error type for robust error handling - (c86f6a7) - Kartik Gajendra
-  Added Catch all error type for robust error handling - (f5a0d59) - Kartik Gajendra
#### Refactoring
-  moved db related modules to db crate - (0ed6bbd) - Ritick Madaan
-  removed old contexts table - (e07db3c) - Ritick Madaan
- moved AppState & utility fx to new crate - (b8d5dce) - Shubhranshu Sanjeev
- moved cac to cargo workspaces - (afbf722) - Shubhranshu Sanjeev
- moved fetching db connection in FromRequest trait impl - (c77a2c2) - Shubhranshu Sanjeev
- improvements to APIs - (91c2466) - Shubhranshu Sanjeev
#### Tests
- fix newman version used in tests - (42b9d77) - Natarajan Kannan
- update to latest newman that handles top level events and body lang type - (509ee50) - Natarajan Kannan

- - -

## v0.1.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
- moved git init to separate stage - (154a601) - Shubhranshu Sanjeev
-  cleaned up Dockerfile - (664770b) - Ritick Madaan
#### Features
-  implemented tracing-actix-web for logging - (551968e) - Ritick Madaan
#### Miscellaneous Chores
- **(version)** v0.1.0 [skip ci] - (bfbc1f5) - Jenkins

- - -

## v0.2.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Features
-  add audit log search endpoint - (16d6f7b) - Kartik Gajendra
#### Miscellaneous Chores
- **(version)** v0.2.0 [skip ci] - (f3fd257) - Jenkins
#### Revert
- Revert "fix:  logged env variable's value before kms decrypting" - (a7c24d0) - Ritick Madaan

- - -

## v0.3.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Features
-  added pod information in response headers and logs - (23c9e67) - Kartik Gajendra
#### Miscellaneous Chores
- **(version)** v0.3.0 [skip ci] - (a347bba) - Jenkins

- - -

## v0.4.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
- fixed setting env in docker image - (056b45b) - Shubhranshu Sanjeev
#### Continuous Integration
-  updated integ AP tracker curl with new version - (f92d148) - Ritick Madaan
#### Miscellaneous Chores
- **(version)** v0.4.0 [skip ci] - (d76376d) - Jenkins

- - -

## v0.4.1 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Features
-  record the chosen variant after conclude - (a237254) - Kartik Gajendra
#### Miscellaneous Chores
- **(version)** v0.4.1 [skip ci] - (8c3fb61) - Jenkins

- - -

## v0.5.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
- trimming newline character from version string - (a37f5d2) - Shubhranshu Sanjeev
#### Continuous Integration
- deleting postgres's docker image on every test - (47b615e) - Ritick Madaan
#### Miscellaneous Chores
- **(version)** v0.5.0 [skip ci] - (53b3143) - Jenkins

- - -

## v0.5.1 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
- fixed random timeouts in internal http calls to CAC - (dd17746) - Shubhranshu Sanjeev
- failed build due to untracked schema.rs file changes - (c249efc) - Shubhranshu Sanjeev
-  eval param fix - (a9c5b04) - Pratik Mishra
#### Continuous Integration
- added 20 minutes timeout on pipeline - (7cda606) - Shubhranshu Sanjeev
#### Features
- Schema addition for Dimension values - (48fc4ad) - Prasanna P
-  Adding generic eval - (b63c753) - Pratik Mishra
#### Miscellaneous Chores
- **(version)** v0.5.1 [skip ci] - (cd0cb60) - Jenkins

- - -

## v0.6.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
-  patching overrides on default-config instead of merge - (8e311c7) - Ritick Madaan
#### Miscellaneous Chores
- **(version)** v0.6.0 [skip ci] - (a5f8757) - Jenkins

- - -

## v0.6.1 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Continuous Integration
- **(flake.nix)** pin nodejs version to 18 in flake - (df0adf0) - Natarajan Kannan
#### Features
-  cors middleware attached - (6a5fe1f) - Ritick Madaan
-  added dashboard auth middleware - (5ca8766) - Kartik Gajendra
#### Miscellaneous Chores
- **(version)** v0.6.1 [skip ci] - (5c471b2) - Jenkins

- - -

## v0.7.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
- ssh.bitbucket.juspay.net added to known hosts in docker bulid - (3da23da) - Ritick Madaan
-  add user struct in delete context API - (1ac80c4) - Kartik Gajendra
#### Miscellaneous Chores
- **(version)** v0.7.0 [skip ci] - (150a7be) - Jenkins

- - -

## v0.7.1 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Features
- server's keep-alive time and db connection pool max size made configurable - (585a15c) - Ritick Madaan
#### Miscellaneous Chores
- **(version)** v0.7.1 [skip ci] - (a9e50be) - Jenkins
- database migration for dimensions table - (1337d34) - Ritick Madaan

- - -

## v0.8.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
-  add migration for changing default_configs_keys - (7b6ae82) - Pratik Mishra
#### Miscellaneous Chores
- **(version)** v0.8.0 [skip ci] - (ee5e00c) - Jenkins

- - -

## v0.8.1 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
- validating override_keys for unique entries - (400f3f0) - Shubhranshu Sanjeev
#### Features
- support to update experiment override_keys and variants - (71c7a25) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- **(version)** v0.8.1 [skip ci] - (189c369) - Jenkins
#### Refactoring
- resolved comments - (4422c83) - Shubhranshu Sanjeev
#### Tests
- added tests for experiment helper fnxs - (7a60eea) - Shubhranshu Sanjeev
- added postman test for update override_keys api - (38bc0e9) - Shubhranshu Sanjeev

- - -

## v0.9.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
-  add all variants in manifest - (ef7a37d) - Pratik Mishra
#### Miscellaneous Chores
- **(version)** v0.9.0 [skip ci] - (dc2db17) - Jenkins

- - -

## v0.9.1 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Features
-  - Dimension value schema validation on context-addition - (2c810f7) - Prasanna P
#### Miscellaneous Chores
- **(version)** v0.9.1 [skip ci] - (eddd8a4) - Jenkins

- - -

## v0.10.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Features
- added middleware and FromRequest for tenant and app scope info - (df2edb6) - Shubhranshu Sanjeev
- added multi-tenant support - (44e9b47) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- **(version)** v0.10.0 [skip ci] - (6d09f90) - Jenkins
#### Refactoring
- moved tables and types out of cac_v1 schema - (2fa26dd) - Shubhranshu Sanjeev

- - -

## v0.11.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
- fixed failing health check (x-tenant header not set) - (3a604b0) - Shubhranshu Sanjeev
#### Features
- multi-tenant support for client libraries - (f524388) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- **(version)** v0.11.0 [skip ci] - (7c429a7) - Jenkins

- - -

## v0.12.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
-  x-tenant header mandate removed for OPTIONS calls - (cb5a92f) - Ritick Madaan
#### Miscellaneous Chores
- **(version)** v0.12.0 [skip ci] - (1612123) - Jenkins

- - -

## v0.12.1 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
-  x-tenant header added for /config/resolve call in diff - (bbecd7b) - Ritick Madaan
-  added external crate to cocogitto config - (900fbb8) - Ritick Madaan
#### Miscellaneous Chores
- **(version)** v0.12.1 [skip ci] - (9f18f58) - Jenkins

- - -

## v0.13.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
- make sure envs with defaults prevent failure - (507e08c) - Kartik Gajendra
#### Features
-  integrate authorize middleware - (eccf091) - Kartik Gajendra
#### Miscellaneous Chores
- **(version)** v0.13.0 [skip ci] - (5aafc72) - Jenkins

- - -

## v0.14.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
- cac service to set last_modified header - (8ad81c3) - ankit.mahato
- Removing acceptance of override_keys in experiment create/update - (ac5fe3c) - ankit.mahato
#### Miscellaneous Chores
- **(version)** v0.14.0 [skip ci] - (01d94ae) - Jenkins

- - -

## v0.14.1 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
- failing build due to update of schema.rs file - (da2a4e3) - Shubhranshu Sanjeev
#### Features
- added frontend crate,combined frontend and backend binaries () - (345b4af) - Saurav Suman
- added format check in the JenkinsFile() - (3611ef3) - Saurav Suman
#### Miscellaneous Chores
- **(version)** v0.14.1 [skip ci] - (00137db) - Jenkins
-  move dependencies to workspaces - (bb87c89) - Kartik Gajendra

- - -

## v0.15.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
- add different auth types for exp requests to CAC - (7730c41) - Kartik Gajendra
#### Miscellaneous Chores
- **(version)** v0.15.0 [skip ci] - (3afb567) - Jenkins

- - -

## v0.15.1 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
- fixed deployment ConfigNotFound failure - (c33b445) - Shubhranshu Sanjeev
#### Features
- update default keys - (c13b140) - ankit.mahato
#### Miscellaneous Chores
- **(version)** v0.15.1 [skip ci] - (9cac541) - Jenkins

- - -

## v0.16.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
-  sort json while context creation - (78a03e2) - Pratik Mishra
#### Miscellaneous Chores
- **(version)** v0.16.0 [skip ci] - (3d037dc) - Jenkins

- - -

## v0.16.1 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
- allow ramp 0 - (892f29c) - Kartik Gajendra
#### Miscellaneous Chores
- **(version)** v0.16.1 [skip ci] - (ba87783) - Jenkins

- - -

## v0.16.2 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
-  - Cac client library changes to consume backend api response - (bec2f1b) - Prasanna P
-  fix json schema validation - (6a68f71) - ankit.mahato
-  array validation for in condition - (8e7db1f) - Pratik Mishra
#### Miscellaneous Chores
- **(version)** v0.16.2 [skip ci] - (ce7e53e) - Jenkins

- - -

## v0.16.3 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
- context parsing - (ba2d33b) - Kartik Gajendra
- fixed experiment list page feedback - (41905a6) - Shubhranshu Sanjeev
- frontend multi-tenancy support + config and dimension page - (adafb49) - Shubhranshu Sanjeev
- minor docs update - (6e9b329) - Kartik Gajendra
- fixed ci-test to support multi-tenant setup - (cad8e7f) - Shubhranshu Sanjeev
#### Features
- ui for cac and exp - (7e87921) - Shubhranshu Sanjeev
- experiment UI - (1f4bd77) - Kartik Gajendra
- added experiment-list page - (d00db1c) - Shubhranshu Sanjeev
- working experiments page - (bf94e31) - Kartik Gajendra
- added default config page - (c8bcfcc) - Saurav Suman
- added default config and override screen - (27f1236) - Saurav Suman
- working experiments page - (ba2eb38) - Kartik Gajendra
- fixed theme + ui changes + form validation + context validation error handling - (6376194) - Saurav Suman
- experiment create form - (588d15a) - Shubhranshu Sanjeev
- fixed experiment suspense block , added generic button - (93a3abd) - Saurav Suman
- working resolve page - (27fbb99) - Kartik Gajendra
#### Miscellaneous Chores
- **(version)** v0.16.3 [skip ci] - (e4d4ef8) - Jenkins
- formatted code + cleanup - (ec7aecc) - Shubhranshu Sanjeev
#### Refactoring
- fixed warnings, added redirection for home page and script for setting up the project - (0ee0618) - Saurav Suman
- fixed warnings, added redirection for home page and script for setting up the project - (1d710f3) - Saurav Suman

- - -

## v0.17.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
- fixed build failure due to rust-version - (55ac190) - Shubhranshu Sanjeev
- frontend build process - (c937b68) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- **(version)** v0.17.0 [skip ci] - (5759a15) - Jenkins

- - -

## v0.17.1 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
- error resolving pages with internal call to server - (46ca970) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- **(version)** v0.17.1 [skip ci] - (06bcd1a) - Jenkins

- - -

## v0.17.2 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
- fixed host resolve issue for internal calls in SSR. - (5c0ebc3) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- **(version)** v0.17.2 [skip ci] - (cf9ab88) - Jenkins

- - -

## v0.17.3 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
- added partitions for 2025 and 2026 for audit table - (917aa73) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- **(version)** v0.17.3 [skip ci] - (c0f69aa) - Jenkins

- - -

## v0.17.4 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
- getting api hostname from env for frontend - (8141696) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- **(version)** v0.17.4 [skip ci] - (cebd182) - Jenkins

- - -

## v0.18.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
- added partitions for audit_log table in cac schema - (7d9c5fe) - Shubhranshu Sanjeev
#### Continuous Integration
- added NY ECR registry push to Jenkins - (0ca6832) - Shubhranshu Sanjeev
- removing test tenant sqls after ci-test - (811bbae) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- **(version)** v0.18.0 [skip ci] - (be4b637) - Jenkins

- - -

## v0.18.1 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Features
-  added authentication header for frontend apis - (e1ab466) - Saurav Suman
#### Miscellaneous Chores
- **(version)** v0.18.1 [skip ci] - (c52344e) - Jenkins

- - -

## v0.19.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
- jenkinsfile now sends build alerts in channel - (ee2254f) - Kartik
- fixing error message for experiment create and bulk context api - (3d64762) - Jenkins
#### Continuous Integration
- pushing cac image to NY sbx ECR - (ce58f47) - Shubhranshu Sanjeev
#### Documentation
- () added setup instruction - (e528e10) - Saurav Suman
-  context aware config docs - (e3a338d) - Kartik
-  add intro doc and features - (5a0f48a) - Natarajan Kannan
-  add intro doc and features - (eb1fba3) - Natarajan Kannan
-  add intro doc and features - (28e0694) - Natarajan Kannan
#### Features
-  client-integration-doc - (f0993c7) - Pratik Mishra
#### Miscellaneous Chores
- **(version)** v0.19.0 [skip ci] - (d407b5a) - Jenkins
- experimentation docs first cut - (2b54792) - Shubhranshu Sanjeev
-  autodeploy to sbx - (7f21af5) - Kartik

- - -

## v0.20.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
-  better logging - (660c642) - Kartik
#### Miscellaneous Chores
- **(version)** v0.20.0 [skip ci] - (975ecf7) - Jenkins

- - -

## v0.20.1 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Features
- support for service prefix - (19223be) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- **(version)** v0.20.1 [skip ci] - (9915af9) - Jenkins

- - -

## v0.21.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Miscellaneous Chores
- **(version)** v0.21.0 [skip ci] - (2ac33ed) - Jenkins

- - -

## v0.22.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Features
-  js eval with node exec - (3786b69) - Pratik Mishra
#### Miscellaneous Chores
- **(version)** v0.22.0 [skip ci] - (f4e4a31) - Jenkins

- - -

## v0.23.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
-  Do not remove keys with null value on merge - (06974f1) - ayush.jain@juspay.in
#### Features
-  CRUD APIs for function validator - (0bb238e) - ankit.mahato
#### Miscellaneous Chores
- **(version)** v0.23.0 [skip ci] - (267dc4a) - Jenkins

- - -

## v0.24.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
-  fix copy of experiment ID - (f039558) - Kartik
#### Miscellaneous Chores
- **(version)** v0.24.0 [skip ci] - (372c9de) - Jenkins

- - -

## v0.24.1 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
- returning error response if CAC call not 200 - (8ccb942) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- **(version)** v0.24.1 [skip ci] - (f2a437b) - Jenkins

- - -

## v0.24.2 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
-  transpose columns in single experiment page for variants - (f3bfcc0) - Kartik
#### Features
- autodeploy - (fb5bb46) - Kartik
#### Miscellaneous Chores
- **(version)** v0.24.2 [skip ci] - (aa56d9b) - Jenkins

- - -

## v0.25.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
- autodeploy - (a5e50d6) - Kartik
#### Features
-  added test,publish api for functions - (3910299) - Pratik Mishra
#### Miscellaneous Chores
- **(version)** v0.25.0 [skip ci] - (6667206) - Jenkins

- - -

## v0.26.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Features
-  add node to app directory - (5b51c63) - Pratik Mishra
#### Miscellaneous Chores
- **(version)** v0.26.0 [skip ci] - (e6d8ed2) - Jenkins

- - -

## v0.27.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Features
-  Replace merge-strategy option for resolve/eval - (032dc3a) - ayush.jain@juspay.in
#### Miscellaneous Chores
- **(version)** v0.27.0 [skip ci] - (c678efe) - Jenkins

- - -

## v0.28.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
- added frontend crate to cog.toml - (fa09a96) - Shubhranshu Sanjeev
#### Features
-  url click and text wrap fixes - (6553e30) - Saurav CV
#### Miscellaneous Chores
- **(version)** v0.28.0 [skip ci] - (75396e5) - Jenkins

- - -

## v0.29.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Features
- added CAC language support - (096d683) - Kartik Gajendra
- support more operations - (2c3d0dc) - Kartik Gajendra
#### Miscellaneous Chores
- **(version)** v0.29.0 [skip ci] - (53a20b5) - Jenkins

- - -

## v0.30.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Miscellaneous Chores
- **(version)** v0.30.0 [skip ci] - (038c898) - Jenkins

- - -

## v0.30.1 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Features
-  Added function validation for context and default_config - (b3ad959) - ankit.mahato
#### Miscellaneous Chores
- **(version)** v0.30.1 [skip ci] - (fd38192) - Jenkins

- - -

## v0.31.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
-  moved to AWS Public ECR for docker images - (7067b36) - Shubhranshu Sanjeev
-  update cargo.lock - (6897659) - Kartik
#### Documentation
-  add intro doc and features - (39bd090) - Natarajan Kannan
#### Features
-  haskell client for superposition - (0ce569b) - Kartik
#### Miscellaneous Chores
- **(version)** v0.31.0 [skip ci] - (c602b93) - Jenkins

- - -

## v0.32.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
-  added routes without service prefix for b/w compatibility - (290b326) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- **(version)** v0.32.0 [skip ci] - (ae88083) - Jenkins

- - -

## v0.32.1 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
-  auto-create variantIds dimension - (d868254) - ankit.mahato
-  Functions bug fixes - (396c68d) - ankit.mahato
#### Miscellaneous Chores
- **(version)** v0.32.1 [skip ci] - (b177bc9) - Jenkins

- - -

## v0.32.2 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Miscellaneous Chores
- **(version)** v0.32.2 [skip ci] - (f4dba31) - Jenkins

- - -

## v0.33.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Features
-  Filter Config by prefix - (0c61362) - ankit.mahato
#### Miscellaneous Chores
- **(version)** v0.33.0 [skip ci] - (693a023) - Jenkins

- - -

## v0.34.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
-  filter config fix - (6201a2d) - ankit.mahato
#### Miscellaneous Chores
- **(version)** v0.34.0 [skip ci] - (bd807ec) - Jenkins

- - -

## v0.34.1 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
-  JS validator functions to take config value and key - (aad2b7a) - ankit.mahato
#### Miscellaneous Chores
- **(version)** v0.34.1 [skip ci] - (9a458c6) - Jenkins

- - -

## v0.34.2 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Documentation
-  add intro doc and features - (85751e0) - Natarajan Kannan
#### Features
-  haskell client for superposition - (af6ea75) - Kartik
-  client interface improvements - (659d384) - Kartik
- -js-secure-sandbox - (53af262) - Pratik Mishra
#### Miscellaneous Chores
- **(version)** v0.34.2 [skip ci] - (7b3c3e7) - Jenkins
- rename superposition to experimentation - (4e2be48) - Kartik

- - -

## v0.35.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Miscellaneous Chores
- **(version)** v0.35.0 [skip ci] - (233bf7d) - Jenkins

- - -

## v0.36.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
-  add path to node_modules - (606947c) - Pratik Mishra
#### Miscellaneous Chores
- **(version)** v0.36.0 [skip ci] - (cdc46ce) - Jenkins

- - -

## v0.36.1 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Features
-  added new result, error type and error macros - (d170727) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- **(version)** v0.36.1 [skip ci] - (ebba320) - Jenkins
#### Refactoring
-  refactored service to use new error type and better error handling - (a99f4af) - Shubhranshu Sanjeev

- - -

## v0.37.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Miscellaneous Chores
- **(version)** v0.37.0 [skip ci] - (4f9f9a6) - Jenkins

- - -

## v0.38.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
-  added service-prefix to functions endpoints - (42881c8) - ankit.mahato
#### Miscellaneous Chores
- **(version)** v0.38.0 [skip ci] - (9208b3e) - Jenkins

- - -

## v0.38.1 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
-  function route fix - (6735635) - Pratik Mishra
#### Miscellaneous Chores
- **(version)** v0.38.1 [skip ci] - (ea982bf) - Jenkins

- - -

## v0.38.2 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
-  fixed error in client - (5db1ba4) - ankit.mahato
#### Features
-  Add filter support to client - (0cc2e6d) - ankit.mahato
#### Miscellaneous Chores
- **(version)** v0.38.2 [skip ci] - (64560fb) - Jenkins

- - -

## v0.39.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
-  removed audit log middleware and reduced max db connection pool size to 2 - (bdfdaed) - Saurav Suman
#### Miscellaneous Chores
- **(version)** v0.39.0 [skip ci] - (3405b5d) - Jenkins

- - -

## v0.39.1 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Miscellaneous Chores
- **(version)** v0.39.1 [skip ci] - (9cf8f81) - Jenkins

- - -

## superposition_types-v0.1.0 - 2024-04-26
### Package updates
- cac_client bumped to cac_client-v0.11.0
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- service_utils bumped to service_utils-v0.15.0
- frontend bumped to frontend-v0.7.0
### Global changes
#### Bug Fixes
- post merge release tagging - (3b7e262) - Kartik
- run github merge action only on PR merge - (6c9eccf) - Kartik
- merge build setup - (761a69b) - Kartik
#### Features
- add auth_type so this can be used when making API calls - (4ae27f2) - Kartik
- ready for open source! - (5f7af15) - Kartik
#### Miscellaneous Chores
- **(version)** v0.40.0 [skip ci] - (cdfe3f4) - Jenkins
- **(version)** v0.41.0 [skip ci] - (b1462bc) - Superposition Bot
- open source superposition - (cbd5b6f) - Kartik
- add PR testing workflows (#6) - (6085588) - Datron
- update merge workflow to work (#8) - (15840e7) - Datron

- - -

## v0.41.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
- corrected env for DB_PASSWORD and default for AWS_REGION - (9770384) - Ritick Madaan
- improved log for env not found - (dfd4528) - Ritick Madaan
- base64 decoding kms cypher - (fb19e0c) - Ritick Madaan
- changed host to 0.0.0.0 - (58c4444) - Ritick Madaan
-  dimension Table scaffolding, PUT<>DELETE apis `/dimension` - (a57be1e) - Ritick Madaan
- storing pre calculated priority in contexts table - (d71107e) - Ritick Madaan
- removed delete dimension api - (9f3a771) - Ritick Madaan
-  DB password URI encoded. - (b52a121) - Shrey Bana
-  moved override inside contexts table - (83987a0) - Ritick Madaan
-  removed unecessary and wrap over conditions - (d31cb14) - Ritick Madaan
-  added search path for schema in database_url - (b649c9d) - Ritick Madaan
-  database schema url - (0c4fa3e) - Ritick Madaan
-  added moved tables to cac_v1 schema - (3802d7f) - Ritick Madaan
-  enabled override updates in PUT /context by deep_merge - (141455b) - Ritick Madaan
-  moved dimension_type to cac_v1 schema - (f62beaf) - Ritick Madaan
-  fixed ordering of /context endpoints - (e84811c) - Ritick Madaan
-  updated response-type of /context/bulk-operations api - (9fd2634) - Ritick Madaan
- fixed context overlap check logic - (c432c0d) - Shubhranshu Sanjeev
- added last_modified column and indexes - (aa7690a) - Shubhranshu Sanjeev
- moved tables and types under cac_v1 schema - (023f260) - Shubhranshu Sanjeev
- calling cac apis for creating context - (0cbdcb9) - Shubhranshu Sanjeev
-  updated last_modified in ramp - (9ed8a88) - ankit.mahato
-  minor fixes for exp client - (1281ec7) - Kartik Gajendra
- removed traffic-percentage from experiment create request - (ce297de) - Shubhranshu Sanjeev
-  added total items to list API response - (f051a02) - Kartik Gajendra
- using audit log tstamp for checking last-modified - (4f7e0e2) - Shubhranshu Sanjeev
-  sorting same priority contexts with created_at - (9cebcae) - Ritick Madaan
-  allowing cug users to fall under test variants - (f1b420d) - Ritick Madaan
-  removed unwanted parameter to prevent warning - (7446853) - Ritick Madaan
- added middleware to insert version in response headers - (f5a3f74) - Shubhranshu Sanjeev
-  logged env variable's value before kms decrypting - (532a6d2) - Ritick Madaan
#### Build system
-  installing ca-certificates for ssl verification - (91b5cc9) - Ritick Madaan
- added version tag to docker images - (6b06dfe) - Shubhranshu Sanjeev
#### Continuous Integration
-  Created pipeline for automated-deployment - (7dd03a8) - Shrey Bana
-  Commented out prod docker push. - (8bf3a27) - Shrey Bana
-  Enabling production docker image push. - (d0ca79d) - Shrey Bana
- moved nixpkgs to nixos-22.11 as the unstable one had broken rustfmt - (8e308ab) - Ritick Madaan
-  Upgraded nixos to 23.05. - (b99f5d6) - Shrey Bana
- automated newman test setup - (ee76ab5) - Natarajan Kannan
-  switch to using newman - (cac419c) - Natarajan Kannan
- regenerated schema.patch with latest schema.rs - (73a4eff) - Ritick Madaan
-  enabling tests in pr builds - (40a9a07) - Ritick Madaan
-  udpated `docker container ls` filter - (b985286) - Ritick Madaan
- fix newman dev dependency ref - (131f37c) - Natarajan Kannan
- added postman collection for experimentation-platform - (4b8fdfc) - Shubhranshu Sanjeev
- added cocogitto config for automatic versioning - (caa805f) - Shubhranshu Sanjeev
#### Features
- context/add api along with db setup - (b04ac3f) - Ritick Madaan
-  Added context fetch API - (f8e49b0) - Shrey Bana
-  Added health-check endpoint. - (b8e0b0d) - Shrey Bana
-  added localstack setup along with kms - (18342c8) - Ritick Madaan
- /default-config/<key> PUT api - (acb9df3) - Ritick Madaan
- GET /config api - (0cdbc3c) - Ritick Madaan
-  Added context list API - (a89b485) - Shrey Bana
-  Added support for validation via JSON schema. - (c542af8) - Shrey Bana
-  rust library - (8004510) - Ritick Madaan
-  removed properties constraint on objects in schema - (ba3020e) - Ritick Madaan
-  Added authentication. - (7120d61) - Shrey Bana
-  added DELETE /context/{ctx_id} api - (53ae341) - Ritick Madaan
-  PUT /context/move/{ctx_id} api - (c838b96) - Ritick Madaan
- added 304 <> last-modified for GET /config - (76cabf4) - Saurav Suman
-  added list experiments API - (bd20bdf) - Kartik Gajendra
- added conclude functionality for experiments - (e7e5eba) - Shubhranshu Sanjeev
-  added experimentation client with few fixes - (be0b70f) - Kartik Gajendra
-  add support for last - (34ba4d4) - Kartik Gajendra
- added log table for all cac_v1 tables - (6115759) - Shubhranshu Sanjeev
-  added support for CUG in super position client - (7caaabc) - Kartik Gajendra
-  Added Catch all error type for robust error handling - (c86f6a7) - Kartik Gajendra
-  Added Catch all error type for robust error handling - (f5a0d59) - Kartik Gajendra
#### Refactoring
-  moved db related modules to db crate - (0ed6bbd) - Ritick Madaan
-  removed old contexts table - (e07db3c) - Ritick Madaan
- moved AppState & utility fx to new crate - (b8d5dce) - Shubhranshu Sanjeev
- moved cac to cargo workspaces - (afbf722) - Shubhranshu Sanjeev
- moved fetching db connection in FromRequest trait impl - (c77a2c2) - Shubhranshu Sanjeev
- improvements to APIs - (91c2466) - Shubhranshu Sanjeev
#### Tests
- fix newman version used in tests - (42b9d77) - Natarajan Kannan
- update to latest newman that handles top level events and body lang type - (509ee50) - Natarajan Kannan

- - -

## v0.1.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
- moved git init to separate stage - (154a601) - Shubhranshu Sanjeev
-  cleaned up Dockerfile - (664770b) - Ritick Madaan
#### Features
-  implemented tracing-actix-web for logging - (551968e) - Ritick Madaan
#### Miscellaneous Chores
- **(version)** v0.1.0 [skip ci] - (bfbc1f5) - Jenkins

- - -

## v0.2.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Features
-  add audit log search endpoint - (16d6f7b) - Kartik Gajendra
#### Miscellaneous Chores
- **(version)** v0.2.0 [skip ci] - (f3fd257) - Jenkins
#### Revert
- Revert "fix:  logged env variable's value before kms decrypting" - (a7c24d0) - Ritick Madaan

- - -

## v0.3.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Features
-  added pod information in response headers and logs - (23c9e67) - Kartik Gajendra
#### Miscellaneous Chores
- **(version)** v0.3.0 [skip ci] - (a347bba) - Jenkins

- - -

## v0.4.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
- fixed setting env in docker image - (056b45b) - Shubhranshu Sanjeev
#### Continuous Integration
-  updated integ AP tracker curl with new version - (f92d148) - Ritick Madaan
#### Miscellaneous Chores
- **(version)** v0.4.0 [skip ci] - (d76376d) - Jenkins

- - -

## v0.4.1 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Features
-  record the chosen variant after conclude - (a237254) - Kartik Gajendra
#### Miscellaneous Chores
- **(version)** v0.4.1 [skip ci] - (8c3fb61) - Jenkins

- - -

## v0.5.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
- trimming newline character from version string - (a37f5d2) - Shubhranshu Sanjeev
#### Continuous Integration
- deleting postgres's docker image on every test - (47b615e) - Ritick Madaan
#### Miscellaneous Chores
- **(version)** v0.5.0 [skip ci] - (53b3143) - Jenkins

- - -

## v0.5.1 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
- fixed random timeouts in internal http calls to CAC - (dd17746) - Shubhranshu Sanjeev
- failed build due to untracked schema.rs file changes - (c249efc) - Shubhranshu Sanjeev
-  eval param fix - (a9c5b04) - Pratik Mishra
#### Continuous Integration
- added 20 minutes timeout on pipeline - (7cda606) - Shubhranshu Sanjeev
#### Features
- Schema addition for Dimension values - (48fc4ad) - Prasanna P
-  Adding generic eval - (b63c753) - Pratik Mishra
#### Miscellaneous Chores
- **(version)** v0.5.1 [skip ci] - (cd0cb60) - Jenkins

- - -

## v0.6.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
-  patching overrides on default-config instead of merge - (8e311c7) - Ritick Madaan
#### Miscellaneous Chores
- **(version)** v0.6.0 [skip ci] - (a5f8757) - Jenkins

- - -

## v0.6.1 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Continuous Integration
- **(flake.nix)** pin nodejs version to 18 in flake - (df0adf0) - Natarajan Kannan
#### Features
-  cors middleware attached - (6a5fe1f) - Ritick Madaan
-  added dashboard auth middleware - (5ca8766) - Kartik Gajendra
#### Miscellaneous Chores
- **(version)** v0.6.1 [skip ci] - (5c471b2) - Jenkins

- - -

## v0.7.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
- ssh.bitbucket.juspay.net added to known hosts in docker bulid - (3da23da) - Ritick Madaan
-  add user struct in delete context API - (1ac80c4) - Kartik Gajendra
#### Miscellaneous Chores
- **(version)** v0.7.0 [skip ci] - (150a7be) - Jenkins

- - -

## v0.7.1 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Features
- server's keep-alive time and db connection pool max size made configurable - (585a15c) - Ritick Madaan
#### Miscellaneous Chores
- **(version)** v0.7.1 [skip ci] - (a9e50be) - Jenkins
- database migration for dimensions table - (1337d34) - Ritick Madaan

- - -

## v0.8.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
-  add migration for changing default_configs_keys - (7b6ae82) - Pratik Mishra
#### Miscellaneous Chores
- **(version)** v0.8.0 [skip ci] - (ee5e00c) - Jenkins

- - -

## v0.8.1 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
- validating override_keys for unique entries - (400f3f0) - Shubhranshu Sanjeev
#### Features
- support to update experiment override_keys and variants - (71c7a25) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- **(version)** v0.8.1 [skip ci] - (189c369) - Jenkins
#### Refactoring
- resolved comments - (4422c83) - Shubhranshu Sanjeev
#### Tests
- added tests for experiment helper fnxs - (7a60eea) - Shubhranshu Sanjeev
- added postman test for update override_keys api - (38bc0e9) - Shubhranshu Sanjeev

- - -

## v0.9.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
-  add all variants in manifest - (ef7a37d) - Pratik Mishra
#### Miscellaneous Chores
- **(version)** v0.9.0 [skip ci] - (dc2db17) - Jenkins

- - -

## v0.9.1 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Features
-  - Dimension value schema validation on context-addition - (2c810f7) - Prasanna P
#### Miscellaneous Chores
- **(version)** v0.9.1 [skip ci] - (eddd8a4) - Jenkins

- - -

## v0.10.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Features
- added middleware and FromRequest for tenant and app scope info - (df2edb6) - Shubhranshu Sanjeev
- added multi-tenant support - (44e9b47) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- **(version)** v0.10.0 [skip ci] - (6d09f90) - Jenkins
#### Refactoring
- moved tables and types out of cac_v1 schema - (2fa26dd) - Shubhranshu Sanjeev

- - -

## v0.11.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
- fixed failing health check (x-tenant header not set) - (3a604b0) - Shubhranshu Sanjeev
#### Features
- multi-tenant support for client libraries - (f524388) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- **(version)** v0.11.0 [skip ci] - (7c429a7) - Jenkins

- - -

## v0.12.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
-  x-tenant header mandate removed for OPTIONS calls - (cb5a92f) - Ritick Madaan
#### Miscellaneous Chores
- **(version)** v0.12.0 [skip ci] - (1612123) - Jenkins

- - -

## v0.12.1 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
-  x-tenant header added for /config/resolve call in diff - (bbecd7b) - Ritick Madaan
-  added external crate to cocogitto config - (900fbb8) - Ritick Madaan
#### Miscellaneous Chores
- **(version)** v0.12.1 [skip ci] - (9f18f58) - Jenkins

- - -

## v0.13.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
- make sure envs with defaults prevent failure - (507e08c) - Kartik Gajendra
#### Features
-  integrate authorize middleware - (eccf091) - Kartik Gajendra
#### Miscellaneous Chores
- **(version)** v0.13.0 [skip ci] - (5aafc72) - Jenkins

- - -

## v0.14.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
- cac service to set last_modified header - (8ad81c3) - ankit.mahato
- Removing acceptance of override_keys in experiment create/update - (ac5fe3c) - ankit.mahato
#### Miscellaneous Chores
- **(version)** v0.14.0 [skip ci] - (01d94ae) - Jenkins

- - -

## v0.14.1 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
- failing build due to update of schema.rs file - (da2a4e3) - Shubhranshu Sanjeev
#### Features
- added frontend crate,combined frontend and backend binaries () - (345b4af) - Saurav Suman
- added format check in the JenkinsFile() - (3611ef3) - Saurav Suman
#### Miscellaneous Chores
- **(version)** v0.14.1 [skip ci] - (00137db) - Jenkins
-  move dependencies to workspaces - (bb87c89) - Kartik Gajendra

- - -

## v0.15.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
- add different auth types for exp requests to CAC - (7730c41) - Kartik Gajendra
#### Miscellaneous Chores
- **(version)** v0.15.0 [skip ci] - (3afb567) - Jenkins

- - -

## v0.15.1 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
- fixed deployment ConfigNotFound failure - (c33b445) - Shubhranshu Sanjeev
#### Features
- update default keys - (c13b140) - ankit.mahato
#### Miscellaneous Chores
- **(version)** v0.15.1 [skip ci] - (9cac541) - Jenkins

- - -

## v0.16.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
-  sort json while context creation - (78a03e2) - Pratik Mishra
#### Miscellaneous Chores
- **(version)** v0.16.0 [skip ci] - (3d037dc) - Jenkins

- - -

## v0.16.1 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
- allow ramp 0 - (892f29c) - Kartik Gajendra
#### Miscellaneous Chores
- **(version)** v0.16.1 [skip ci] - (ba87783) - Jenkins

- - -

## v0.16.2 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
-  - Cac client library changes to consume backend api response - (bec2f1b) - Prasanna P
-  fix json schema validation - (6a68f71) - ankit.mahato
-  array validation for in condition - (8e7db1f) - Pratik Mishra
#### Miscellaneous Chores
- **(version)** v0.16.2 [skip ci] - (ce7e53e) - Jenkins

- - -

## v0.16.3 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
- context parsing - (ba2d33b) - Kartik Gajendra
- fixed experiment list page feedback - (41905a6) - Shubhranshu Sanjeev
- frontend multi-tenancy support + config and dimension page - (adafb49) - Shubhranshu Sanjeev
- minor docs update - (6e9b329) - Kartik Gajendra
- fixed ci-test to support multi-tenant setup - (cad8e7f) - Shubhranshu Sanjeev
#### Features
- ui for cac and exp - (7e87921) - Shubhranshu Sanjeev
- experiment UI - (1f4bd77) - Kartik Gajendra
- added experiment-list page - (d00db1c) - Shubhranshu Sanjeev
- working experiments page - (bf94e31) - Kartik Gajendra
- added default config page - (c8bcfcc) - Saurav Suman
- added default config and override screen - (27f1236) - Saurav Suman
- working experiments page - (ba2eb38) - Kartik Gajendra
- fixed theme + ui changes + form validation + context validation error handling - (6376194) - Saurav Suman
- experiment create form - (588d15a) - Shubhranshu Sanjeev
- fixed experiment suspense block , added generic button - (93a3abd) - Saurav Suman
- working resolve page - (27fbb99) - Kartik Gajendra
#### Miscellaneous Chores
- **(version)** v0.16.3 [skip ci] - (e4d4ef8) - Jenkins
- formatted code + cleanup - (ec7aecc) - Shubhranshu Sanjeev
#### Refactoring
- fixed warnings, added redirection for home page and script for setting up the project - (0ee0618) - Saurav Suman
- fixed warnings, added redirection for home page and script for setting up the project - (1d710f3) - Saurav Suman

- - -

## v0.17.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
- fixed build failure due to rust-version - (55ac190) - Shubhranshu Sanjeev
- frontend build process - (c937b68) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- **(version)** v0.17.0 [skip ci] - (5759a15) - Jenkins

- - -

## v0.17.1 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
- error resolving pages with internal call to server - (46ca970) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- **(version)** v0.17.1 [skip ci] - (06bcd1a) - Jenkins

- - -

## v0.17.2 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
- fixed host resolve issue for internal calls in SSR. - (5c0ebc3) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- **(version)** v0.17.2 [skip ci] - (cf9ab88) - Jenkins

- - -

## v0.17.3 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
- added partitions for 2025 and 2026 for audit table - (917aa73) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- **(version)** v0.17.3 [skip ci] - (c0f69aa) - Jenkins

- - -

## v0.17.4 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
- getting api hostname from env for frontend - (8141696) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- **(version)** v0.17.4 [skip ci] - (cebd182) - Jenkins

- - -

## v0.18.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
- added partitions for audit_log table in cac schema - (7d9c5fe) - Shubhranshu Sanjeev
#### Continuous Integration
- added NY ECR registry push to Jenkins - (0ca6832) - Shubhranshu Sanjeev
- removing test tenant sqls after ci-test - (811bbae) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- **(version)** v0.18.0 [skip ci] - (be4b637) - Jenkins

- - -

## v0.18.1 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Features
-  added authentication header for frontend apis - (e1ab466) - Saurav Suman
#### Miscellaneous Chores
- **(version)** v0.18.1 [skip ci] - (c52344e) - Jenkins

- - -

## v0.19.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
- jenkinsfile now sends build alerts in channel - (ee2254f) - Kartik
- fixing error message for experiment create and bulk context api - (3d64762) - Jenkins
#### Continuous Integration
- pushing cac image to NY sbx ECR - (ce58f47) - Shubhranshu Sanjeev
#### Documentation
- () added setup instruction - (e528e10) - Saurav Suman
-  context aware config docs - (e3a338d) - Kartik
-  add intro doc and features - (5a0f48a) - Natarajan Kannan
-  add intro doc and features - (eb1fba3) - Natarajan Kannan
-  add intro doc and features - (28e0694) - Natarajan Kannan
#### Features
-  client-integration-doc - (f0993c7) - Pratik Mishra
#### Miscellaneous Chores
- **(version)** v0.19.0 [skip ci] - (d407b5a) - Jenkins
- experimentation docs first cut - (2b54792) - Shubhranshu Sanjeev
-  autodeploy to sbx - (7f21af5) - Kartik

- - -

## v0.20.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
-  better logging - (660c642) - Kartik
#### Miscellaneous Chores
- **(version)** v0.20.0 [skip ci] - (975ecf7) - Jenkins

- - -

## v0.20.1 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Features
- support for service prefix - (19223be) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- **(version)** v0.20.1 [skip ci] - (9915af9) - Jenkins

- - -

## v0.21.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Miscellaneous Chores
- **(version)** v0.21.0 [skip ci] - (2ac33ed) - Jenkins

- - -

## v0.22.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Features
-  js eval with node exec - (3786b69) - Pratik Mishra
#### Miscellaneous Chores
- **(version)** v0.22.0 [skip ci] - (f4e4a31) - Jenkins

- - -

## v0.23.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
-  Do not remove keys with null value on merge - (06974f1) - ayush.jain@juspay.in
#### Features
-  CRUD APIs for function validator - (0bb238e) - ankit.mahato
#### Miscellaneous Chores
- **(version)** v0.23.0 [skip ci] - (267dc4a) - Jenkins

- - -

## v0.24.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
-  fix copy of experiment ID - (f039558) - Kartik
#### Miscellaneous Chores
- **(version)** v0.24.0 [skip ci] - (372c9de) - Jenkins

- - -

## v0.24.1 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
- returning error response if CAC call not 200 - (8ccb942) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- **(version)** v0.24.1 [skip ci] - (f2a437b) - Jenkins

- - -

## v0.24.2 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
-  transpose columns in single experiment page for variants - (f3bfcc0) - Kartik
#### Features
- autodeploy - (fb5bb46) - Kartik
#### Miscellaneous Chores
- **(version)** v0.24.2 [skip ci] - (aa56d9b) - Jenkins

- - -

## v0.25.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
- autodeploy - (a5e50d6) - Kartik
#### Features
-  added test,publish api for functions - (3910299) - Pratik Mishra
#### Miscellaneous Chores
- **(version)** v0.25.0 [skip ci] - (6667206) - Jenkins

- - -

## v0.26.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Features
-  add node to app directory - (5b51c63) - Pratik Mishra
#### Miscellaneous Chores
- **(version)** v0.26.0 [skip ci] - (e6d8ed2) - Jenkins

- - -

## v0.27.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Features
-  Replace merge-strategy option for resolve/eval - (032dc3a) - ayush.jain@juspay.in
#### Miscellaneous Chores
- **(version)** v0.27.0 [skip ci] - (c678efe) - Jenkins

- - -

## v0.28.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
- added frontend crate to cog.toml - (fa09a96) - Shubhranshu Sanjeev
#### Features
-  url click and text wrap fixes - (6553e30) - Saurav CV
#### Miscellaneous Chores
- **(version)** v0.28.0 [skip ci] - (75396e5) - Jenkins

- - -

## v0.29.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Features
- added CAC language support - (096d683) - Kartik Gajendra
- support more operations - (2c3d0dc) - Kartik Gajendra
#### Miscellaneous Chores
- **(version)** v0.29.0 [skip ci] - (53a20b5) - Jenkins

- - -

## v0.30.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Miscellaneous Chores
- **(version)** v0.30.0 [skip ci] - (038c898) - Jenkins

- - -

## v0.30.1 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Features
-  Added function validation for context and default_config - (b3ad959) - ankit.mahato
#### Miscellaneous Chores
- **(version)** v0.30.1 [skip ci] - (fd38192) - Jenkins

- - -

## v0.31.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
-  moved to AWS Public ECR for docker images - (7067b36) - Shubhranshu Sanjeev
-  update cargo.lock - (6897659) - Kartik
#### Documentation
-  add intro doc and features - (39bd090) - Natarajan Kannan
#### Features
-  haskell client for superposition - (0ce569b) - Kartik
#### Miscellaneous Chores
- **(version)** v0.31.0 [skip ci] - (c602b93) - Jenkins

- - -

## v0.32.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
-  added routes without service prefix for b/w compatibility - (290b326) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- **(version)** v0.32.0 [skip ci] - (ae88083) - Jenkins

- - -

## v0.32.1 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
-  auto-create variantIds dimension - (d868254) - ankit.mahato
-  Functions bug fixes - (396c68d) - ankit.mahato
#### Miscellaneous Chores
- **(version)** v0.32.1 [skip ci] - (b177bc9) - Jenkins

- - -

## v0.32.2 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Miscellaneous Chores
- **(version)** v0.32.2 [skip ci] - (f4dba31) - Jenkins

- - -

## v0.33.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Features
-  Filter Config by prefix - (0c61362) - ankit.mahato
#### Miscellaneous Chores
- **(version)** v0.33.0 [skip ci] - (693a023) - Jenkins

- - -

## v0.34.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
-  filter config fix - (6201a2d) - ankit.mahato
#### Miscellaneous Chores
- **(version)** v0.34.0 [skip ci] - (bd807ec) - Jenkins

- - -

## v0.34.1 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
-  JS validator functions to take config value and key - (aad2b7a) - ankit.mahato
#### Miscellaneous Chores
- **(version)** v0.34.1 [skip ci] - (9a458c6) - Jenkins

- - -

## v0.34.2 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Documentation
-  add intro doc and features - (85751e0) - Natarajan Kannan
#### Features
-  haskell client for superposition - (af6ea75) - Kartik
-  client interface improvements - (659d384) - Kartik
- -js-secure-sandbox - (53af262) - Pratik Mishra
#### Miscellaneous Chores
- **(version)** v0.34.2 [skip ci] - (7b3c3e7) - Jenkins
- rename superposition to experimentation - (4e2be48) - Kartik

- - -

## v0.35.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Miscellaneous Chores
- **(version)** v0.35.0 [skip ci] - (233bf7d) - Jenkins

- - -

## v0.36.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
-  add path to node_modules - (606947c) - Pratik Mishra
#### Miscellaneous Chores
- **(version)** v0.36.0 [skip ci] - (cdc46ce) - Jenkins

- - -

## v0.36.1 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Features
-  added new result, error type and error macros - (d170727) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- **(version)** v0.36.1 [skip ci] - (ebba320) - Jenkins
#### Refactoring
-  refactored service to use new error type and better error handling - (a99f4af) - Shubhranshu Sanjeev

- - -

## v0.37.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Miscellaneous Chores
- **(version)** v0.37.0 [skip ci] - (4f9f9a6) - Jenkins

- - -

## v0.38.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
-  added service-prefix to functions endpoints - (42881c8) - ankit.mahato
#### Miscellaneous Chores
- **(version)** v0.38.0 [skip ci] - (9208b3e) - Jenkins

- - -

## v0.38.1 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
-  function route fix - (6735635) - Pratik Mishra
#### Miscellaneous Chores
- **(version)** v0.38.1 [skip ci] - (ea982bf) - Jenkins

- - -

## v0.38.2 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
-  fixed error in client - (5db1ba4) - ankit.mahato
#### Features
-  Add filter support to client - (0cc2e6d) - ankit.mahato
#### Miscellaneous Chores
- **(version)** v0.38.2 [skip ci] - (64560fb) - Jenkins

- - -

## v0.39.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
-  removed audit log middleware and reduced max db connection pool size to 2 - (bdfdaed) - Saurav Suman
#### Miscellaneous Chores
- **(version)** v0.39.0 [skip ci] - (3405b5d) - Jenkins

- - -

## v0.39.1 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Miscellaneous Chores
- **(version)** v0.39.1 [skip ci] - (9cf8f81) - Jenkins

- - -

## superposition_types-v0.1.0 - 2024-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.14.0
- context_aware_config bumped to context_aware_config-v0.28.0
- cac_client bumped to cac_client-v0.11.0
- frontend bumped to frontend-v0.7.0
- service_utils bumped to service_utils-v0.15.0
### Global changes
#### Bug Fixes
- post merge release tagging - (3b7e262) - Kartik
#### Features
- add auth_type so this can be used when making API calls - (4ae27f2) - Kartik
- ready for open source! - (5f7af15) - Kartik
#### Miscellaneous Chores
- **(version)** v0.40.0 [skip ci] - (cdfe3f4) - Jenkins
- open source superposition - (cbd5b6f) - Kartik
- add PR testing workflows (#6) - (6085588) - Datron
- update merge workflow to work (#8) - (15840e7) - Datron

- - -

## v0.40.0 - 2024-04-18
### Package updates
- cac_client bumped to cac_client-v0.10.0
- external bumped to external-v0.5.0
- frontend bumped to frontend-v0.6.0
- context_aware_config bumped to context_aware_config-v0.27.0
- service_utils bumped to service_utils-v0.14.0
- experimentation_platform bumped to experimentation_platform-v0.13.0
### Global changes
#### Features
- ready for open source! - (b7d36be) - Kartik
- add auth_type so this can be used when making API calls - (0861310) - Kartik

- - -

## v0.39.1 - 2024-04-17
### Package updates
- cac_client bumped to cac_client-v0.9.1
- context-aware-config bumped to context-aware-config-v0.26.1
### Global changes

- - -

## v0.39.0 - 2024-04-16
### Package updates
- cac_client bumped to cac_client-v0.9.0
- context-aware-config bumped to context-aware-config-v0.26.0
### Global changes
#### Bug Fixes
- PICAF-26366 fixed error in client - (d1b1f03) - ankit.mahato
#### Features
- PICAF-26366 Add filter support to client - (f4c12c7) - ankit.mahato

- - -

## v0.38.2 - 2024-04-12
### Package updates
- frontend bumped to frontend-v0.5.1
### Global changes
#### Bug Fixes
- [PICAF-26529] function route fix - (aba54da) - Pratik Mishra

- - -

## v0.38.1 - 2024-04-10
### Package updates
- context-aware-config bumped to context-aware-config-v0.25.2
### Global changes

- - -

## v0.38.0 - 2024-04-10
### Package updates
- frontend bumped to frontend-v0.5.0
### Global changes

- - -

## v0.37.0 - 2024-04-10
### Package updates
- service-utils bumped to service-utils-v0.13.0
### Global changes
#### Features
- [PICAF-25423] added new result, error type and error macros - (e673fb1) - Shubhranshu Sanjeev
#### Refactoring
- [PICAF-26558] refactored service to use new error type and better error handling - (741f391) - Shubhranshu Sanjeev

- - -

## v0.36.1 - 2024-04-08
### Package updates
- context-aware-config bumped to context-aware-config-v0.25.1
### Global changes

- - -

## v0.36.0 - 2024-04-05
### Package updates
- frontend bumped to frontend-v0.4.0
### Global changes

- - -

## v0.35.0 - 2024-04-05
### Package updates
- context-aware-config bumped to context-aware-config-v0.25.0
- experimentation-platform bumped to experimentation-platform-v0.12.0
- frontend bumped to frontend-v0.3.0
- cac_client bumped to cac_client-v0.8.0
### Global changes
#### Documentation
- PICAF-25981: add intro doc and features - (64fa30f) - Natarajan Kannan
#### Features
- [PICAF-26101] client interface improvements - (d606cb1) - Kartik
- [PICAF-26126] haskell client for superposition - (651a66d) - Kartik
#### Miscellaneous Chores
- rename superposition to experimentation - (9efa3cb) - Kartik

- - -

## v0.34.2 - 2024-03-27
### Package updates
- context-aware-config bumped to context-aware-config-v0.24.2
### Global changes

- - -

## v0.34.1 - 2024-03-21
### Package updates
- context-aware-config bumped to context-aware-config-v0.24.1
### Global changes

- - -

## v0.34.0 - 2024-03-21
### Package updates
- context-aware-config bumped to context-aware-config-v0.24.0
- frontend bumped to frontend-v0.2.1
### Global changes

- - -

## v0.33.0 - 2024-03-21
### Package updates
- frontend bumped to frontend-v0.2.0
### Global changes

- - -

## v0.32.2 - 2024-03-20
### Package updates
- context-aware-config bumped to context-aware-config-v0.23.2
### Global changes
#### Bug Fixes
- PICAF-25598 auto-create variantIds dimension - (ee2e7dc) - ankit.mahato

- - -

## v0.32.1 - 2024-03-19
### Package updates
- context-aware-config bumped to context-aware-config-v0.23.1
### Global changes

- - -

## v0.32.0 - 2024-03-18
### Package updates
- experimentation-platform bumped to experimentation-platform-v0.11.0
- cac_client bumped to cac_client-v0.7.0
- superposition_client bumped to superposition_client-v0.5.0
### Global changes
#### Bug Fixes
- [PICAF-26428] update cargo.lock - (b7aa8b6) - Kartik
- [PICAF-26428] moved to AWS Public ECR for docker images - (26cd710) - Shubhranshu Sanjeev
#### Documentation
- PICAF-25981: add intro doc and features - (d09ba53) - Natarajan Kannan
#### Features
- [PICAF-26126] haskell client for superposition - (7106b56) - Kartik

- - -

## v0.31.0 - 2024-03-08
### Package updates
- external bumped to external-v0.4.0
- experimentation-platform bumped to experimentation-platform-v0.10.0
- service-utils bumped to service-utils-v0.12.0
- context-aware-config bumped to context-aware-config-v0.23.0
### Global changes

- - -

## v0.30.1 - 2024-03-07
### Package updates
- frontend bumped to frontend-v0.1.1
### Global changes

- - -

## v0.30.0 - 2024-03-06
### Package updates
- context-aware-config bumped to context-aware-config-v0.22.0
- caclang bumped to caclang-v0.1.0
### Global changes
#### Features
- support more operations - (4db2c31) - Kartik Gajendra
- added CAC language support - (c549384) - Kartik Gajendra

- - -

## v0.29.0 - 2024-03-06
### Package updates
- frontend bumped to frontend-v0.1.0
### Global changes
#### Bug Fixes
- added frontend crate to cog.toml - (c901e21) - Shubhranshu Sanjeev
#### Features
- PICAF-26266 url click and text wrap fixes - (643c54d) - Saurav CV

- - -

## v0.28.0 - 2024-03-04
### Package updates
- cac_client bumped to cac_client-v0.6.0
- context-aware-config bumped to context-aware-config-v0.21.0
### Global changes
#### Features
- PICAF-26185 Replace merge-strategy option for resolve/eval - (453cfb9) - ayush.jain@juspay.in

- - -

## v0.27.0 - 2024-03-04
### Package updates
- context-aware-config bumped to context-aware-config-v0.20.0
### Global changes
#### Features
- [PICAF-25877 add node to app directory - (9671875) - Pratik Mishra

- - -

## v0.26.0 - 2024-02-29
### Package updates
- context-aware-config bumped to context-aware-config-v0.19.0
### Global changes
#### Bug Fixes
- autodeploy - (43a4ada) - Kartik
#### Features
- [PICAF-25879] added test,publish api for functions - (050ab24) - Pratik Mishra

- - -

## v0.25.0 - 2024-02-28
### Package updates
- context-aware-config bumped to context-aware-config-v0.18.2
### Global changes
#### Bug Fixes
- [PICAF-26199] transpose columns in single experiment page for variants - (a1a8ac8) - Kartik
- [PICAF-26196] add traffic percentage to experiments table - (5fb0221) - Kartik
#### Features
- autodeploy - (94bc7c7) - Kartik

- - -

## v0.24.2 - 2024-02-27
### Package updates
- experimentation-platform bumped to experimentation-platform-v0.9.4
### Global changes

- - -

## v0.24.1 - 2024-02-26
### Package updates
- context-aware-config bumped to context-aware-config-v0.18.1
### Global changes
#### Bug Fixes
- [PICAF-26195] fix copy of experiment ID - (37e4c24) - Kartik

- - -

## v0.24.0 - 2024-02-22
### Package updates
- cac_client bumped to cac_client-v0.5.1
- context-aware-config bumped to context-aware-config-v0.18.0
### Global changes
#### Bug Fixes
- PICAF-26157 Do not remove keys with null value on merge - (bd3c196) - ayush.jain@juspay.in
#### Features
- PICAF-25876 CRUD APIs for function validator - (7c0c963) - ankit.mahato

- - -

## v0.23.0 - 2024-02-22
### Package updates
- context-aware-config bumped to context-aware-config-v0.17.0
### Global changes
#### Features
- [PICAF-25877] js eval with node exec - (adc9b19) - Pratik Mishra

- - -

## v0.22.0 - 2024-02-21
### Packages
- context-aware-config locked to context-aware-config-v0.16.0
- cac_client locked to cac_client-v0.5.0
- superposition_client locked to superposition_client-v0.4.0
- service-utils locked to service-utils-v0.11.0
- external locked to external-v0.3.0
- experimentation-platform locked to experimentation-platform-v0.9.3
### Global changes
#### Bug Fixes
- using SERVICE_NAME in is_server instead of SERVER_NAME(wrong var name) - (efe97f0) - Shubhranshu Sanjeev

- - -

## v0.21.0 - 2024-02-20
### Package updates
- context-aware-config bumped to context-aware-config-v0.16.0
- service-utils bumped to service-utils-v0.11.0
### Global changes
#### Features
- support for service prefix - (a2915b4) - Shubhranshu Sanjeev

- - -

## v0.20.1 - 2024-02-19
### Package updates
- context-aware-config bumped to context-aware-config-v0.15.2
### Global changes
#### Bug Fixes
- [PICAF-26004] better logging - (b3d1bc8) - Kartik

- - -

## v0.20.0 - 2024-02-15
### Package updates
- service-utils bumped to service-utils-v0.10.3
- experimentation-platform bumped to experimentation-platform-v0.9.3
- context-aware-config bumped to context-aware-config-v0.15.1
### Global changes
#### Bug Fixes
- fixing error message for experiment create and bulk context api - (bc0d7be) - Jenkins
- jenkinsfile now sends build alerts in channel - (2e04ca5) - Kartik
#### Continuous Integration
- pushing cac image to NY sbx ECR - (78c1b32) - Shubhranshu Sanjeev
#### Documentation
- PICAF-25981: add intro doc and features - (4dc6f19) - Natarajan Kannan
- PICAF-25981: add intro doc and features - (14a7f44) - Natarajan Kannan
- PICAF-25981: add intro doc and features - (0e11056) - Natarajan Kannan
- [PICAF-25981] context aware config docs - (ea04b76) - Kartik
- (PICAF-25983) added setup instruction - (e7d00d9) - Saurav Suman
#### Features
- [PICAF-25981] client-integration-doc - (bc4927d) - Pratik Mishra
- added bool, i64 and decimal in default config form - (fca1ca6) - Saurav Suman
#### Miscellaneous Chores
- [PICAF-25973] autodeploy to sbx - (b812140) - Kartik
- experimentation docs first cut - (d81aea4) - Shubhranshu Sanjeev

- - -

## v0.19.0 - 2024-01-31
### Package updates
- context-aware-config bumped to context-aware-config-v0.15.0
### Global changes
#### Bug Fixes
- refactored experiment page and fixed experiment edit flow - (b153486) - Shubhranshu Sanjeev
#### Features
- [PICAF-25817] added authentication header for frontend apis - (3f90592) - Saurav Suman

- - -

## v0.18.1 - 2024-01-29
### Package updates
- context-aware-config bumped to context-aware-config-v0.14.3
- experimentation-platform bumped to experimentation-platform-v0.9.2
### Global changes
#### Bug Fixes
- added partitions for audit_log table in cac schema - (d771050) - Shubhranshu Sanjeev
#### Continuous Integration
- removing test tenant sqls after ci-test - (d1e42db) - Shubhranshu Sanjeev
- added NY ECR registry push to Jenkins - (51995ae) - Shubhranshu Sanjeev

- - -

## v0.18.0 - 2024-01-22
### Packages
- experimentation-platform locked to experimentation-platform-v0.9.1
- cac_client locked to cac_client-v0.5.0
- external locked to external-v0.3.0
- service-utils locked to service-utils-v0.10.2
- experimentation_client locked to experimentation_client-v0.4.0
- context-aware-config locked to context-aware-config-v0.14.2
### Global changes
#### Bug Fixes
- getting api hostname from env for frontend - (837899d) - Shubhranshu Sanjeev
#### Features
- added between in frontend - (0eb60e5) - Akhilesh Bhadauriya

- - -

## v0.17.4 - 2024-01-22
### Package updates
- experimentation-platform bumped to experimentation-platform-v0.9.1
### Global changes

- - -

## v0.17.3 - 2024-01-22
### Package updates
- service-utils bumped to service-utils-v0.10.2
### Global changes
#### Bug Fixes
- fixed host resolve issue for internal calls in SSR. - (3cc9d6e) - Shubhranshu Sanjeev

- - -

## v0.17.2 - 2024-01-18
### Package updates
- service-utils bumped to service-utils-v0.10.1
- context-aware-config bumped to context-aware-config-v0.14.2
### Global changes
#### Bug Fixes
- error resolving pages with internal call to server - (084d08b) - Shubhranshu Sanjeev
- refactored DefaultConfig component + fixed edit flow - (f2d38cc) - Shubhranshu Sanjeev
- fixed dimension form edit flow + fixed table component CellFormatter to accept move closures - (9c3a364) - Shubhranshu Sanjeev
#### Refactoring
- using snake case for component fxn names - (19e9aca) - Shubhranshu Sanjeev

- - -

## v0.17.1 - 2024-01-12
### Package updates
- context-aware-config bumped to context-aware-config-v0.14.1
### Global changes
#### Bug Fixes
- frontend build process - (cbdad01) - Shubhranshu Sanjeev
- fixed build failure due to rust-version - (f689597) - Shubhranshu Sanjeev

- - -

## v0.17.0 - 2024-01-04
### Package updates
- experimentation-platform bumped to experimentation-platform-v0.9.0
- cac_client bumped to cac_client-v0.5.0
- context-aware-config bumped to context-aware-config-v0.14.0
- service-utils bumped to service-utils-v0.10.0
### Global changes
#### Bug Fixes
- fixed tenant hydration bug - (cf0e633) - Saurav Suman
- fixed ci-test to support multi-tenant setup - (916b75d) - Shubhranshu Sanjeev
- cleanup code - (4820f31) - Kartik Gajendra
- minor docs update - (04ab586) - Kartik Gajendra
- UI fixes for demo - (4927766) - Kartik Gajendra
- frontend multi-tenancy support + config and dimension page - (a1689a1) - Shubhranshu Sanjeev
- fixed experiment list page feedback - (f406264) - Shubhranshu Sanjeev
- context parsing - (d46ca42) - Kartik Gajendra
- resolve UI bugs - (98695a8) - Kartik Gajendra
- dimensions page updates - (5220b36) - ankit.mahato
#### Features
- added validation inside default config form , formatted dates , added disable feature of edit - (cacf20f) - Saurav Suman
- resolve page with unified UI - (e84eb41) - Kartik Gajendra
- working resolve page - (803dfbd) - Kartik Gajendra
- fixed experiment suspense block , added generic button - (117bfc8) - Saurav Suman
- experiment create form - (91371c0) - Shubhranshu Sanjeev
- fixed theme + ui changes + form validation + context validation error handling - (6cf5929) - Saurav Suman
- working resolve page - (81c83d4) - Kartik Gajendra
- added state changes in the form - (b64a227) - Saurav Suman
- testing create form - (d0a5aea) - Kartik Gajendra
- working experiments page - (81b17dc) - Kartik Gajendra
- experiment UI - (72e19e6) - Kartik Gajendra
- added default config and override screen - (cd4267e) - Saurav Suman
- added default config page - (95e909d) - Saurav Suman
- working experiments page - (9a1d74c) - Kartik Gajendra
- override and context form - (553e3ad) - Shubhranshu Sanjeev
- dimensions - (c5e94fa) - ankit.mahato
- added experiment-list page - (ee462fd) - Shubhranshu Sanjeev
- experiment UI - (24e1b56) - Kartik Gajendra
- ui for cac and exp - (41f884f) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- formatted code + cleanup - (6d4874b) - Shubhranshu Sanjeev
- formatted frontend code - (70f873f) - Shubhranshu Sanjeev
#### Refactoring
- fixed warnings, added redirection for home page and script for setting up the project - (a9ee3bd) - Saurav Suman
- fixed warnings, added redirection for home page and script for setting up the project - (6b21fb9) - Saurav Suman

- - -

## v0.16.3 - 2023-12-27
### Package updates
- context-aware-config bumped to context-aware-config-v0.13.2
### Global changes
#### Bug Fixes
- PICAF-24552 - Cac client library changes to consume backend api response - (5998ccd) - Prasanna P

- - -

## v0.16.2 - 2023-11-30
### Package updates
- experimentation-platform bumped to experimentation-platform-v0.8.2
### Global changes
#### Bug Fixes
- allow ramp 0 - (b8d49aa) - Kartik Gajendra

- - -

## v0.16.1 - 2023-11-22
### Package updates
- context-aware-config bumped to context-aware-config-v0.13.1
### Global changes
#### Bug Fixes
- PICAF-25066 sort json while context creation - (3bd7a97) - Pratik Mishra

- - -

## v0.16.0 - 2023-11-16
### Package updates
- context-aware-config bumped to context-aware-config-v0.13.0
### Global changes
#### Bug Fixes
- fixed deployment ConfigNotFound failure - (be381f1) - Shubhranshu Sanjeev
#### Features
- update default keys - (d6b9992) - ankit.mahato

- - -

## v0.15.1 - 2023-11-16
### Package updates
- experimentation-platform bumped to experimentation-platform-v0.8.1
### Global changes
#### Bug Fixes
- add different auth types for exp requests to CAC - (bd8ae88) - Kartik Gajendra

- - -

## v0.15.0 - 2023-11-11
### Package updates
- context-aware-config bumped to context-aware-config-v0.12.0
- external bumped to external-v0.3.0
- cac_client bumped to cac_client-v0.4.0
- service-utils bumped to service-utils-v0.9.0
- experimentation_client bumped to experimentation_client-v0.4.0
- experimentation-platform bumped to experimentation-platform-v0.8.0
### Global changes
#### Bug Fixes
- failing build due to update of schema.rs file - (131463b) - Shubhranshu Sanjeev
#### Features
- added format check in the JenkinsFile(PICAF-24813) - (4fdf864) - Saurav Suman
- added frontend crate,combined frontend and backend binaries (PICAF-24540) - (ee084ba) - Saurav Suman
#### Miscellaneous Chores
- [PICAF-24778] move dependencies to workspaces - (38a524f) - Kartik Gajendra

- - -

## v0.14.1 - 2023-11-09
### Package updates
- experimentation-platform bumped to experimentation-platform-v0.7.1
### Global changes

- - -

## v0.14.0 - 2023-11-08
### Package updates
- external bumped to external-v0.2.0
- experimentation-platform bumped to experimentation-platform-v0.7.0
- service-utils bumped to service-utils-v0.8.0
- context-aware-config bumped to context-aware-config-v0.11.0
### Global changes
#### Features
- [PICAF-24779] integrate authorize middleware - (4a582f3) - Kartik Gajendra

- - -

## v0.13.0 - 2023-11-06
### Package updates
- external bumped to external-v0.1.0
### Global changes
#### Bug Fixes
- PICAF-25068 added external crate to cocogitto config - (f6f60ef) - Ritick Madaan
- PICAF-25068 x-tenant header added for /config/resolve call in diff - (0e34c31) - Ritick Madaan

- - -

## v0.12.1 - 2023-10-31
### Package updates
- context-aware-config bumped to context-aware-config-v0.10.2
### Global changes

- - -

## v0.12.0 - 2023-10-27
### Package updates
- context-aware-config bumped to context-aware-config-v0.10.1
- cac_client bumped to cac_client-v0.3.0
- service-utils bumped to service-utils-v0.7.1
- experimentation_client bumped to experimentation_client-v0.3.0
### Global changes
#### Bug Fixes
- fixed failing health check (x-tenant header not set) - (23af679) - Shubhranshu Sanjeev
#### Features
- multi-tenant support for client libraries - (c603be0) - Shubhranshu Sanjeev

- - -

## v0.11.0 - 2023-10-25
### Package updates
- experimentation-platform bumped to experimentation-platform-v0.6.0
- service-utils bumped to service-utils-v0.7.0
- context-aware-config bumped to context-aware-config-v0.10.0
### Global changes
#### Features
- added multi-tenant support - (5d34e78) - Shubhranshu Sanjeev
- added middleware and FromRequest for tenant and app scope info - (07a64ad) - Shubhranshu Sanjeev
#### Refactoring
- moved tables and types out of cac_v1 schema - (f70a0c5) - Shubhranshu Sanjeev

- - -

## v0.10.0 - 2023-10-20
### Package updates
- context-aware-config bumped to context-aware-config-v0.9.0
- service-utils bumped to service-utils-v0.6.0
- experimentation_client bumped to experimentation_client-v0.2.0
### Global changes

- - -

## v0.9.1 - 2023-10-13
### Package updates
- experimentation_client bumped to experimentation_client-v0.1.3
### Global changes

- - -

## v0.9.0 - 2023-10-10
### Package updates
- context-aware-config bumped to context-aware-config-v0.8.0
- experimentation-platform bumped to experimentation-platform-v0.5.0
### Global changes
#### Refactoring
- resolved comments - (aefb03e) - Shubhranshu Sanjeev
#### Tests
- added postman test for update override_keys api - (cc96ca1) - Shubhranshu Sanjeev
- added tests for experiment helper fnxs - (ea4db17) - Shubhranshu Sanjeev

- - -

## v0.8.1 - 2023-10-10
### Package updates
- context-aware-config bumped to context-aware-config-v0.7.1
### Global changes

- - -

## v0.8.0 - 2023-10-09
### Package updates
- context-aware-config bumped to context-aware-config-v0.7.0
- service-utils bumped to service-utils-v0.5.0
### Global changes
#### Features
- server's keep-alive time and db connection pool max size made configurable - (110ee00) - Ritick Madaan

- - -

## v0.7.1 - 2023-10-05
### Package updates
- service-utils bumped to service-utils-v0.4.1
- context-aware-config bumped to context-aware-config-v0.6.1
### Global changes
#### Bug Fixes
- [PICAF-24563] add user struct in delete context API - (9a0360d) - Kartik Gajendra
- ssh.bitbucket.juspay.net added to known hosts in docker bulid - (9207701) - Ritick Madaan

- - -

## v0.7.0 - 2023-10-05
### Package updates
- experimentation-platform bumped to experimentation-platform-v0.4.0
- context-aware-config bumped to context-aware-config-v0.6.0
### Global changes
#### Continuous Integration
- **(flake.nix)** pin nodejs version to 18 in flake - (614261e) - Natarajan Kannan
#### Features
- [PICAF-24563] added dashboard auth middleware - (955d9e9) - Kartik Gajendra
- PICAF-24664 cors middleware attached - (8cb4805) - Ritick Madaan

- - -

## v0.6.1 - 2023-09-20
### Package updates
- cac_client bumped to cac_client-v0.2.1
- context-aware-config bumped to context-aware-config-v0.5.1
### Global changes

- - -

## v0.6.0 - 2023-09-12
### Package updates
- cac_client bumped to cac_client-v0.2.0
- service-utils bumped to service-utils-v0.4.0
- context-aware-config bumped to context-aware-config-v0.5.0
- experimentation-platform bumped to experimentation-platform-v0.3.1
### Global changes
#### Bug Fixes
- fixed random timeouts in internal http calls to CAC - (a4e95a3) - Shubhranshu Sanjeev
#### Continuous Integration
- added 20 minutes timeout on pipeline - (2f6bf9e) - Shubhranshu Sanjeev
#### Features
- PICAF-24223 Adding generic eval - (b94ce46) - Pratik Mishra
- Schema addition for Dimension values - (7960a67) - Prasanna P

- - -

## v0.5.1 - 2023-09-06
### Package updates
- experimentation_client bumped to experimentation_client-v0.1.2
### Global changes
#### Bug Fixes
- trimming newline character from version string - (2c61077) - Shubhranshu Sanjeev
#### Continuous Integration
- deleting postgres's docker image on every test - (8a198d6) - Ritick Madaan

- - -

## v0.5.0 - 2023-09-06
### Package updates
- experimentation-platform bumped to experimentation-platform-v0.3.0
### Global changes
#### Features
- [PICAF-24160] record the chosen variant after conclude - (1c3c6e6) - Kartik Gajendra

- - -

## v0.4.1 - 2023-09-06
### Package updates
- experimentation_client bumped to experimentation_client-v0.1.1
### Global changes
#### Bug Fixes
- fixed setting env in docker image - (272454b) - Shubhranshu Sanjeev
#### Continuous Integration
- PICAF-24114 updated integ AP tracker curl with new version - (1e0fa5b) - Ritick Madaan

- - -

## v0.4.0 - 2023-09-06
### Package updates
- context-aware-config bumped to context-aware-config-v0.4.0
- service-utils bumped to service-utils-v0.3.0
### Global changes
#### Features
- [PICAF-24065] added pod information in response headers and logs - (5ee8a9c) - Kartik Gajendra

- - -

## v0.3.0 - 2023-09-05
### Package updates
- experimentation-platform bumped to experimentation-platform-v0.2.0
- context-aware-config bumped to context-aware-config-v0.3.0
- service-utils bumped to service-utils-v0.2.0
### Global changes
#### Features
- [PICAF-24073] add audit log search endpoint - (19f75c7) - Kartik Gajendra
#### Revert
- Revert "fix: PICAF-24114 logged env variable's value before kms decrypting" - (2a935c9) - Ritick Madaan

- - -

## v0.2.0 - 2023-09-05
### Package updates
- context-aware-config bumped to context-aware-config-v0.2.0
### Global changes
#### Bug Fixes
- PICAF-24273 cleaned up Dockerfile - (cb5c4fb) - Ritick Madaan
- moved git init to separate stage - (9477de6) - Shubhranshu Sanjeev
#### Features
- PICAF-23598 implemented tracing-actix-web for logging - (63dee8c) - Ritick Madaan

- - -

## v0.1.0 - 2023-09-01
### Package updates
- experimentation-platform bumped to experimentation-platform-v0.1.0
- service-utils bumped to service-utils-v0.1.0
- cac_client bumped to cac_client-v0.1.0
- context-aware-config bumped to context-aware-config-v0.1.0
- experimentation_client bumped to experimentation_client-v0.1.0
### Global changes
#### Bug Fixes
- PICAF-24114 logged env variable's value before kms decrypting - (5bda6fb) - Ritick Madaan
- added middleware to insert version in response headers - (449eea4) - Shubhranshu Sanjeev
- calling cac apis for creating context - (a7d92f5) - Shubhranshu Sanjeev
- moved tables and types under cac_v1 schema - (1be82f1) - Shubhranshu Sanjeev
- added last_modified column and indexes - (942d723) - Shubhranshu Sanjeev
- PICAF-23634 fixed ordering of /context endpoints - (e7b71b3) - Ritick Madaan
- PICAF-22642 moved dimension_type to cac_v1 schema - (5ae90a7) - Ritick Madaan
- PICAF-23520 enabled override updates in PUT /context by deep_merge - (53b148a) - Ritick Madaan
- PICAF-23552 added moved tables to cac_v1 schema - (f9fb191) - Ritick Madaan
- PICAF-23552 database schema url - (e32c1c7) - Ritick Madaan
- PICAF-23552 added search path for schema in database_url - (fd36bdc) - Ritick Madaan
- PICAF-23199 removed unecessary and wrap over conditions - (602643d) - Ritick Madaan
- PICAF-23231 moved override inside contexts table - (e09dd45) - Ritick Madaan
- PICAF-22967: DB password URI encoded. - (c01d921) - Shrey Bana
- removed delete dimension api - (b3b126b) - Ritick Madaan
- storing pre calculated priority in contexts table - (7de363c) - Ritick Madaan
- PICAF-22646 dimension Table scaffolding, PUT<>DELETE apis `/dimension` - (6f4b52e) - Ritick Madaan
- changed host to 0.0.0.0 - (f487a5b) - Ritick Madaan
- base64 decoding kms cypher - (952b546) - Ritick Madaan
- improved log for env not found - (7240266) - Ritick Madaan
- corrected env for DB_PASSWORD and default for AWS_REGION - (a056d85) - Ritick Madaan
#### Build system
- added version tag to docker images - (501bdf4) - Shubhranshu Sanjeev
- PICAF-22783 installing ca-certificates for ssl verification - (626e8e5) - Ritick Madaan
#### Continuous Integration
- added cocogitto config for automatic versioning - (1266bf9) - Shubhranshu Sanjeev
- added postman collection for experimentation-platform - (280441a) - Shubhranshu Sanjeev
- fix newman dev dependency ref - (11e4cbd) - Natarajan Kannan
- PICAF-23646 udpated `docker container ls` filter - (ce93bc0) - Ritick Madaan
- PICAF-23646 enabling tests in pr builds - (d09f566) - Ritick Madaan
- PICAF-23646: switch to using newman - (846b931) - Natarajan Kannan
- automated newman test setup - (c2667c0) - Natarajan Kannan
- PICAF-22647: Upgraded nixos to 23.05. - (267dfdc) - Shrey Bana
- moved nixpkgs to nixos-22.11 as the unstable one had broken rustfmt - (0083af7) - Ritick Madaan
- PICAF-22654: Enabling production docker image push. - (76311d7) - Shrey Bana
- PICAF-22654: Commented out prod docker push. - (60b8142) - Shrey Bana
- PICAF-22654: Created pipeline for automated-deployment - (b3208cc) - Shrey Bana
- made some miscellaneous changes for local setup - (89cf0ec) - Ritick Madaan
#### Features
- [PICAF-23868] Added Catch all error type for robust error handling - (60f6f2a) - Kartik Gajendra
- [PICAF-24010] added support for CUG in super position client - (4eeae99) - Kartik Gajendra
- [PICAF-23632] added experimentation client with few fixes - (9a31815) - Kartik Gajendra
- [PICAF-23502] added list experiments API - (01b52cc) - Kartik Gajendra
- added 304 <> last-modified for GET /config - (7592a21) - Saurav Suman
- PICAF-23520 PUT /context/move/{ctx_id} api - (4875284) - Ritick Madaan
- PICAF-22511 added DELETE /context/{ctx_id} api - (d557f84) - Ritick Madaan
- PICAF-22678: Added authentication. - (cd20874) - Shrey Bana
- PICAF-23199 removed properties constraint on objects in schema - (dd83bd5) - Ritick Madaan
- PICAF-23057 rust library - (05f80ec) - Ritick Madaan
- PICAF-22932: Added support for validation via JSON schema. - (51c81cb) - Shrey Bana
- PICAF-22664: Added context list API - (dc21416) - Shrey Bana
- GET /config api - (37ab8e9) - Ritick Madaan
- /default-config/<key> PUT api - (ae12c7b) - Ritick Madaan
- PICAF-22783 added localstack setup along with kms - (a8d3a15) - Ritick Madaan
- PICAF-22654: Added health-check endpoint. - (993a6d2) - Shrey Bana
- PICAF-22510 Added context fetch API - (7609a68) - Shrey Bana
- context/add api along with db setup - (f5206cb) - Ritick Madaan
#### Refactoring
- improvements to APIs - (60bf5c0) - Shubhranshu Sanjeev
- moved cac to cargo workspaces - (1855ef8) - Shubhranshu Sanjeev
- moved AppState & utility fx to new crate - (4f734a5) - Shubhranshu Sanjeev
- PICAF-22469 removed old contexts table - (49770dc) - Ritick Madaan
- PICAF-22468 moved db related modules to db crate - (f7d9492) - Ritick Madaan
#### Tests
- update to latest newman that handles top level events and body lang type - (fcd7724) - Natarajan Kannan
- fix newman version used in tests - (62ede3d) - Natarajan Kannan

- - -

Changelog generated by [cocogitto](https://github.com/cocogitto/cocogitto).