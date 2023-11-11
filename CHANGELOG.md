# Changelog
All notable changes to this project will be documented in this file. See [conventional commits](https://www.conventionalcommits.org/) for commit guidelines.

- - -
## v0.15.0 - 2023-11-11
### Package updates
- context-aware-config bumped to context-aware-config-v0.12.0
- external bumped to external-v0.3.0
- cac_client bumped to cac_client-v0.4.0
- service-utils bumped to service-utils-v0.9.0
- superposition_client bumped to superposition_client-v0.4.0
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
- superposition_client bumped to superposition_client-v0.3.0
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
- superposition_client bumped to superposition_client-v0.2.0
### Global changes

- - -

## v0.9.1 - 2023-10-13
### Package updates
- superposition_client bumped to superposition_client-v0.1.3
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
- superposition_client bumped to superposition_client-v0.1.2
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
- superposition_client bumped to superposition_client-v0.1.1
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
- superposition_client bumped to superposition_client-v0.1.0
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