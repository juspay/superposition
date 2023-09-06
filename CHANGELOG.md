# Changelog
All notable changes to this project will be documented in this file. See [conventional commits](https://www.conventionalcommits.org/) for commit guidelines.

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