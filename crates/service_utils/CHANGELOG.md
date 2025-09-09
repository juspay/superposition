# Changelog
All notable changes to this project will be documented in this file. See [conventional commits](https://www.conventionalcommits.org/) for commit guidelines.

- - -
## service_utils-v0.33.1 - 2025-09-09
#### Bug Fixes
- use log_level to decide req/resp logging - (24e2545) - Natarajan Kannan
#### Miscellaneous Chores
- **(version)** v0.88.0 [skip ci] - (e23bb63) - Superposition Bot
- address review comments - (ae5f24a) - Natarajan Kannan
- simplify Default - (9a39bd8) - Natarajan Kannan
- remove re-orders and keep rest of code intact - (e2e44b2) - Natarajan Kannan
- make res type explicit - (d9161ac) - Natarajan Kannan
- address review comments and PR check issues - (4b7af6e) - Natarajan Kannan
- add response body logging - (434b451) - Natarajan Kannan
- add request body logging - (4eff98b) - Natarajan Kannan
- add request/response middleware - (502fbfe) - Natarajan Kannan

- - -

## service_utils-v0.33.0 - 2025-08-19
#### Features
- Move jsonlogic behind compile time flag - (328671c) - ayush.jain@juspay.in
#### Miscellaneous Chores
- **(version)** v0.83.0 [skip ci] - (2e2d48c) - Superposition Bot

- - -

## service_utils-v0.32.0 - 2025-08-04
#### Features
- publish rust libraries through crates.io - (8b62d45) - datron

- - -

## service_utils-v0.29.0 - 2025-07-03
#### Features
- Rust Version Bump + Uniffi + Java OpenFeature SDK (#553) - (1aba4b4) - ShreyBana
#### Miscellaneous Chores
- Rename auth to auth_n (#542) - (fbf4e1a) - Ayush Jain
- Remove dead code (#536) - (621b516) - Ayush Jain

- - -

## service_utils-v0.28.1 - 2025-05-12
#### Bug Fixes
- custom headers parsing for webhook (#506) - (04133a2) - Ankit Kumar Mahato

- - -

## service_utils-v0.28.0 - 2025-05-09
#### Bug Fixes
- webhook implementation (#503) - (1ffd6eb) - Ankit Kumar Mahato
#### Features
- webhook cruds (#313) - (292689e) - Ankit Kumar Mahato

- - -

## service_utils-v0.27.0 - 2025-04-17
#### Features
- smithy models - (2958cce) - PRATIK MISHRA

- - -

## service_utils-v0.26.3 - 2025-02-28
#### Bug Fixes
- Fixed clippy warnings - (88bbfe9) - ShreyBana

- - -

## service_utils-v0.26.2 - 2025-02-19
#### Bug Fixes
- tenant config - (416f066) - Ankit Kumar Mahato

- - -

## service_utils-v0.26.1 - 2025-02-14
#### Bug Fixes
- disable prepared statement cache so that schema swapping does not break - (c0dc141) - Kartik

- - -

## service_utils-v0.26.0 - 2025-01-23
#### Bug Fixes
- experiment handlers to send org_id - (571f201) - Kartik
- read from workspaces table when showing the dropdown - (1594218) - Kartik
#### Features
- auth path prefix support and org_user authentication - (919de10) - ayush.jain@juspay.in
- workspace-ui - (4ccd9c6) - Ankit.Mahato
- added schema and crl apis for organisation (#322) - (c333f3a) - sauraww
- Add auth via OAUTH2 (#321) - (f5092f8) - Ayush Jain
- Use Common db model types in frontend (#291) - (e68782d) - Ayush Jain
#### Refactoring
- OrgId, WorkspaceId, SchemaName cleanup and refactor (#379) - (470ab48) - Ayush Jain
- merge cac and experimentation schemas - (51367a6) - Kartik

- - -

## service_utils-v0.25.0 - 2025-01-06
#### Bug Fixes
- webhook kms decrypt auth key (#332) - (09ebc29) - Ankit Kumar Mahato
#### Features
- search and sort experiments - (95b87c5) - Datron

- - -

## service_utils-v0.24.2 - 2024-12-04
#### Bug Fixes
- webhook optional field (#296) - (fe0171f) - Ankit Kumar Mahato

- - -

## service_utils-v0.24.1 - 2024-11-22
#### Bug Fixes
- fixed and improved webhooks (#283) - (4864849) - Ankit Kumar Mahato

- - -

## service_utils-v0.24.0 - 2024-11-20
#### Features
- Webhook trigger for experiments (#265) - (585ee1e) - Ankit Kumar Mahato

- - -

## service_utils-v0.23.0 - 2024-10-23
#### Features
- add high performance mode with redis - (adc2712) - Kartik

- - -

## service_utils-v0.22.0 - 2024-09-30
#### Bug Fixes
- Dependency pruning (#250) - (8b68900) - Ayush Jain
#### Features
- Tenant specific config support via .cac.toml (#246) - (ffc247e) - Ayush Jain

- - -

## service_utils-v0.21.0 - 2024-08-14
#### Bug Fixes
- changing aws kms library to aws-sdk-kms to rusto (#203) - (a455f93) - namit goel
#### Features
- Mandatory dimensions feature (#173) - (8d95a30) - Ankit Kumar Mahato

- - -

## service_utils-v0.20.0 - 2024-08-08
#### Bug Fixes
- jsonschema for dimension and remove default_config's jsonsschema check (#197) - (89a23af) - PRATIK MISHRA
- api validation with new types (#146) - (66ad741) - PRATIK MISHRA
#### Features
- monaco as a component (#184) - (5233f1a) - Datron

- - -

## service_utils-v0.19.0 - 2024-07-11
#### Features
- move apperror to superposition_types - (f1c8395) - Pratik Mishra

- - -

## service_utils-v0.18.0 - 2024-06-20
#### Features
- add config version header in api response (#87) - (213a21e) - PRATIK MISHRA

- - -

## service_utils-v0.17.0 - 2024-06-11
#### Features
- snapshot changes - apis - (69588f6) - Pratik Mishra
#### Miscellaneous Chores
- address some clippy lints (#85) - (001b8d4) - Sanchith Hegde
- simplify module structure and follow Rust 2018 edition module naming convention (#86) - (403ea59) - Sanchith Hegde

- - -

## service_utils-v0.16.1 - 2024-05-29
#### Bug Fixes
- creating experiments for default-config (no context) (#38) - (4f6b92c) - Sauravcv98
- reject experiment contexts with `variantIds` (#29) - (092e568) - Shubhranshu Sanjeev
- setup superposition without nix - (ea51bd4) - Kartik

- - -

## service_utils-v0.16.0 - 2024-05-06
#### Features
- improved error communication on frontend, with toast component - (51838eb) - Shubhranshu Sanjeev
- ready for open source! - (f48db35) - Kartik
#### Miscellaneous Chores
- **(version)** v0.42.0 [skip ci] - (e4bc080) - Superposition Bot
- **(version)** v0.41.0 [skip ci] - (ceefd83) - Superposition Bot
- **(version)** v0.40.0 [skip ci] - (836b769) - Jenkins
- add PR testing workflows (#6) - (d6bb8d4) - Datron

- - -

## service_utils-v0.16.0 - 2024-05-06
#### Features
- improved error communication on frontend, with toast component - (51838eb) - Shubhranshu Sanjeev
- ready for open source! - (f48db35) - Kartik
#### Miscellaneous Chores
- **(version)** v0.41.0 [skip ci] - (ceefd83) - Superposition Bot
- **(version)** v0.40.0 [skip ci] - (836b769) - Jenkins
- add PR testing workflows (#6) - (d6bb8d4) - Datron

- - -

## service_utils-v0.15.0 - 2024-04-24
#### Features
- ready for open source! - (5f7af15) - Kartik
#### Miscellaneous Chores
- **(version)** v0.40.0 [skip ci] - (cdfe3f4) - Jenkins
- add PR testing workflows (#6) - (6085588) - Datron

- - -

## service_utils-v0.14.0 - 2024-04-18
#### Features
- ready for open source! - (b7d36be) - Kartik

- - -

## service_utils-v0.13.0 - 2024-04-10
#### Features
- [PICAF-25423] added new result, error type and error macros - (e673fb1) - Shubhranshu Sanjeev
#### Refactoring
- [PICAF-26558] refactored service to use new error type and better error handling - (741f391) - Shubhranshu Sanjeev

- - -

## service_utils-v0.12.0 - 2024-03-08
#### Features
- PICAF-25884 Added function validation for context and default_config - (990b729) - ankit.mahato

- - -

## service_utils-v0.11.0 - 2024-02-20
#### Features
- support for service prefix - (a2915b4) - Shubhranshu Sanjeev

- - -

## service_utils-v0.10.3 - 2024-02-15
#### Bug Fixes
- fixing error message for experiment create and bulk context api - (bc0d7be) - Jenkins

- - -

## service_utils-v0.10.2 - 2024-01-22
#### Bug Fixes
- fixed host resolve issue for internal calls in SSR. - (3cc9d6e) - Shubhranshu Sanjeev

- - -

## service_utils-v0.10.1 - 2024-01-18
#### Bug Fixes
- error resolving pages with internal call to server - (084d08b) - Shubhranshu Sanjeev

- - -

## service_utils-v0.10.0 - 2024-01-04
#### Bug Fixes
- frontend multi-tenancy support + config and dimension page - (a1689a1) - Shubhranshu Sanjeev
#### Features
- experiment UI - (24e1b56) - Kartik Gajendra

- - -

## service_utils-v0.9.0 - 2023-11-11
#### Features
- added format check in the JenkinsFile(PICAF-24813) - (4fdf864) - Saurav Suman
#### Miscellaneous Chores
- [PICAF-24778] move dependencies to workspaces - (38a524f) - Kartik Gajendra

- - -

## service_utils-v0.8.0 - 2023-11-08
#### Bug Fixes
- make sure envs with defaults prevent failure - (aac0303) - Kartik Gajendra
#### Features
- [PICAF-24779] integrate authorize middleware - (4a582f3) - Kartik Gajendra

- - -

## service_utils-v0.7.1 - 2023-10-27
#### Bug Fixes
- fixed failing health check (x-tenant header not set) - (23af679) - Shubhranshu Sanjeev

- - -

## service_utils-v0.7.0 - 2023-10-25
#### Features
- added multi-tenant support - (5d34e78) - Shubhranshu Sanjeev
- added middleware and FromRequest for tenant and app scope info - (07a64ad) - Shubhranshu Sanjeev

- - -

## service_utils-v0.6.0 - 2023-10-20
#### Features
- PICAF-23643 - Dimension value schema validation on context-addition - (b2fad9e) - Prasanna P

- - -

## service_utils-v0.5.0 - 2023-10-09
#### Features
- server's keep-alive time and db connection pool max size made configurable - (110ee00) - Ritick Madaan

- - -

## service_utils-v0.4.1 - 2023-10-05
#### Bug Fixes
- [PICAF-24563] add user struct in delete context API - (9a0360d) - Kartik Gajendra

- - -

## service_utils-v0.4.0 - 2023-09-12
#### Features
- Schema addition for Dimension values - (7960a67) - Prasanna P

- - -

## service_utils-v0.3.0 - 2023-09-06
#### Features
- [PICAF-24065] added pod information in response headers and logs - (5ee8a9c) - Kartik Gajendra

- - -

## service_utils-v0.2.0 - 2023-09-05
#### Features
- [PICAF-24073] add audit log search endpoint - (19f75c7) - Kartik Gajendra
#### Revert
- Revert "fix: PICAF-24114 logged env variable's value before kms decrypting" - (2a935c9) - Ritick Madaan

- - -

## service_utils-v0.1.0 - 2023-09-01
#### Bug Fixes
- PICAF-24114 logged env variable's value before kms decrypting - (5bda6fb) - Ritick Madaan
- added middleware to insert version in response headers - (449eea4) - Shubhranshu Sanjeev
- calling cac apis for creating context - (a7d92f5) - Shubhranshu Sanjeev
#### Continuous Integration
- PICAF-23646 enabling tests in pr builds - (d09f566) - Ritick Madaan
#### Features
- [PICAF-23868] Added Catch all error type for robust error handling - (60f6f2a) - Kartik Gajendra
- [PICAF-23502] added list experiments API - (01b52cc) - Kartik Gajendra
#### Refactoring
- moved fetching db connection in FromRequest trait impl - (c07c1d2) - Shubhranshu Sanjeev
- moved cac to cargo workspaces - (1855ef8) - Shubhranshu Sanjeev
- moved AppState & utility fx to new crate - (4f734a5) - Shubhranshu Sanjeev

- - -

Changelog generated by [cocogitto](https://github.com/cocogitto/cocogitto).