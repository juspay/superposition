# Changelog
All notable changes to this project will be documented in this file. See [conventional commits](https://www.conventionalcommits.org/) for commit guidelines.

- - -
## context_aware_config-v0.37.0 - 2024-08-14
#### Bug Fixes
- changing aws kms library to aws-sdk-kms to rusto (#203) - (a455f93) - namit goel
- newtypes for entity fields (#199) - (49562b7) - PRATIK MISHRA
#### Features
- Mandatory dimensions feature (#173) - (8d95a30) - Ankit Kumar Mahato

- - -

## context_aware_config-v0.36.1 - 2024-08-08
#### Bug Fixes
- jsonschema for dimension and remove default_config's jsonsschema check (#197) - (89a23af) - PRATIK MISHRA
- add last modified to all tables (#192) - (d41195c) - PRATIK MISHRA
- api validation with new types (#146) - (66ad741) - PRATIK MISHRA
- Snapshot version parsing (#193) - (f3f4535) - Ayush Jain
- resolve page issue (#180) - (73b5dbc) - Ankit Kumar Mahato
- remove process, filter env variables (#174) - (710c69d) - PRATIK MISHRA

- - -

## context_aware_config-v0.36.0 - 2024-07-17
#### Bug Fixes
- update axios version in cac crate (#162) - (ff1cde6) - PRATIK MISHRA
- rename experiment test folders in postman to work on linux machines - (6526fa2) - Kartik
#### Features
- Add fetch context from context condition (#168) - (6dd17f8) - Ayush Jain

- - -

## context_aware_config-v0.35.0 - 2024-07-11
#### Bug Fixes
- undo stringify of last_modified timestamp (#154) - (8334925) - PRATIK MISHRA
- x-config-version in get config and experiments response (#152) - (1a429a9) - Ayush Jain
#### Features
- move apperror to superposition_types - (f1c8395) - Pratik Mishra

- - -

## context_aware_config-v0.34.3 - 2024-07-01
#### Bug Fixes
- default config regex validation (#141) - (29d3f1a) - Ankit Kumar Mahato

- - -

## context_aware_config-v0.34.2 - 2024-06-27
#### Bug Fixes
- allow positive i32 number for dimension priority in dimension create (#135) - (5e79ad8) - Ayush Jain

- - -

## context_aware_config-v0.34.1 - 2024-06-20
#### Bug Fixes
- Use jsonlogc's partial_apply for config and experiment filtering (#127) - (97bf39b) - Ayush Jain

- - -

## context_aware_config-v0.34.0 - 2024-06-20
#### Features
- add config version header in api response (#87) - (213a21e) - PRATIK MISHRA

- - -

## context_aware_config-v0.33.0 - 2024-06-11
#### Bug Fixes
- unify actions column for default_config and custom_types - (bdcc902) - Kartik
- add patch_file in diesel.toml - (5ec9835) - Pratik Mishra
#### Features
- integrate type templates in forms - (3954d21) - Kartik
- add type templates UI - (c8f2e3a) - Kartik
- add custom types API endpoints - (c63192d) - Kartik
- snapshot changes - apis - (69588f6) - Pratik Mishra
#### Miscellaneous Chores
- rename upper case enum variants to use `PascalCase` names (#94) - (6d3f4ca) - Sanchith Hegde
- address some clippy lints (#85) - (001b8d4) - Sanchith Hegde
- simplify module structure and follow Rust 2018 edition module naming convention (#86) - (403ea59) - Sanchith Hegde

- - -

## context_aware_config-v0.32.0 - 2024-05-31
#### Bug Fixes
- stricter validation for key names in default config (#79) - (78a80c0) - Natarajan Kannan
#### Features
- Add local storage support (#78) - (9eee7f0) - Ankit Kumar Mahato

- - -

## context_aware_config-v0.31.0 - 2024-05-29
#### Bug Fixes
- disallow trailing . in key name (#77) - (57807d6) - Natarajan Kannan
- reject experiment contexts with `variantIds` (#29) - (092e568) - Shubhranshu Sanjeev
#### Features
- reducing context tool (#44) - (104f06d) - Sauravcv98
- added support for update , create and clone of override - (ebf38bd) - Saurav Suman
- add api for recompute priority (#17) - (b26d0f6) - PRATIK MISHRA
#### Miscellaneous Chores
- Add CI check to lint the .sql files based on rules defined in .editorconfig - (16bf460) - Hao

- - -

## context_aware_config-v0.30.0 - 2024-05-10
#### Bug Fixes
- for the first page redirect to default config page - (361054b) - ankit.mahato
#### Features
- delete default config api (#34) - (d5c001c) - PRATIK MISHRA

- - -

## context_aware_config-v0.29.0 - 2024-05-06
#### Bug Fixes
- fixed function template (#16) - (bb2df67) - Ankit Kumar Mahato
#### Features
- improved error communication on frontend, with toast component - (51838eb) - Shubhranshu Sanjeev
- ready for open source! - (f48db35) - Kartik
#### Miscellaneous Chores
- **(version)** v0.42.0 [skip ci] - (e4bc080) - Superposition Bot
- **(version)** v0.41.0 [skip ci] - (ceefd83) - Superposition Bot
- **(version)** v0.40.0 [skip ci] - (836b769) - Jenkins
- open source superposition - (b85a0a8) - Kartik

- - -

## context_aware_config-v0.29.0 - 2024-05-06
#### Bug Fixes
- fixed function template (#16) - (bb2df67) - Ankit Kumar Mahato
#### Features
- improved error communication on frontend, with toast component - (51838eb) - Shubhranshu Sanjeev
- ready for open source! - (f48db35) - Kartik
#### Miscellaneous Chores
- **(version)** v0.41.0 [skip ci] - (ceefd83) - Superposition Bot
- **(version)** v0.40.0 [skip ci] - (836b769) - Jenkins
- open source superposition - (b85a0a8) - Kartik

- - -

## context_aware_config-v0.28.0 - 2024-04-24
#### Features
- ready for open source! - (5f7af15) - Kartik
#### Miscellaneous Chores
- **(version)** v0.40.0 [skip ci] - (cdfe3f4) - Jenkins
- open source superposition - (cbd5b6f) - Kartik

- - -

## context_aware_config-v0.27.0 - 2024-04-18
#### Features
- ready for open source! - (b7d36be) - Kartik

- - -

## context-aware-config-v0.26.1 - 2024-04-17
#### Bug Fixes
- [PICAF-26653] removed audit log middleware and reduced max db connection pool size to 2 - (82022eb) - Saurav Suman

- - -

## context-aware-config-v0.26.0 - 2024-04-16
#### Features
- PICAF-26366 Add filter support to client - (f4c12c7) - ankit.mahato

- - -

## context-aware-config-v0.25.2 - 2024-04-10
#### Bug Fixes
- PICAF-26366 added service-prefix to functions endpoints - (5492072) - ankit.mahato
#### Refactoring
- [PICAF-26558] refactored service to use new error type and better error handling - (741f391) - Shubhranshu Sanjeev

- - -

## context-aware-config-v0.25.1 - 2024-04-08
#### Bug Fixes
- [PICAF-26346] add path to node_modules - (c4bc7b6) - Pratik Mishra

- - -

## context-aware-config-v0.25.0 - 2024-04-05
#### Features
- PICAF-26168-js-secure-sandbox - (566f8be) - Pratik Mishra

- - -

## context-aware-config-v0.24.2 - 2024-03-27
#### Bug Fixes
- PICAF-26454 JS validator functions to take config value and key - (656fe39) - ankit.mahato

- - -

## context-aware-config-v0.24.1 - 2024-03-21
#### Bug Fixes
- PICAF-26307 filter config fix - (aa114fb) - ankit.mahato

- - -

## context-aware-config-v0.24.0 - 2024-03-21
#### Features
- PICAF-26307 Filter Config by prefix - (c0a0bfe) - ankit.mahato

- - -

## context-aware-config-v0.23.2 - 2024-03-20
#### Bug Fixes
- PICAF-25884 Functions bug fixes - (8e7452b) - ankit.mahato

- - -

## context-aware-config-v0.23.1 - 2024-03-19
#### Bug Fixes
- [PICAF-26348] added routes without service prefix for b/w compatibility - (079c02d) - Shubhranshu Sanjeev
#### Documentation
- PICAF-25981: add intro doc and features - (d09ba53) - Natarajan Kannan

- - -

## context-aware-config-v0.23.0 - 2024-03-08
#### Features
- PICAF-25884 Added function validation for context and default_config - (990b729) - ankit.mahato

- - -

## context-aware-config-v0.22.0 - 2024-03-06
#### Features
- support more operations - (4db2c31) - Kartik Gajendra

- - -

## context-aware-config-v0.21.0 - 2024-03-04
#### Features
- PICAF-26185 Replace merge-strategy option for resolve/eval - (453cfb9) - ayush.jain@juspay.in

- - -

## context-aware-config-v0.20.0 - 2024-03-04
#### Features
- [PICAF-25877 add node to app directory - (9671875) - Pratik Mishra

- - -

## context-aware-config-v0.19.0 - 2024-02-29
#### Features
- [PICAF-25879] added test,publish api for functions - (050ab24) - Pratik Mishra

- - -

## context-aware-config-v0.18.2 - 2024-02-28
#### Bug Fixes
- [PICAF-26199] transpose columns in single experiment page for variants - (a1a8ac8) - Kartik

- - -

## context-aware-config-v0.18.1 - 2024-02-26
#### Bug Fixes
- [PICAF-26195] fix copy of experiment ID - (37e4c24) - Kartik

- - -

## context-aware-config-v0.18.0 - 2024-02-22
#### Bug Fixes
- PICAF-26157 Do not remove keys with null value on merge - (bd3c196) - ayush.jain@juspay.in
#### Features
- PICAF-25876 CRUD APIs for function validator - (7c0c963) - ankit.mahato

- - -

## context-aware-config-v0.17.0 - 2024-02-22
#### Features
- [PICAF-25877] js eval with node exec - (adc9b19) - Pratik Mishra

- - -

## context-aware-config-v0.16.0 - 2024-02-20
#### Features
- support for service prefix - (a2915b4) - Shubhranshu Sanjeev

- - -

## context-aware-config-v0.15.2 - 2024-02-19
#### Bug Fixes
- [PICAF-26004] better logging - (b3d1bc8) - Kartik

- - -

## context-aware-config-v0.15.1 - 2024-02-15
#### Bug Fixes
- fixing error message for experiment create and bulk context api - (bc0d7be) - Jenkins

- - -

## context-aware-config-v0.15.0 - 2024-01-31
#### Features
- [PICAF-25817] added authentication header for frontend apis - (3f90592) - Saurav Suman

- - -

## context-aware-config-v0.14.3 - 2024-01-29
#### Bug Fixes
- added partitions for audit_log table in cac schema - (d771050) - Shubhranshu Sanjeev

- - -

## context-aware-config-v0.14.2 - 2024-01-18
#### Bug Fixes
- error resolving pages with internal call to server - (084d08b) - Shubhranshu Sanjeev

- - -

## context-aware-config-v0.14.1 - 2024-01-12
#### Bug Fixes
- frontend build process - (cbdad01) - Shubhranshu Sanjeev

- - -

## context-aware-config-v0.14.0 - 2024-01-04
#### Bug Fixes
- fixed ci-test to support multi-tenant setup - (916b75d) - Shubhranshu Sanjeev
#### Features
- working resolve page - (803dfbd) - Kartik Gajendra
- fixed theme + ui changes + form validation + context validation error handling - (6cf5929) - Saurav Suman
- working experiments page - (9a1d74c) - Kartik Gajendra
- added experiment-list page - (ee462fd) - Shubhranshu Sanjeev
- experiment UI - (24e1b56) - Kartik Gajendra
- ui for cac and exp - (41f884f) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- formatted code + cleanup - (6d4874b) - Shubhranshu Sanjeev
#### Refactoring
- fixed warnings, added redirection for home page and script for setting up the project - (6b21fb9) - Saurav Suman

- - -

## context-aware-config-v0.13.2 - 2023-12-27
#### Bug Fixes
- [PICAF-25568] array validation for in condition - (a45ac4a) - Pratik Mishra
- PICAF-24961 fix json schema validation - (ed6f814) - ankit.mahato

- - -

## context-aware-config-v0.13.1 - 2023-11-22
#### Bug Fixes
- PICAF-25066 sort json while context creation - (3bd7a97) - Pratik Mishra

- - -

## context-aware-config-v0.13.0 - 2023-11-16
#### Features
- update default keys - (d6b9992) - ankit.mahato

- - -

## context-aware-config-v0.12.0 - 2023-11-11
#### Features
- added format check in the JenkinsFile(PICAF-24813) - (4fdf864) - Saurav Suman
- added frontend crate,combined frontend and backend binaries (PICAF-24540) - (ee084ba) - Saurav Suman
#### Miscellaneous Chores
- [PICAF-24778] move dependencies to workspaces - (38a524f) - Kartik Gajendra

- - -

## context-aware-config-v0.11.0 - 2023-11-08
#### Bug Fixes
- make sure envs with defaults prevent failure - (aac0303) - Kartik Gajendra
#### Features
- [PICAF-24779] integrate authorize middleware - (4a582f3) - Kartik Gajendra

- - -

## context-aware-config-v0.10.2 - 2023-10-31
#### Bug Fixes
- PICAF-25020 x-tenant header mandate removed for OPTIONS calls - (9ee39b5) - Ritick Madaan

- - -

## context-aware-config-v0.10.1 - 2023-10-27
#### Bug Fixes
- fixed failing health check (x-tenant header not set) - (23af679) - Shubhranshu Sanjeev

- - -

## context-aware-config-v0.10.0 - 2023-10-25
#### Features
- added multi-tenant support - (5d34e78) - Shubhranshu Sanjeev
- added middleware and FromRequest for tenant and app scope info - (07a64ad) - Shubhranshu Sanjeev
#### Refactoring
- moved tables and types out of cac_v1 schema - (f70a0c5) - Shubhranshu Sanjeev

- - -

## context-aware-config-v0.9.0 - 2023-10-20
#### Features
- PICAF-23643 - Dimension value schema validation on context-addition - (b2fad9e) - Prasanna P

- - -

## context-aware-config-v0.8.0 - 2023-10-10
#### Features
- support to update experiment override_keys and variants - (9432bf7) - Shubhranshu Sanjeev
#### Refactoring
- resolved comments - (aefb03e) - Shubhranshu Sanjeev

- - -

## context-aware-config-v0.7.1 - 2023-10-10
#### Bug Fixes
- PICAF-24742 add migration for changing default_configs_keys - (55f8895) - Pratik Mishra

- - -

## context-aware-config-v0.7.0 - 2023-10-09
#### Features
- server's keep-alive time and db connection pool max size made configurable - (110ee00) - Ritick Madaan
#### Miscellaneous Chores
- database migration for dimensions table - (3a36c56) - Ritick Madaan

- - -

## context-aware-config-v0.6.1 - 2023-10-05
#### Bug Fixes
- [PICAF-24563] add user struct in delete context API - (9a0360d) - Kartik Gajendra

- - -

## context-aware-config-v0.6.0 - 2023-10-05
#### Features
- [PICAF-24563] added dashboard auth middleware - (955d9e9) - Kartik Gajendra
- PICAF-24664 cors middleware attached - (8cb4805) - Ritick Madaan

- - -

## context-aware-config-v0.5.1 - 2023-09-20
#### Bug Fixes
- PICAF-24507 patching overrides on default-config instead of merge - (2c09e32) - Ritick Madaan

- - -

## context-aware-config-v0.5.0 - 2023-09-12
#### Bug Fixes
- PICAF-24223 eval param fix - (9d4d678) - Pratik Mishra
#### Features
- PICAF-24223 Adding generic eval - (b94ce46) - Pratik Mishra
- Schema addition for Dimension values - (7960a67) - Prasanna P

- - -

## context-aware-config-v0.4.0 - 2023-09-06
#### Features
- [PICAF-24065] added pod information in response headers and logs - (5ee8a9c) - Kartik Gajendra

- - -

## context-aware-config-v0.3.0 - 2023-09-05
#### Features
- [PICAF-24073] add audit log search endpoint - (19f75c7) - Kartik Gajendra

- - -

## context-aware-config-v0.2.0 - 2023-09-05
#### Features
- PICAF-23598 implemented tracing-actix-web for logging - (63dee8c) - Ritick Madaan

- - -

## context-aware-config-v0.1.0 - 2023-09-01
#### Bug Fixes
- added middleware to insert version in response headers - (449eea4) - Shubhranshu Sanjeev
- PICAF-24023 sorting same priority contexts with created_at - (24852f5) - Ritick Madaan
- using audit log tstamp for checking last-modified - (2ccaa7e) - Shubhranshu Sanjeev
- calling cac apis for creating context - (a7d92f5) - Shubhranshu Sanjeev
- PICAF-23545 updated response-type of /context/bulk-operations api - (1640986) - Ritick Madaan
#### Continuous Integration
- PICAF-23646 enabling tests in pr builds - (d09f566) - Ritick Madaan
#### Features
- added log table for all cac_v1 tables - (88a3328) - Shubhranshu Sanjeev
- [PICAF-23632] added experimentation client with few fixes - (9a31815) - Kartik Gajendra
#### Refactoring
- improvements to APIs - (60bf5c0) - Shubhranshu Sanjeev
- moved cac to cargo workspaces - (1855ef8) - Shubhranshu Sanjeev

- - -

Changelog generated by [cocogitto](https://github.com/cocogitto/cocogitto).