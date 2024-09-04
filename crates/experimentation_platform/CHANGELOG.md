# Changelog
All notable changes to this project will be documented in this file. See [conventional commits](https://www.conventionalcommits.org/) for commit guidelines.

- - -
## experimentation_platform-v0.18.1 - 2024-08-08
#### Bug Fixes
- jsonschema for dimension and remove default_config's jsonsschema check (#197) - (89a23af) - PRATIK MISHRA
- api validation with new types (#146) - (66ad741) - PRATIK MISHRA

- - -

## experimentation_platform-v0.18.0 - 2024-07-11
#### Bug Fixes
- x-config-version in get config and experiments response (#152) - (1a429a9) - Ayush Jain
#### Features
- move apperror to superposition_types - (f1c8395) - Pratik Mishra

- - -

## experimentation_platform-v0.17.0 - 2024-06-20
#### Features
- add config version header in api response (#87) - (213a21e) - PRATIK MISHRA

- - -

## experimentation_platform-v0.16.0 - 2024-06-11
#### Bug Fixes
- add patch_file in diesel.toml - (5ec9835) - Pratik Mishra
#### Features
- snapshot changes - apis - (69588f6) - Pratik Mishra
#### Miscellaneous Chores
- address some clippy lints (#85) - (001b8d4) - Sanchith Hegde
- simplify module structure and follow Rust 2018 edition module naming convention (#86) - (403ea59) - Sanchith Hegde

- - -

## experimentation_platform-v0.15.1 - 2024-05-29
#### Bug Fixes
- creating experiments for default-config (no context) (#38) - (4f6b92c) - Sauravcv98
- reject experiment contexts with `variantIds` (#29) - (092e568) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- Add CI check to lint the .sql files based on rules defined in .editorconfig - (16bf460) - Hao

- - -

## experimentation_platform-v0.15.0 - 2024-05-06
#### Bug Fixes
- Do not lowercase dmension inputs while resolving (#11) - (8536a84) - Ayush Jain
- post merge release tagging - (f589018) - Kartik
#### Features
- Added grouping in default_config page (#9) - (9f4a46a) - Ankit Kumar Mahato
- ready for open source! - (f48db35) - Kartik
#### Miscellaneous Chores
- **(version)** v0.42.0 [skip ci] - (e4bc080) - Superposition Bot
- **(version)** v0.41.0 [skip ci] - (ceefd83) - Superposition Bot
- **(version)** v0.40.0 [skip ci] - (836b769) - Jenkins
- open source superposition - (b85a0a8) - Kartik

- - -

## experimentation_platform-v0.15.0 - 2024-05-06
#### Bug Fixes
- Do not lowercase dmension inputs while resolving (#11) - (8536a84) - Ayush Jain
- post merge release tagging - (f589018) - Kartik
#### Features
- Added grouping in default_config page (#9) - (9f4a46a) - Ankit Kumar Mahato
- ready for open source! - (f48db35) - Kartik
#### Miscellaneous Chores
- **(version)** v0.41.0 [skip ci] - (ceefd83) - Superposition Bot
- **(version)** v0.40.0 [skip ci] - (836b769) - Jenkins
- open source superposition - (b85a0a8) - Kartik

- - -

## experimentation_platform-v0.14.0 - 2024-04-24
#### Bug Fixes
- post merge release tagging - (3b7e262) - Kartik
#### Features
- ready for open source! - (5f7af15) - Kartik
#### Miscellaneous Chores
- **(version)** v0.40.0 [skip ci] - (cdfe3f4) - Jenkins
- open source superposition - (cbd5b6f) - Kartik

- - -

## experimentation_platform-v0.13.0 - 2024-04-18
#### Features
- ready for open source! - (b7d36be) - Kartik

- - -

## experimentation-platform-v0.12.0 - 2024-04-05
#### Documentation
- PICAF-25981: add intro doc and features - (64fa30f) - Natarajan Kannan
#### Features
- [PICAF-26126] haskell client for superposition - (651a66d) - Kartik

- - -

## experimentation-platform-v0.11.0 - 2024-03-18
#### Documentation
- PICAF-25981: add intro doc and features - (d09ba53) - Natarajan Kannan
#### Features
- [PICAF-26126] haskell client for superposition - (7106b56) - Kartik

- - -

## experimentation-platform-v0.10.0 - 2024-03-08
#### Features
- PICAF-25884 Added function validation for context and default_config - (990b729) - ankit.mahato

- - -

## experimentation-platform-v0.9.4 - 2024-02-27
#### Bug Fixes
- returning error response if CAC call not 200 - (fa0eb5e) - Shubhranshu Sanjeev

- - -

## experimentation-platform-v0.9.3 - 2024-02-15
#### Bug Fixes
- fixing error message for experiment create and bulk context api - (bc0d7be) - Jenkins

- - -

## experimentation-platform-v0.9.2 - 2024-01-29
#### Bug Fixes
- added partitions for audit_log table in cac schema - (d771050) - Shubhranshu Sanjeev

- - -

## experimentation-platform-v0.9.1 - 2024-01-22
#### Bug Fixes
- added partitions for 2025 and 2026 for audit table - (45d37dd) - Shubhranshu Sanjeev

- - -

## experimentation-platform-v0.9.0 - 2024-01-04
#### Bug Fixes
- fixed ci-test to support multi-tenant setup - (916b75d) - Shubhranshu Sanjeev
- fixed experiment list page feedback - (f406264) - Shubhranshu Sanjeev
#### Features
- working resolve page - (803dfbd) - Kartik Gajendra

- - -

## experimentation-platform-v0.8.2 - 2023-11-30
#### Bug Fixes
- allow ramp 0 - (b8d49aa) - Kartik Gajendra

- - -

## experimentation-platform-v0.8.1 - 2023-11-16
#### Bug Fixes
- add different auth types for exp requests to CAC - (bd8ae88) - Kartik Gajendra

- - -

## experimentation-platform-v0.8.0 - 2023-11-11
#### Features
- added format check in the JenkinsFile(PICAF-24813) - (4fdf864) - Saurav Suman
#### Miscellaneous Chores
- [PICAF-24778] move dependencies to workspaces - (38a524f) - Kartik Gajendra

- - -

## experimentation-platform-v0.7.1 - 2023-11-09
#### Bug Fixes
- Removing acceptance of override_keys in experiment create/update - (033597e) - ankit.mahato

- - -

## experimentation-platform-v0.7.0 - 2023-11-08
#### Features
- [PICAF-24779] integrate authorize middleware - (4a582f3) - Kartik Gajendra

- - -

## experimentation-platform-v0.6.0 - 2023-10-25
#### Features
- added multi-tenant support - (5d34e78) - Shubhranshu Sanjeev
- added middleware and FromRequest for tenant and app scope info - (07a64ad) - Shubhranshu Sanjeev
#### Refactoring
- moved tables and types out of cac_v1 schema - (f70a0c5) - Shubhranshu Sanjeev

- - -

## experimentation-platform-v0.5.0 - 2023-10-10
#### Bug Fixes
- validating override_keys for unique entries - (36cf523) - Shubhranshu Sanjeev
#### Features
- support to update experiment override_keys and variants - (9432bf7) - Shubhranshu Sanjeev
#### Refactoring
- resolved comments - (aefb03e) - Shubhranshu Sanjeev
#### Tests
- added tests for experiment helper fnxs - (ea4db17) - Shubhranshu Sanjeev

- - -

## experimentation-platform-v0.4.0 - 2023-10-05
#### Features
- [PICAF-24563] added dashboard auth middleware - (955d9e9) - Kartik Gajendra

- - -

## experimentation-platform-v0.3.1 - 2023-09-12
#### Bug Fixes
- failed build due to untracked schema.rs file changes - (5bc4eae) - Shubhranshu Sanjeev
- fixed random timeouts in internal http calls to CAC - (a4e95a3) - Shubhranshu Sanjeev

- - -

## experimentation-platform-v0.3.0 - 2023-09-06
#### Features
- [PICAF-24160] record the chosen variant after conclude - (1c3c6e6) - Kartik Gajendra

- - -

## experimentation-platform-v0.2.0 - 2023-09-05
#### Features
- [PICAF-24073] add audit log search endpoint - (19f75c7) - Kartik Gajendra

- - -

## experimentation-platform-v0.1.0 - 2023-09-01
#### Bug Fixes
- using audit log tstamp for checking last-modified - (2ccaa7e) - Shubhranshu Sanjeev
- [PICAF-23846] added total items to list API response - (17955fa) - Kartik Gajendra
- removed traffic-percentage from experiment create request - (2a62555) - Shubhranshu Sanjeev
- PICAF-23632 - (247542e) - Ritick Madaan
- PICAF-23622 updated last_modified in ramp - (fcbaaa4) - ankit.mahato
- calling cac apis for creating context - (a7d92f5) - Shubhranshu Sanjeev
- moved tables and types under cac_v1 schema - (1be82f1) - Shubhranshu Sanjeev
- added last_modified column and indexes - (942d723) - Shubhranshu Sanjeev
- fixed context overlap check logic - (691eae7) - Shubhranshu Sanjeev
#### Continuous Integration
- regenerated schema.patch with latest schema.rs - (390818a) - Ritick Madaan
#### Features
- [PICAF-23868] Added Catch all error type for robust error handling - (91386ee) - Kartik Gajendra
- [PICAF-23868] Added Catch all error type for robust error handling - (60f6f2a) - Kartik Gajendra
- added log table for all cac_v1 tables - (88a3328) - Shubhranshu Sanjeev
- [PICAF-23856] add support for last - (d23ee26) - Kartik Gajendra
- [PICAF-23632] added experimentation client with few fixes - (9a31815) - Kartik Gajendra
- added conclude functionality for experiments - (4def4bc) - Shubhranshu Sanjeev
- [PICAF-23502] added list experiments API - (01b52cc) - Kartik Gajendra
#### Refactoring
- improvements to APIs - (60bf5c0) - Shubhranshu Sanjeev

- - -

Changelog generated by [cocogitto](https://github.com/cocogitto/cocogitto).