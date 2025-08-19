# Changelog
All notable changes to this project will be documented in this file. See [conventional commits](https://www.conventionalcommits.org/) for commit guidelines.

- - -
## experimentation_platform-v0.41.0 - 2025-08-19
#### Features
- Move jsonlogic behind compile time flag - (328671c) - ayush.jain@juspay.in
#### Miscellaneous Chores
- **(version)** v0.86.0 [skip ci] - (f11fc97) - Superposition Bot

- - -

## experimentation_platform-v0.40.0 - 2025-08-12
#### Features
- control population - (aa9d23d) - Ankit.Mahato
#### Miscellaneous Chores
- **(version)** v0.85.1 [skip ci] - (ab6070f) - Superposition Bot

- - -

## experimentation_platform-v0.39.1 - 2025-08-08
#### Bug Fixes
- Use workspace superposition_types in frontend crate (#650) - (613abce) - Ayush Jain
- experiment group backfilling user details - (8a4788c) - Ankit.Mahato
#### Miscellaneous Chores
- **(version)** v0.83.0 [skip ci] - (2e2d48c) - Superposition Bot

- - -

## experimentation_platform-v0.39.0 - 2025-08-04
#### Features
- publish rust libraries through crates.io - (8b62d45) - datron

- - -

## experimentation_platform-v0.36.0 - 2025-07-30
#### Features
- experiment bucketing - (274afdf) - Ankit.Mahato

- - -

## experimentation_platform-v0.35.1 - 2025-07-10
#### Bug Fixes
- Revive Function Page UI - (6e77ed5) - ayush.jain@juspay.in
- Experiment list group id filter - (588e469) - ayush.jain@juspay.in

- - -

## experimentation_platform-v0.35.0 - 2025-07-03
#### Bug Fixes
- Show experiment filters - (a6a0e7e) - ayush.jain@juspay.in
- added validation for and wrapper over context json logic - (a48cc71) - Shubhranshu Sanjeev
#### Features
- added a CRUD for experiment groups (#540) - (6eedef2) - Datron
- experiment group integration (#526) - (73d0950) - Ankit Kumar Mahato
- Workspace setting for allowing experiment self approval (#552) - (472f03e) - Ayush Jain

- - -

## experimentation_platform-v0.34.0 - 2025-06-23
#### Bug Fixes
- List exp behaviour for list all (#528) - (24d287d) - Ayush Jain
#### Features
- add experiment groups API - (c09cef1) - Kartik

- - -

## experimentation_platform-v0.33.0 - 2025-05-22
#### Features
- Frontend changes for delete experiment (#511) - (792c4cd) - Ayush Jain

- - -

## experimentation_platform-v0.32.0 - 2025-05-16
#### Features
- delete overrides by experiment (#500) - (51a20e3) - PRATIK MISHRA
- add pause experiment (#509) - (36defc9) - Ankit Kumar Mahato

- - -

## experimentation_platform-v0.31.0 - 2025-05-12
#### Features
- Metrics initial setup - (530586d) - ayush.jain@juspay.in

- - -

## experimentation_platform-v0.30.0 - 2025-05-09
#### Bug Fixes
- webhook implementation (#503) - (1ffd6eb) - Ankit Kumar Mahato
- Make last_modified non-mandatory in experiments/list and fix UI behaviour - (507d716) - ayush.jain@juspay.in
#### Features
- DefaultConfig type unification and conclude/exp contract fix (#497) - (395e6eb) - Ayush Jain
- added conclude changeform , change_reason propogation to overrides , fixed context move (#499) - (dea0626) - sauraww
- Update override using context id - (6c2543d) - Ayush Jain
- webhook cruds (#313) - (292689e) - Ankit Kumar Mahato

- - -

## experimentation_platform-v0.29.0 - 2025-04-24
#### Bug Fixes
- Update experiment restrictions and default value of sort_by (#485) - (f7542f4) - Ayush Jain
#### Features
- Paginate Context overrides page (#457) - (72a74b1) - Ayush Jain

- - -

## experimentation_platform-v0.28.0 - 2025-04-17
#### Features
- smithy models - (2958cce) - PRATIK MISHRA
- Add types for experiment apis in superposition_types - (219a2eb) - Ayush Jain

- - -

## experimentation_platform-v0.27.0 - 2025-03-26
#### Features
- experiment filters in UI - (d4604e4) - Kartik

- - -

## experimentation_platform-v0.26.2 - 2025-02-28
#### Bug Fixes
- Fixed clippy warnings - (88bbfe9) - ShreyBana

- - -

## experimentation_platform-v0.26.1 - 2025-02-19
#### Bug Fixes
- Unsafe error handling in experimentation (#421) - (2cb6840) - Ayush Jain

- - -

## experimentation_platform-v0.26.0 - 2025-02-14
#### Bug Fixes
- Update experiments page - (d0c8d4e) - ayush.jain@juspay.in
#### Features
- Discard experiment - (42ac967) - ayush.jain@juspay.in

- - -

## experimentation_platform-v0.25.0 - 2025-01-23
#### Bug Fixes
- experiment handlers to send org_id - (571f201) - Kartik
- added missing returning DSLs - (c6735b3) - Shubhranshu Sanjeev
#### Features
- added description and comment (#284) - (4e0006c) - sauraww
- Use Common db model types in frontend (#291) - (e68782d) - Ayush Jain
#### Miscellaneous Chores
- formatting - (c3a1ca1) - Shubhranshu Sanjeev
#### Refactoring
- OrgId, WorkspaceId, SchemaName cleanup and refactor (#379) - (470ab48) - Ayush Jain
- workspace ui & form (#373) - (9dca3aa) - Shubhranshu Sanjeev
- added schema_name dsl to experimentation queries - (9642d20) - Shubhranshu Sanjeev
- merge cac and experimentation schemas - (51367a6) - Kartik

- - -

## experimentation_platform-v0.24.0 - 2025-01-06
#### Bug Fixes
- webhook kms decrypt auth key (#332) - (09ebc29) - Ankit Kumar Mahato
- webhook payload (#302) - (90beb70) - Ankit Kumar Mahato
#### Features
- Replace priority with position (#299) - (61e052a) - PRATIK MISHRA
- search and sort experiments - (95b87c5) - Datron

- - -

## experimentation_platform-v0.23.1 - 2024-12-04
#### Bug Fixes
- disregarding experiments last-modified filter in case of (#295) - (516a2c1) - Shubhranshu Sanjeev

- - -

## experimentation_platform-v0.23.0 - 2024-12-02
#### Bug Fixes
- Add exp feature flag - (5ff2807) - ayush.jain@juspay.in
#### Features
- Revert seperate crate creation - (a6e905d) - ayush.jain@juspay.in
- Renaming - (e987e40) - ayush.jain@juspay.in
- Exp model types migration - (f0dfe8c) - ayush.jain@juspay.in

- - -

## experimentation_platform-v0.22.1 - 2024-11-22
#### Bug Fixes
- fixed and improved webhooks (#283) - (4864849) - Ankit Kumar Mahato

- - -

## experimentation_platform-v0.22.0 - 2024-11-20
#### Bug Fixes
- Add pagination to list APIs (#209) - (c155bb0) - Ankit Kumar Mahato
#### Features
- Webhook trigger for experiments (#265) - (585ee1e) - Ankit Kumar Mahato

- - -

## experimentation_platform-v0.21.0 - 2024-10-23
#### Features
- Search contexts by dimension values (#264) - (12743af) - Ayush Jain

- - -

## experimentation_platform-v0.20.0 - 2024-10-17
#### Features
- use concrete Variant type in db model (#241) - (8ca8135) - Ayush Jain

- - -

## experimentation_platform-v0.19.0 - 2024-09-30
#### Bug Fixes
- Dependency pruning (#250) - (8b68900) - Ayush Jain
- Make experiment response type consistent with the db type (#233) - (a844ed3) - Ayush Jain
#### Features
- Tenant specific config support via .cac.toml (#246) - (ffc247e) - Ayush Jain
- Add get_applicable_variants as expt endpoint (#210) - (54f2037) - Ayush Jain
#### Miscellaneous Chores
- Delete SuperpositionUser trait (#251) - (e77ae0b) - Ayush Jain

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