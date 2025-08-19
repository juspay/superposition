# Changelog
All notable changes to this project will be documented in this file. See [conventional commits](https://www.conventionalcommits.org/) for commit guidelines.

- - -
## superposition_types-v0.31.0 - 2025-08-19
#### Features
- Move jsonlogic behind compile time flag - (328671c) - ayush.jain@juspay.in
#### Miscellaneous Chores
- **(version)** v0.87.0 [skip ci] - (87e0763) - Superposition Bot

- - -

## superposition_types-v0.30.1 - 2025-08-13
#### Bug Fixes
- show proper license in crates.io (#657) - (9406a0d) - Datron
#### Miscellaneous Chores
- **(version)** v0.86.0 [skip ci] - (f11fc97) - Superposition Bot

- - -

## superposition_types-v0.30.0 - 2025-08-12
#### Features
- control population - (aa9d23d) - Ankit.Mahato
#### Miscellaneous Chores
- **(version)** v0.85.1 [skip ci] - (ab6070f) - Superposition Bot

- - -

## superposition_types-v0.29.1 - 2025-08-08
#### Bug Fixes
- Use workspace superposition_types in frontend crate (#650) - (613abce) - Ayush Jain
#### Miscellaneous Chores
- **(version)** v0.83.0 [skip ci] - (2e2d48c) - Superposition Bot

- - -

## superposition_types-v0.29.0 - 2025-08-04
#### Features
- publish rust libraries through crates.io - (8b62d45) - datron

- - -

## superposition_types-v0.26.1 - 2025-07-31
#### Bug Fixes
- Add created_by and created_at column for functions table (#626) - (984c436) - Ayush Jain

- - -

## superposition_types-v0.26.0 - 2025-07-30
#### Bug Fixes
- bucketing queries (#628) - (a475cdf) - Ankit Kumar Mahato
#### Features
- experiment bucketing - (274afdf) - Ankit.Mahato

- - -

## superposition_types-v0.25.0 - 2025-07-28
#### Features
- Standalone page for dimensions, default-configs, type-templates, webhooks (#614) - (f2f7d1a) - Ayush Jain

- - -

## superposition_types-v0.24.1 - 2025-07-24
#### Bug Fixes
- Dimension api types unification (#609) - (65e64a6) - Ayush Jain

- - -

## superposition_types-v0.24.0 - 2025-07-10
#### Bug Fixes
- Revive Function Page UI - (6e77ed5) - ayush.jain@juspay.in
- Experiment list group id filter - (588e469) - ayush.jain@juspay.in
- Improve compare page UI - (dd2a560) - ayush.jain@juspay.in
#### Features
- Add change log popup for override, default-config, dimension, type-template, webhook and experiment changes (#532) - (aeb1bf3) - Ayush Jain
#### Miscellaneous Chores
- Add missing audit log for experiment group table - (556df54) - ayush.jain@juspay.in

- - -

## superposition_types-v0.23.0 - 2025-07-03
#### Bug Fixes
- Show experiment filters - (a6a0e7e) - ayush.jain@juspay.in
- Make forms look consistent - (024478d) - ayush.jain@juspay.in
- Default Config page UX issues - (939505e) - ayush.jain@juspay.in
- experiment client fetch fix (#555) - (cb08467) - PRATIK MISHRA
- added validation for and wrapper over context json logic - (a48cc71) - Shubhranshu Sanjeev
#### Features
- added experiment openfeature support with uniffi (#561) - (dfad18c) - PRATIK MISHRA
- added a CRUD for experiment groups (#540) - (6eedef2) - Datron
- Rust Version Bump + Uniffi + Java OpenFeature SDK (#553) - (1aba4b4) - ShreyBana
- experiment group integration (#526) - (73d0950) - Ankit Kumar Mahato
- Workspace setting for allowing experiment self approval (#552) - (472f03e) - Ayush Jain

- - -

## superposition_types-v0.22.0 - 2025-06-23
#### Bug Fixes
- corrected decode for post in resolve config (#545) - (892092e) - George James
- Show context filters (#531) - (a0ad626) - Ayush Jain
- stack overflow in applicable variants and updated versions for the smithy client (#538) - (477e402) - George James
#### Features
- add experiment groups API - (c09cef1) - Kartik
- set config version (#512) - (bc662bd) - PRATIK MISHRA

- - -

## superposition_types-v0.21.0 - 2025-05-22
#### Bug Fixes
- Fixing usage of default (#518) - (bc936a7) - vraghunandhan
- Removing completed and discarded experiments from experiment homescreen (#517) - (f80f250) - vraghunandhan
#### Features
- Frontend changes for delete experiment (#511) - (792c4cd) - Ayush Jain
- introduce experiment groups data model - (10815da) - Kartik

- - -

## superposition_types-v0.20.0 - 2025-05-16
#### Features
- delete overrides by experiment (#500) - (51a20e3) - PRATIK MISHRA
- add pause experiment (#509) - (36defc9) - Ankit Kumar Mahato

- - -

## superposition_types-v0.19.0 - 2025-05-12
#### Features
- Metrics initial setup - (530586d) - ayush.jain@juspay.in

- - -

## superposition_types-v0.18.0 - 2025-05-09
#### Bug Fixes
- webhook implementation (#503) - (1ffd6eb) - Ankit Kumar Mahato
- table key overflow issue (#502) - (049fd00) - Ayush Jain
- Make last_modified non-mandatory in experiments/list and fix UI behaviour - (507d716) - ayush.jain@juspay.in
#### Features
- DefaultConfig type unification and conclude/exp contract fix (#497) - (395e6eb) - Ayush Jain
- webhooks ui - (e0257dc) - Ankit.Mahato
- Update override using context id - (6c2543d) - Ayush Jain
- added info modal and change form for description and change reason (#437) - (4352eb3) - sauraww
- add strict mode support for workspaces (#470) - (63e17f8) - Datron
- webhook cruds (#313) - (292689e) - Ankit Kumar Mahato
- add support to link autocomplete functions to dimensions - (2d5540c) - Kartik

- - -

## superposition_types-v0.17.1 - 2025-04-29
#### Bug Fixes
- Page reset issue in frontend (#491) - (b9a0659) - Ayush Jain
- Empty check for keys of struct - (8d3b709) - ayush.jain@juspay.in

- - -

## superposition_types-v0.17.0 - 2025-04-24
#### Bug Fixes
- Update experiment restrictions and default value of sort_by (#485) - (f7542f4) - Ayush Jain
- corrected sql constraint (#482) - (8052357) - PRATIK MISHRA
#### Features
- Paginate Context overrides page (#457) - (72a74b1) - Ayush Jain
#### Tests
- Experiment - (b5a8120) - ayush.jain@juspay.in

- - -

## superposition_types-v0.16.0 - 2025-04-17
#### Features
- Add dimension form support for dependent dimensions - (3929049) - Ankit.Mahato
- smithy models - (2958cce) - PRATIK MISHRA
- Add types for experiment apis in superposition_types - (219a2eb) - Ayush Jain
- dependent dimensions - (5a89f5b) - Ankit Kumar Mahato

- - -

## superposition_types-v0.15.0 - 2025-04-03
#### Bug Fixes
- config version alter query - (a4ddf7c) - Kartik
#### Features
- add autocomplete functions - (1f39003) - Kartik

- - -

## superposition_types-v0.14.0 - 2025-03-26
#### Features
- experiment filters in UI - (d4604e4) - Kartik

- - -

## superposition_types-v0.13.0 - 2025-03-10
#### Features
- add key filter for default config - (d29bc18) - Datron

- - -

## superposition_types-v0.12.2 - 2025-02-28
#### Bug Fixes
- optimize db query for update (#390) - (258933f) - PRATIK MISHRA
- Fixed clippy warnings - (88bbfe9) - ShreyBana

- - -

## superposition_types-v0.12.1 - 2025-02-19
#### Bug Fixes
- tenant config - (416f066) - Ankit Kumar Mahato

- - -

## superposition_types-v0.12.0 - 2025-02-14
#### Features
- Discard experiment - (42ac967) - ayush.jain@juspay.in

- - -

## superposition_types-v0.11.0 - 2025-01-23
#### Bug Fixes
- mandatory dimensions with workspaces (#370) - (e5b8097) - Datron
- resolved comments - (09ea160) - Shubhranshu Sanjeev
- experiment handlers to send org_id - (571f201) - Kartik
- add org tests - (deab731) - Pratik Mishra
- read from workspaces table when showing the dropdown - (1594218) - Kartik
#### Features
- workspace-ui - (4ccd9c6) - Ankit.Mahato
- added workspaces table and workspace management APIs - (5910f3e) - Kartik
- added description and comment (#284) - (4e0006c) - sauraww
- JsonLogic algebraic representation - (1ac1a5b) - Shubhranshu Sanjeev
- added schema and crl apis for organisation (#322) - (c333f3a) - sauraww
- Add auth via OAUTH2 (#321) - (f5092f8) - Ayush Jain
- Use Common db model types in frontend (#291) - (e68782d) - Ayush Jain
#### Miscellaneous Chores
- merge cac and experimentation schemas - (8863875) - Kartik
- formatting - (c3a1ca1) - Shubhranshu Sanjeev
#### Refactoring
- OrgId, WorkspaceId, SchemaName cleanup and refactor (#379) - (470ab48) - Ayush Jain
- workspace ui & form (#373) - (9dca3aa) - Shubhranshu Sanjeev
- added schema_name dsl to cac queries - (73d6ba2) - Shubhranshu Sanjeev
- merge cac and experimentation schemas - (51367a6) - Kartik

- - -

## superposition_types-v0.10.0 - 2025-01-06
#### Features
- Replace priority with position (#299) - (61e052a) - PRATIK MISHRA
- Use dimension[] for dimension params in context/list (#303) - (0eb9086) - Ayush Jain
- search and sort experiments - (95b87c5) - Datron

- - -

## superposition_types-v0.9.0 - 2024-12-04
#### Bug Fixes
- webhook optional field (#296) - (fe0171f) - Ankit Kumar Mahato
#### Features
- priority recompute restructure (#279) - (849063d) - PRATIK MISHRA

- - -

## superposition_types-v0.8.0 - 2024-12-02
#### Bug Fixes
- Filter config by prefix (#293) - (c2e5726) - Ayush Jain
- Add exp feature flag - (5ff2807) - ayush.jain@juspay.in
#### Features
- Revert seperate crate creation - (a6e905d) - ayush.jain@juspay.in
- Renaming - (e987e40) - ayush.jain@juspay.in
- Common changes - (9c68567) - ayush.jain@juspay.in
- Exp model types migration - (f0dfe8c) - ayush.jain@juspay.in
- Cac model types migration - (b15f1b7) - ayush.jain@juspay.in

- - -

## superposition_types-v0.7.1 - 2024-11-22
#### Bug Fixes
- fixed and improved webhooks (#283) - (4864849) - Ankit Kumar Mahato

- - -

## superposition_types-v0.7.0 - 2024-11-20
#### Bug Fixes
- Add pagination to list APIs (#209) - (c155bb0) - Ankit Kumar Mahato
#### Features
- use new input type for json schema inputs as well - (e7075be) - Kartik
- Webhook trigger for experiments (#265) - (585ee1e) - Ankit Kumar Mahato

- - -

## superposition_types-v0.6.0 - 2024-10-23
#### Features
- Search contexts by dimension values (#264) - (12743af) - Ayush Jain

- - -

## superposition_types-v0.5.0 - 2024-10-08
#### Features
- custom query-param extractor for platform queries (#242) - (3031e48) - Shubhranshu Sanjeev
- get config-versions api (#248) - (260392c) - Ankit Kumar Mahato

- - -

## superposition_types-v0.4.0 - 2024-09-30
#### Bug Fixes
- Dependency pruning (#250) - (8b68900) - Ayush Jain
#### Features
- Tenant specific config support via .cac.toml (#246) - (ffc247e) - Ayush Jain
#### Miscellaneous Chores
- Delete SuperpositionUser trait (#251) - (e77ae0b) - Ayush Jain

- - -

## superposition_types-v0.3.2 - 2024-08-14
#### Bug Fixes
- newtypes for entity fields (#199) - (49562b7) - PRATIK MISHRA

- - -

## superposition_types-v0.3.1 - 2024-08-08
#### Bug Fixes
- api validation with new types (#146) - (66ad741) - PRATIK MISHRA

- - -

## superposition_types-v0.3.0 - 2024-07-11
#### Features
- move apperror to superposition_types - (f1c8395) - Pratik Mishra
#### Miscellaneous Chores
- simplify module structure and follow Rust 2018 edition module naming convention (#86) - (403ea59) - Sanchith Hegde

- - -

## superposition_types-v0.2.1 - 2024-05-10
#### Bug Fixes
- remove juspay email address for anon user - (f174428) - Kartik

- - -

## superposition_types-v0.2.0 - 2024-05-06
#### Features
- add auth_type so this can be used when making API calls - (4427384) - Kartik
- created new types crate for superposition movement - (c9c56d1) - Kartik
#### Miscellaneous Chores
- **(version)** v0.42.0 [skip ci] - (e4bc080) - Superposition Bot

- - -

## superposition_types-v0.2.0 - 2024-05-06
#### Features
- add auth_type so this can be used when making API calls - (4427384) - Kartik
- created new types crate for superposition movement - (c9c56d1) - Kartik

- - -

Changelog generated by [cocogitto](https://github.com/cocogitto/cocogitto).