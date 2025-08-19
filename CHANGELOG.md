# Changelog
All notable changes to this project will be documented in this file. See [conventional commits](https://www.conventionalcommits.org/) for commit guidelines.

- - -
## v0.88.0 - 2025-08-19
### Package updates
- experimentation_client bumped to experimentation_client-v0.18.0
- cac_client bumped to cac_client-v0.21.0
- experimentation_platform bumped to experimentation_platform-v0.41.0
- context_aware_config bumped to context_aware_config-v0.60.0
- superposition_types bumped to superposition_types-v0.31.0
- frontend bumped to frontend-v0.41.0
- service_utils bumped to service_utils-v0.33.0
- superposition_core bumped to superposition_core-v0.6.0
### Global changes
#### Documentation
- update Java badges and minor fixes - (9f0b880) - Natarajan Kannan
- update Java badges and minor fixes - (f586489) - Natarajan Kannan
#### Features
- Move jsonlogic behind compile time flag - (328671c) - ayush.jain@juspay.in
#### Miscellaneous Chores
- **(version)** v0.87.0 [skip ci] - (87e0763) - Superposition Bot

- - -

## v0.87.0 - 2025-08-13
### Package updates
- superposition_types bumped to superposition_types-v0.30.1
- superposition_provider bumped to superposition_provider-v0.4.3
- superposition_sdk bumped to superposition_sdk-v0.5.0
- superposition_derives bumped to superposition_derives-v0.6.1
- superposition_core bumped to superposition_core-v0.5.1
### Global changes
#### Bug Fixes
- **(java)** Added docs, fixed doc generation & fixed release-ci for java. (#665) - (e67533a) - ShreyBana
- show proper license in crates.io (#657) - (9406a0d) - Datron
#### Documentation
- base README.md update (#655) - (1eb4f45) - Natarajan Kannan
- fix links - (6040455) - Natarajan Kannan
- fix links in intro.md - (9815cf9) - Natarajan Kannan
#### Features
- simplify workspace migration with an API endpoint (#585) - (0f58aed) - Datron
#### Miscellaneous Chores
- **(deps)** bump esbuild and vite in /examples/dynamic-payment-fields - (61ed7de) - dependabot[bot]
- **(deps)** bump slab in /examples/cac_redis_module - (330d0ff) - dependabot[bot]
- **(deps)** bump tokio in /examples/k8s-staggered-releaser - (07dd301) - dependabot[bot]
- **(deps)** bump openssl in /examples/k8s-staggered-releaser - (3c4cc57) - dependabot[bot]
- **(deps)** bump ring in /examples/k8s-staggered-releaser - (2239c4a) - dependabot[bot]
- **(version)** v0.86.0 [skip ci] - (f11fc97) - Superposition Bot

- - -

## v0.86.0 - 2025-08-12
### Package updates
- superposition_types bumped to superposition_types-v0.30.0
- superposition_sdk bumped to superposition_sdk-v0.4.0
- experimentation_platform bumped to experimentation_platform-v0.40.0
- frontend bumped to frontend-v0.40.0
- experimentation_client bumped to experimentation_client-v0.17.1
### Global changes
#### Documentation
- Add registry specific documentation to provider, bindings and SDK (#652) - (7badc93) - Datron
#### Features
- **(java)** Publishing to central-sonatype. (#638) - (5750a54) - ShreyBana
- control population - (aa9d23d) - Ankit.Mahato
#### Miscellaneous Chores
- **(version)** v0.85.1 [skip ci] - (ab6070f) - Superposition Bot
#### Tests
- Update test - (16af9bb) - ayush.jain@juspay.in

- - -

## v0.85.1 - 2025-08-08
### Package updates
- superposition_types bumped to superposition_types-v0.29.1
- experimentation_platform bumped to experimentation_platform-v0.39.1
- context_aware_config bumped to context_aware_config-v0.59.1
- superposition_provider bumped to superposition_provider-v0.4.2
- frontend bumped to frontend-v0.39.1
### Global changes
#### Bug Fixes
- **(haskell-sdk)** JSON & Show instances for Error types. (#649) - (6004dbf) - ShreyBana
- add patch file for rust (#646) - (1e90f08) - Datron
- Use workspace superposition_types in frontend crate (#650) - (613abce) - Ayush Jain
#### Documentation
- add readme badges (#645) - (5f9ad55) - Datron
#### Miscellaneous Chores
- **(version)** v0.85.0 [skip ci] - (fa5767d) - Superposition Bot

- - -

## v0.85.0 - 2025-08-06
### Package updates
- superposition_provider bumped to superposition_provider-v0.4.1
### Global changes
#### Bug Fixes
- python sdk package version and npm release - (a53bd49) - datron
- push to pypi registry - (c70f39e) - datron
- add smithy python sdk patch - (8764abf) - datron
#### Features
- verify domain for algolia - (4d92451) - datron
- add algolia search support to docusaurus - (35f9511) - datron
#### Miscellaneous Chores
- **(version)** v0.84.0 [skip ci] - (b5cfc08) - Superposition Bot

- - -

## v0.84.0 - 2025-08-05
### Package updates
- superposition_provider bumped to superposition_provider-v0.4.0
### Global changes
#### Features
- push npm and pypi packages to public registry - (6b31c97) - datron
#### Miscellaneous Chores
- **(version)** v0.83.0 [skip ci] - (2e2d48c) - Superposition Bot

- - -

## v0.83.0 - 2025-08-04
### Package updates
- superposition_derives bumped to superposition_derives-v0.6.0
- cac_client bumped to cac_client-v0.20.0
- cac_toml bumped to cac_toml-v0.5.0
- service_utils bumped to service_utils-v0.32.0
- superposition_provider bumped to superposition_provider-v0.3.0
- experimentation_client bumped to experimentation_client-v0.17.0
- experimentation_platform bumped to experimentation_platform-v0.39.0
- frontend bumped to frontend-v0.39.0
- context_aware_config bumped to context_aware_config-v0.59.0
- superposition_macros bumped to superposition_macros-v0.6.0
- superposition_sdk bumped to superposition_sdk-v0.3.0
- superposition_core bumped to superposition_core-v0.5.0
- superposition_types bumped to superposition_types-v0.29.0
### Global changes
#### Bug Fixes
- Juspay forked dependencies (#631) - (48a1473) - Ayush Jain
#### Features
- Context API types unification (#619) - (424de2e) - Ayush Jain
- publish rust libraries through crates.io - (8b62d45) - datron
#### Miscellaneous Chores
- Add superposition.sql for initial db setup (#613) - (3a7398e) - Ayush Jain

- - -

## v0.82.1 - 2025-07-31
### Package updates
- experimentation_client bumped to experimentation_client-v0.14.1
- superposition_types bumped to superposition_types-v0.26.1
- context_aware_config bumped to context_aware_config-v0.56.1
- frontend bumped to frontend-v0.36.1
### Global changes
#### Bug Fixes
- Add created_by and created_at column for functions table (#626) - (984c436) - Ayush Jain

- - -

## v0.82.0 - 2025-07-30
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.36.0
- superposition_types bumped to superposition_types-v0.26.0
- frontend bumped to frontend-v0.36.0
- haskell_client bumped to haskell_client-v0.4.0
- experimentation_client bumped to experimentation_client-v0.14.0
### Global changes
#### Bug Fixes
- moved tests to root folder (#617) - (0355ea1) - sauraww
- provider changes for identifier (#616) - (e45b378) - Ankit Kumar Mahato
- bucketing queries (#628) - (a475cdf) - Ankit Kumar Mahato
#### Documentation
- capturing cloudfront settings for documentation website - (49ee4a4) - Natarajan Kannan
#### Features
- experiment bucketing - (274afdf) - Ankit.Mahato
#### Miscellaneous Chores
- fix makefile formatting for readability - (d4eb872) - Natarajan Kannan
- remove superfluous regex strip - (59ca566) - Natarajan Kannan

- - -

## v0.81.0 - 2025-07-28
### Package updates
- superposition_types bumped to superposition_types-v0.25.0
- frontend bumped to frontend-v0.35.0
- context_aware_config bumped to context_aware_config-v0.56.0
- superposition_core bumped to superposition_core-v0.2.0
### Global changes
#### Bug Fixes
- JS builds to include sdk and bindings (#612) - (3d0d458) - Datron
- add juspay scope for typescript sdk package (#621) - (a2bd8e7) - sauraww
- updated js patch file (#622) - (677cfdf) - sauraww
- uncomment tag push in release.yaml - (c79370b) - datron
#### Build system
- use make functions instead of inline commands for wider compat… (#625) - (bb5e30b) - Natarajan Kannan
#### Features
- rust open-feature implementation (#597) - (e34dc64) - PRATIK MISHRA
#### Miscellaneous Chores
- **(deps-dev)** bump axios from 1.10.0 to 1.11.0 (#618) - (219d99a) - dependabot[bot]

- - -

## v0.80.1 - 2025-07-24
### Package updates
- superposition_types bumped to superposition_types-v0.24.1
- frontend bumped to frontend-v0.34.1
- context_aware_config bumped to context_aware_config-v0.55.1
### Global changes
#### Bug Fixes
- code artifact pushes for js (#607) - (9ffccea) - Datron
- support complete bundling of python and JS packages - (be77640) - datron
- Dimension api types unification (#609) - (65e64a6) - Ayush Jain
#### Build system
- fix gradle/python/npm publish (#604) - (4a37e59) - Natarajan Kannan
- fix npm issue (#605) - (edefdbd) - Natarajan Kannan
#### Miscellaneous Chores
- **(deps)** bump on-headers and compression in /docs (#599) - (fb3b530) - dependabot[bot]
- update packages in code-artifact - (b87b524) - datron
- fix npm push - (3f99a23) - datron
#### Refactoring
- removed older test folder (#606) - (a6aa6a3) - sauraww
- remove duplicate python sdk (#615) - (6fb6bc0) - Natarajan Kannan
#### Tests
- Make tests great again (#610) - (6ed1b2b) - Ayush Jain

- - -

## v0.80.0 - 2025-07-18
### Package updates
- frontend bumped to frontend-v0.34.0
### Global changes
#### Bug Fixes
- change base URL for docusaurus - (9b0ad54) - datron
- docusaurus - (74222bf) - ayush.jain@juspay.in
- smithy target copy path fix to javascript (#600) - (b168174) - Ankit Kumar Mahato
#### Features
- Extend A tag usage, wasm download related changes - (f27efd4) - ayush.jain@juspay.in
- push packages to AWS codeArtifact (#603) - (2f4acd6) - Datron
#### Miscellaneous Chores
- move java-legacy under legacy/java (#593) - (943548c) - Natarajan Kannan
#### Refactoring
- move tests folder to sdk, rename js client to javascript-browser (#596) - (2e3f37d) - sauraww

- - -

## v0.79.1 - 2025-07-15
### Package updates
- superposition_core bumped to superposition_core-v0.1.2
### Global changes
#### Bug Fixes
- rename core to superposition_core - (c899522) - Pratik Mishra

- - -

## v0.78.0 - 2025-07-10
### Package updates
- experimentation_client bumped to experimentation_client-v0.13.3
- frontend bumped to frontend-v0.33.0
- experimentation_platform bumped to experimentation_platform-v0.35.1
- context_aware_config bumped to context_aware_config-v0.55.0
- superposition_types bumped to superposition_types-v0.24.0
### Global changes
#### Bug Fixes
- remove warnings from wasm-bindgen-macro (#578) - (de2e5ad) - Datron
- openfeature git support - (ba3719c) - Pratik Mishra
- fix openfeature provider readme (#582) - (fc7fb84) - PRATIK MISHRA
- Revive Function Page UI - (6e77ed5) - ayush.jain@juspay.in
#### Features
- **(java)** Publishing scripts (#574) - (2c64919) - ShreyBana
- **(java-openf)** Experimentation support. (#569) - (34e9319) - ShreyBana
#### Miscellaneous Chores
- **(logs)** enable access log for each actix request (#584) - (5928cb2) - Natarajan Kannan
- update documentation (#577) - (4c3c031) - Datron
- Smithy changes - (b54e468) - ayush.jain@juspay.in

- - -

## v0.77.1 - 2025-07-07
### Package updates
- experimentation_client bumped to experimentation_client-v0.13.2
### Global changes
#### Bug Fixes
- Readme links in md - (1de3e3c) - Kartik
#### Miscellaneous Chores
- Update smithy clients - (fe0e4b0) - ayush.jain@juspay.in
- Add missing audit log for experiment group table - (556df54) - ayush.jain@juspay.in
#### Refactoring
- Refactor and restructure python ofc - (f9de8f5) - PRATIK MISHRA

- - -

## v0.77.0 - 2025-07-03
### Package updates
- frontend bumped to frontend-v0.32.0
- service_utils bumped to service_utils-v0.29.0
- superposition_derives bumped to superposition_derives-v0.3.0
- experimentation_client bumped to experimentation_client-v0.13.1
- superposition_types bumped to superposition_types-v0.23.0
- experimentation_platform bumped to experimentation_platform-v0.35.0
- context_aware_config bumped to context_aware_config-v0.54.0
### Global changes
#### Bug Fixes
- update systems in nix CI action (#550) - (d3deff6) - Datron
- added validation for and wrapper over context json logic - (a48cc71) - Shubhranshu Sanjeev
- experiment client fetch fix (#555) - (cb08467) - PRATIK MISHRA
- smithy model types (#554) - (6ef6d1e) - Datron
- python update - (20a2622) - Pratik Mishra
- Default Config page UX issues - (939505e) - ayush.jain@juspay.in
- Frontend formatting - (92a5835) - ayush.jain@juspay.in
- Change is operator to == in strict mode - (d09baf5) - ayush.jain@juspay.in
- Show experiment filters - (a6a0e7e) - ayush.jain@juspay.in
#### Features
- Workspace setting for allowing experiment self approval (#552) - (472f03e) - Ayush Jain
- openfeature python client (#546) - (bc4dba3) - PRATIK MISHRA
- experiment group integration (#526) - (73d0950) - Ankit Kumar Mahato
- Typescript OpenFeature + crate/core (#522) - (6a7f893) - sauraww
- Rust Version Bump + Uniffi + Java OpenFeature SDK (#553) - (1aba4b4) - ShreyBana
- added a CRUD for experiment groups (#540) - (6eedef2) - Datron
- added experiment openfeature support with uniffi (#561) - (dfad18c) - PRATIK MISHRA
- Experimentation FFI. - (d94f79a) - Shrey Bana
- python openfeature exp support - (d9b3d3d) - Pratik Mishra
- add exp support - (b5b7edb) - Pratik Mishra
#### Miscellaneous Chores
- **(deps)** bump self_cell from 1.0.1 to 1.0.4 - (0c3b29c) - dependabot[bot]
- **(deps)** bump mio from 0.8.6 to 0.8.11 - (a3afe8f) - dependabot[bot]
- Update Smithy for metrics changes - (4b3204b) - ayush.jain@juspay.in
- introduce docusaurus - (ff9b76d) - Kartik
- update docker build - (78888a5) - Kartik
- build and release core binaries - (6a79905) - Kartik
- update macos-13 builds for x86 - (3a83502) - Kartik

- - -

## v0.76.0 - 2025-06-23
### Package updates
- frontend bumped to frontend-v0.31.0
- superposition_types bumped to superposition_types-v0.22.0
- context_aware_config bumped to context_aware_config-v0.53.0
- experimentation_platform bumped to experimentation_platform-v0.34.0
### Global changes
#### Bug Fixes
- stack overflow in applicable variants and updated versions for the smithy client (#538) - (477e402) - George James
- Removed copy of superposition.cac.toml, since the file was removed in an earlier revision (#541) - (a560181) - George James
#### Features
- add support to link autocomplete functions to dimensions - (ae6bbaa) - Kartik
- add support for predicting inputs in forms - (fc4e63c) - Kartik
- Get dimension by name (#524) - (bb7cbd4) - Ayush Jain
- set config version (#512) - (bc662bd) - PRATIK MISHRA
- add experiment groups API - (c09cef1) - Kartik
- Superposition hs SDK. - (6d11228) - Shrey Bana
#### Miscellaneous Chores
- **(deps)** bump flask-cors from 5.0.0 to 6.0.0 in /locust - (6f941ee) - dependabot[bot]
- **(deps)** bump requests from 2.32.3 to 2.32.4 in /locust - (d468ef1) - dependabot[bot]
- **(deps)** bump urllib3 from 1.26.19 to 2.5.0 in /locust - (f32c9c4) - dependabot[bot]
- Remove dead code (#536) - (621b516) - Ayush Jain
- Rename auth to auth_n (#542) - (fbf4e1a) - Ayush Jain
- add cline memory-bank & rules (#520) - (8f925a7) - Naman Agarwal

- - -

## v0.75.0 - 2025-05-22
### Package updates
- frontend bumped to frontend-v0.30.0
- superposition_types bumped to superposition_types-v0.21.0
- experimentation_platform bumped to experimentation_platform-v0.33.0
- context_aware_config bumped to context_aware_config-v0.52.1
### Global changes
#### Bug Fixes
- removed explicit err handling for FKValidation & removed redundant crate - (0d1d481) - Solomon
#### Features
- introduce experiment groups data model - (10815da) - Kartik

- - -

## v0.74.0 - 2025-05-16
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.32.0
- context_aware_config bumped to context_aware_config-v0.52.0
- frontend bumped to frontend-v0.29.0
- superposition_types bumped to superposition_types-v0.20.0
### Global changes
#### Features
- add pause experiment (#509) - (36defc9) - Ankit Kumar Mahato
- delete overrides by experiment (#500) - (51a20e3) - PRATIK MISHRA

- - -

## v0.73.0 - 2025-05-12
### Package updates
- frontend bumped to frontend-v0.28.0
- experimentation_platform bumped to experimentation_platform-v0.31.0
- superposition_types bumped to superposition_types-v0.19.0
- service_utils bumped to service_utils-v0.28.1
### Global changes
#### Bug Fixes
- Postman test (#505) - (e48ff97) - Ayush Jain
- custom headers parsing for webhook (#506) - (04133a2) - Ankit Kumar Mahato
#### Features
- Metrics initial setup - (530586d) - ayush.jain@juspay.in

- - -

## v0.72.0 - 2025-05-09
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.30.0
- frontend bumped to frontend-v0.27.0
- superposition_macros bumped to superposition_macros-v0.3.1
- superposition_types bumped to superposition_types-v0.18.0
- service_utils bumped to service_utils-v0.28.0
- context_aware_config bumped to context_aware_config-v0.51.0
### Global changes
#### Bug Fixes
- Initial values during workspace creation (#501) - (92d37ef) - Ayush Jain
- webhook implementation (#503) - (1ffd6eb) - Ankit Kumar Mahato
#### Features
- add support to link autocomplete functions to dimensions - (2d5540c) - Kartik
- webhook cruds (#313) - (292689e) - Ankit Kumar Mahato
- add strict mode support for workspaces (#470) - (63e17f8) - Datron
- added smithy clients (#459) - (0488d8b) - PRATIK MISHRA
- Update override using context id - (6c2543d) - Ayush Jain
- introduce compare UI (#495) - (dff7e2c) - Datron
#### Miscellaneous Chores
- improve builds - (660adee) - Kartik
- set a common cache key - (d14953e) - Kartik

- - -

## v0.71.1 - 1970-01-01
### Package updates
- superposition_derives bumped to superposition_derives-v0.2.1
- frontend bumped to frontend-v0.26.1
- superposition_types bumped to superposition_types-v0.17.1
### Global changes

- - -

## v0.71.0 - 2025-04-24
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.29.0
- frontend bumped to frontend-v0.26.0
- superposition_types bumped to superposition_types-v0.17.0
- superposition_macros bumped to superposition_macros-v0.3.0
- context_aware_config bumped to context_aware_config-v0.50.0
### Global changes
#### Bug Fixes
- db_init.sql - (89de57c) - ayush.jain@juspay.in
- corrected sql constraint (#482) - (8052357) - PRATIK MISHRA
- Update experiment restrictions and default value of sort_by (#485) - (f7542f4) - Ayush Jain
#### Features
- Paginate Context overrides page (#457) - (72a74b1) - Ayush Jain
#### Miscellaneous Chores
- **(deps)** bump crossbeam-channel from 0.5.13 to 0.5.15 - (63e7f74) - dependabot[bot]
#### Tests
- added cleanup step for tests - (612c0ac) - Shubhranshu Sanjeev
- Updated dimension test suite. - (44a7023) - Shrey Bana
- added pre-test setup - (25bc2ab) - Shubhranshu Sanjeev
- added client test's to CI-test flow - (d0fa421) - Shubhranshu Sanjeev
- Experiment - (b5a8120) - ayush.jain@juspay.in
- Update experiments test cleanup (#478) - (91f8523) - Ayush Jain

- - -

## v0.70.0 - 2025-04-17
### Package updates
- superposition_types bumped to superposition_types-v0.16.0
- experimentation_client bumped to experimentation_client-v0.13.0
- context_aware_config bumped to context_aware_config-v0.49.0
- superposition_derives bumped to superposition_derives-v0.2.0
- haskell_client bumped to haskell_client-v0.3.0
- frontend bumped to frontend-v0.25.0
- experimentation_platform bumped to experimentation_platform-v0.28.0
- service_utils bumped to service_utils-v0.27.0
### Global changes
#### Bug Fixes
- workspace_template.sql and db-init.sql - (cc9ed84) - Ayush Jain
- added tests for contexts (along with bug-fixes) - (1f53762) - Shubhranshu Sanjeev
#### Features
- dependent dimensions - (5a89f5b) - Ankit Kumar Mahato
- Add types for experiment apis in superposition_types - (219a2eb) - Ayush Jain
- smithy models - (2958cce) - PRATIK MISHRA
#### Miscellaneous Chores
- **(deps)** bump openssl from 0.10.70 to 0.10.72 - (5960d2d) - dependabot[bot]
#### Refactoring
- Refactored organisation smtihy model w/ mixin for requests. (#455) - (fb37a62) - ShreyBana
#### Tests
- Scaffolding for ts client tests. - (4b6b29a) - Shrey Bana
- tests for the org module in ts sdk - (1981658) - Kartik
- Config & Dimension happy test cases w/ ts client. - (c0f6e48) - Shrey Bana
- workspace and type template - (f9ccd41) - sauraww
- added default-config endpoint tests - (5dc457d) - Shubhranshu Sanjeev
- add function tests for ts sdk - (afd27bc) - Kartik

- - -

## v0.69.0 - 2025-04-03
### Package updates
- frontend bumped to frontend-v0.24.0
- context_aware_config bumped to context_aware_config-v0.48.0
- superposition_types bumped to superposition_types-v0.15.0
### Global changes
#### Bug Fixes
- config version alter query - (a4ddf7c) - Kartik
#### Features
- add autocomplete functions - (1f39003) - Kartik

- - -

## v0.68.0 - 2025-03-26
### Package updates
- experimentation_client bumped to experimentation_client-v0.12.0
- experimentation_platform bumped to experimentation_platform-v0.27.0
- superposition_types bumped to superposition_types-v0.14.0
- frontend bumped to frontend-v0.23.0
- cac_client bumped to cac_client-v0.17.0
### Global changes
#### Features
- experiment filters in UI - (d4604e4) - Kartik
- generate multiple binaries for clients based on platform (#445) - (cd6d30b) - Datron
#### Miscellaneous Chores
- **(deps)** bump jinja2 from 3.1.5 to 3.1.6 in /locust - (bc31cb6) - dependabot[bot]
- **(deps-dev)** bump axios from 1.7.4 to 1.8.2 - (a326d64) - dependabot[bot]
- **(docs)** Correct flake inputs - (334e955) - Sridhar Ratnakumar

- - -

## v0.67.0 - 2025-03-10
### Package updates
- superposition_types bumped to superposition_types-v0.13.0
- context_aware_config bumped to context_aware_config-v0.47.0
- frontend bumped to frontend-v0.22.0
### Global changes
#### Features
- Added support for completions in monaco editor. - (b840133) - Shrey Bana

- - -

## v0.66.2 - 2025-02-28
### Package updates
- frontend bumped to frontend-v0.21.1
- context_aware_config bumped to context_aware_config-v0.46.2
- experimentation_platform bumped to experimentation_platform-v0.26.2
- cac_client bumped to cac_client-v0.16.2
- service_utils bumped to service_utils-v0.26.3
- superposition_types bumped to superposition_types-v0.12.2
### Global changes
#### Bug Fixes
- Fixed local db init. - (6ff2518) - Shrey Bana
- Fixed clippy warnings - (88bbfe9) - ShreyBana
- optimize db query for update (#390) - (258933f) - PRATIK MISHRA

- - -

## v0.66.1 - 2025-02-19
### Package updates
- superposition_types bumped to superposition_types-v0.12.1
- experimentation_platform bumped to experimentation_platform-v0.26.1
- service_utils bumped to service_utils-v0.26.2
### Global changes
#### Bug Fixes
- tenant config - (416f066) - Ankit Kumar Mahato
#### Miscellaneous Chores
- **(deps)** bump openssl from 0.10.54 to 0.10.70 - (f9d536d) - dependabot[bot]

- - -

## v0.66.0 - 2025-02-14
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.26.0
- superposition_types bumped to superposition_types-v0.12.0
- experimentation_client bumped to experimentation_client-v0.11.0
- context_aware_config bumped to context_aware_config-v0.46.1
- frontend bumped to frontend-v0.21.0
- service_utils bumped to service_utils-v0.26.1
- js_client bumped to js_client-v0.3.0
### Global changes
#### Bug Fixes
- using schema_name in redis key - (18dfc5d) - Kartik
- workspace dropdown and UI misalignment (#408) - (82dde7f) - Datron
- update rust-monaco version (#411) - (47c0fc6) - Datron
- nix builds (#418) - (d0bb78a) - Datron
#### Features
- Discard experiment - (42ac967) - ayush.jain@juspay.in
#### Miscellaneous Chores
- **(deps)** bump jinja2 from 3.1.4 to 3.1.5 in /locust - (2b1aa66) - dependabot[bot]
- **(deps)** bump cryptography from 43.0.1 to 44.0.1 in /locust - (ce5dad8) - dependabot[bot]
- Pin cargo-edit and cocogitto in github action - (9595662) - ayush.jain@juspay.in

- - -

## v0.65.0 - 2025-01-23
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.25.0
- superposition_types bumped to superposition_types-v0.11.0
- context_aware_config bumped to context_aware_config-v0.46.0
- frontend bumped to frontend-v0.20.0
- service_utils bumped to service_utils-v0.26.0
- experimentation_client bumped to experimentation_client-v0.10.0
### Global changes
#### Bug Fixes
- Redirect to workspaces after selecting org_id in local - (1ed5a3f) - ayush.jain@juspay.in
- read from workspaces table when showing the dropdown - (1594218) - Kartik
- org_id in tests - (9484a9c) - Pratik Mishra
- add org_id in tests headers - (4ca46bc) - Pratik Mishra
- add org tests - (deab731) - Pratik Mishra
- experiment handlers to send org_id - (571f201) - Kartik
- get types test failure - (d72ae46) - Shubhranshu Sanjeev
- workspace template SQL and workspace update handler - (fb68d3b) - Kartik
- revert experiment list pagination bug - (df94849) - Kartik
- resolved comments - (09ea160) - Shubhranshu Sanjeev
- Fixed update w/ accurate verbage. - (82e7466) - Shrey Bana
- Resolving general pending issues (#375) - (d3e2e34) - Ayush Jain
#### Features
- added update api for default config (#310) - (e6ca4de) - sauraww
- Use Common db model types in frontend (#291) - (e68782d) - Ayush Jain
- Add auth via OAUTH2 (#321) - (f5092f8) - Ayush Jain
- added schema and crl apis for organisation (#322) - (c333f3a) - sauraww
- added description and comment (#284) - (4e0006c) - sauraww
- added workspaces table and workspace management APIs - (5910f3e) - Kartik
- workspace-ui - (4ccd9c6) - Ankit.Mahato
- Redirect to workspaces after selecting org_id - (2cafa61) - ayush.jain@juspay.in
- auth path prefix support and org_user authentication - (919de10) - ayush.jain@juspay.in
#### Miscellaneous Chores
- formatting - (c3a1ca1) - Shubhranshu Sanjeev
#### Refactoring
- merge cac and experimentation schemas - (51367a6) - Kartik
- using juspay diesel fork - (c306404) - Shubhranshu Sanjeev
- added schema_name dsl to cac queries - (73d6ba2) - Shubhranshu Sanjeev
- Refactored make & related files. (#339) - (9153a46) - ShreyBana
- workspace ui & form (#373) - (9dca3aa) - Shubhranshu Sanjeev
- OrgId, WorkspaceId, SchemaName cleanup and refactor (#379) - (470ab48) - Ayush Jain

- - -

## v0.64.1 - 1970-01-01
### Package updates
- frontend bumped to frontend-v0.19.1
- haskell_client bumped to haskell_client-v0.2.1
### Global changes

- - -

## v0.64.0 - 2025-01-06
### Package updates
- context_aware_config bumped to context_aware_config-v0.45.0
- superposition_types bumped to superposition_types-v0.10.0
- frontend bumped to frontend-v0.19.0
- service_utils bumped to service_utils-v0.25.0
- experimentation_platform bumped to experimentation_platform-v0.24.0
### Global changes
#### Bug Fixes
- webhook kms decrypt auth key (#332) - (09ebc29) - Ankit Kumar Mahato
#### Features
- position migration update for demo up (#300) - (d2f56db) - PRATIK MISHRA
- search and sort experiments - (95b87c5) - Datron
- Replace priority with position (#299) - (61e052a) - PRATIK MISHRA

- - -

## v0.63.0 - 2024-12-04
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.23.1
- context_aware_config bumped to context_aware_config-v0.44.0
- superposition_types bumped to superposition_types-v0.9.0
- service_utils bumped to service_utils-v0.24.2
- cac_client bumped to cac_client-v0.16.1
### Global changes
#### Features
- priority recompute restructure (#279) - (849063d) - PRATIK MISHRA

- - -

## v0.62.0 - 2024-12-02
### Package updates
- context_aware_config bumped to context_aware_config-v0.43.0
- experimentation_platform bumped to experimentation_platform-v0.23.0
- superposition_types bumped to superposition_types-v0.8.0
### Global changes
#### Features
- Cac model types migration - (b15f1b7) - ayush.jain@juspay.in
- Exp model types migration - (f0dfe8c) - ayush.jain@juspay.in
- Common changes - (9c68567) - ayush.jain@juspay.in
- Renaming - (e987e40) - ayush.jain@juspay.in
- Revert seperate crate creation - (a6e905d) - ayush.jain@juspay.in

- - -

## v0.61.1 - 2024-11-22
### Package updates
- frontend bumped to frontend-v0.18.1
- service_utils bumped to service_utils-v0.24.1
- superposition_types bumped to superposition_types-v0.7.1
- experimentation_platform bumped to experimentation_platform-v0.22.1
### Global changes
#### Bug Fixes
- fixed and improved webhooks (#283) - (4864849) - Ankit Kumar Mahato

- - -

## v0.61.0 - 2024-11-20
### Package updates
- superposition_types bumped to superposition_types-v0.7.0
- frontend bumped to frontend-v0.18.0
- superposition_macros bumped to superposition_macros-v0.2.0
- cac_client bumped to cac_client-v0.16.0
- experimentation_platform bumped to experimentation_platform-v0.22.0
- haskell_client bumped to haskell_client-v0.2.0
- context_aware_config bumped to context_aware_config-v0.42.0
- service_utils bumped to service_utils-v0.24.0
### Global changes
#### Bug Fixes
- Add pagination to list APIs (#209) - (c155bb0) - Ankit Kumar Mahato
#### Features
- add cache in cac client (#268) - (0cd82dd) - PRATIK MISHRA
- Webhook trigger for experiments (#265) - (585ee1e) - Ankit Kumar Mahato
- use new input type for json schema inputs as well - (e7075be) - Kartik
#### Miscellaneous Chores
- **(deps)** bump werkzeug from 3.0.4 to 3.0.6 in /locust - (fb2e22d) - dependabot[bot]

- - -

## v0.60.0 - 2024-10-23
### Package updates
- context_aware_config bumped to context_aware_config-v0.41.0
- experimentation_client bumped to experimentation_client-v0.9.0
- service_utils bumped to service_utils-v0.23.0
- cac_client bumped to cac_client-v0.15.0
- frontend bumped to frontend-v0.17.0
- experimentation_platform bumped to experimentation_platform-v0.21.0
- superposition_types bumped to superposition_types-v0.6.0
### Global changes
#### Bug Fixes
- locust files - (2a1e4fe) - Kartik
#### Features
- add high performance mode with redis - (adc2712) - Kartik
- Search contexts by dimension values (#264) - (12743af) - Ayush Jain
#### Miscellaneous Chores
- **(deps)** bump cryptography from 41.0.7 to 43.0.1 in /locust - (6146647) - dependabot[bot]
- **(deps)** bump urllib3 from 1.26.18 to 1.26.19 in /locust - (24e9a9e) - dependabot[bot]

- - -

## v0.59.0 - 2024-10-17
### Package updates
- frontend bumped to frontend-v0.16.0
- superposition_derives bumped to superposition_derives-v0.1.0
- experimentation_platform bumped to experimentation_platform-v0.20.0
### Global changes
#### Features
- use concrete Variant type in db model (#241) - (8ca8135) - Ayush Jain

- - -

## v0.58.0 - 1970-01-01
### Package updates
- context_aware_config bumped to context_aware_config-v0.40.0
- superposition_types bumped to superposition_types-v0.5.0
### Global changes

- - -

## v0.57.0 - 2024-09-30
### Package updates
- cac_client bumped to cac_client-v0.14.3
- superposition_types bumped to superposition_types-v0.4.0
- cac_toml bumped to cac_toml-v0.2.0
- experimentation_platform bumped to experimentation_platform-v0.19.0
- superposition_macros bumped to superposition_macros-v0.1.1
- context_aware_config bumped to context_aware_config-v0.39.0
- frontend bumped to frontend-v0.15.2
- experimentation_client bumped to experimentation_client-v0.8.2
- service_utils bumped to service_utils-v0.22.0
### Global changes
#### Bug Fixes
- Frontend related issues and warnings (#243) - (1a54fd2) - Ayush Jain
- Dependency pruning (#250) - (8b68900) - Ayush Jain
#### Documentation
- use permanent discord invite link (#235) - (a60110c) - Natarajan Kannan
#### Features
- Add get_applicable_variants as expt endpoint (#210) - (54f2037) - Ayush Jain
- Tenant specific config support via .cac.toml (#246) - (ffc247e) - Ayush Jain
#### Miscellaneous Chores
- add github container registry as a target - (b7804a2) - Kartik
- Add tailwind extension settings to workspace settings for vscode (#240) - (565f822) - Ayush Jain

- - -

## v0.56.1 - 2024-09-09
### Package updates
- frontend bumped to frontend-v0.15.1
### Global changes
#### Bug Fixes
- update docker image binary name - (cdd59b2) - Kartik
- re-enable ARM builds - (ddc17ad) - Kartik
- faster docker builds - (a9afdb9) - Kartik
#### Documentation
- fix discord link - (5baad41) - Natarajan Kannan
#### Miscellaneous Chores
- turn off ARM docker builds - (37306e7) - Kartik

- - -

## v0.56.0 - 2024-09-06
### Package updates
- context_aware_config bumped to context_aware_config-v0.38.0
- cac_client bumped to cac_client-v0.14.2
- experimentation_client bumped to experimentation_client-v0.8.1
- frontend bumped to frontend-v0.15.0
- cac_toml bumped to cac_toml-v0.1.0
### Global changes
#### Bug Fixes
- update go clients - (3497b0e) - Kartik
- optimised wasm file size (#222) - (a044706) - sauraww
- python client (#219) - (31a9887) - PRATIK MISHRA
- example dockerfile - (533a277) - Kartik
- add setup.py fle and doc update - (87f8e34) - Pratik Mishra
- removed edit option for experiment contexts (#229) - (a9b01a6) - Shubhranshu Sanjeev
#### Continuous Integration
- **(nix)** Run Haskell clients in CI (#191) - (5eeb498) - Shivaraj B H
#### Documentation
- update docs with correct icons and include logo (#228) - (cb80c7d) - Natarajan Kannan
#### Features
- [Client] Java client (#198) - (290227f) - Ankit Kumar Mahato
- adding go cac and experimentation client wrapper - (ff2e240) - namit goel
- adding go cac and experimentation client wrapper - (9198e02) - namit goel
- delete dimension api and ui (#213) - (01cdc18) - PRATIK MISHRA
- Build CAC Lang using a formal expression parser (#220) - (88a24a5) - Natarajan Kannan
- JsonSchema generated forms (#221) - (3c02248) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- **(action)** split up github docker actions to build faster - (be2abe3) - Kartik
- **(deps-dev)** bump axios from 1.7.2 to 1.7.4 - (a0e6a8d) - dependabot[bot]
- **(nix)** Switch to omnix (#206) - (d142ce5) - Sridhar Ratnakumar
- add example app and release scripts - (41073fc) - Kartik
- add example app and release scripts - (c6201fa) - Kartik
- Rename example.DockerFile to example.Dockerfile - (a92e530) - Datron
- Rename DockerFile to Dockerfile - (e205e71) - Datron

- - -

## v0.55.0 - 2024-08-14
### Package updates
- context_aware_config bumped to context_aware_config-v0.37.0
- service_utils bumped to service_utils-v0.21.0
- superposition_types bumped to superposition_types-v0.3.2
- frontend bumped to frontend-v0.14.0
### Global changes
#### Bug Fixes
- newtypes for entity fields (#199) - (49562b7) - PRATIK MISHRA
- changing aws kms library to aws-sdk-kms to rusto (#203) - (a455f93) - namit goel
#### Features
- adding python cac and experimentation client wrapper - (46b7342) - namit goel
- Mandatory dimensions feature (#173) - (8d95a30) - Ankit Kumar Mahato

- - -

## v0.54.0 - 2024-08-08
### Package updates
- service_utils bumped to service_utils-v0.20.0
- experimentation_platform bumped to experimentation_platform-v0.18.1
- frontend bumped to frontend-v0.13.0
- cac_client bumped to cac_client-v0.14.1
- haskell_client bumped to haskell_client-v0.1.3
- superposition_types bumped to superposition_types-v0.3.1
- context_aware_config bumped to context_aware_config-v0.36.1
### Global changes
#### Bug Fixes
- **(cac_client)** Run `fixDarwinDylibNames` during `postInstall` - (bf33e15) - shivaraj-bh
- api validation with new types (#146) - (66ad741) - PRATIK MISHRA
- add last modified to all tables (#192) - (d41195c) - PRATIK MISHRA
- haskell-client-doc - (291df89) - Ankit Mahato
- jsonschema for dimension and remove default_config's jsonsschema check (#197) - (89a23af) - PRATIK MISHRA
#### Features
- monaco as a component (#184) - (5233f1a) - Datron

- - -

## v0.53.0 - 2024-07-17
### Package updates
- context_aware_config bumped to context_aware_config-v0.36.0
- js_client bumped to js_client-v0.2.1
- frontend bumped to frontend-v0.12.1
### Global changes
#### Bug Fixes
- update axios version for function (#158) - (c52def4) - PRATIK MISHRA
- rename experiment test folders in postman to work on linux machines - (6526fa2) - Kartik
#### Miscellaneous Chores
- **(nix)** Build *all* binary crates - (502abdf) - Sridhar Ratnakumar

- - -

## v0.52.0 - 2024-07-11
### Package updates
- superposition_macros bumped to superposition_macros-v0.1.0
### Global changes
#### Bug Fixes
- add superposition_macros in cog.toml to upgrade versions (#155) - (1644bf8) - PRATIK MISHRA

- - -

## v0.51.0 - 2024-07-11
### Package updates
- superposition_types bumped to superposition_types-v0.3.0
- frontend bumped to frontend-v0.12.0
- experimentation_platform bumped to experimentation_platform-v0.18.0
- context_aware_config bumped to context_aware_config-v0.35.0
- service_utils bumped to service_utils-v0.19.0
- cac_client bumped to cac_client-v0.14.0
### Global changes
#### Documentation
- updated nix package addition docs (#147) - (3bdc59d) - Shivam Ashtikar
#### Features
- move apperror to superposition_types - (f1c8395) - Pratik Mishra
#### Miscellaneous Chores
- add more docs on integration with C libraries (#153) - (7d32720) - Datron

- - -

## v0.50.1 - 1970-01-01
### Package updates
- context_aware_config bumped to context_aware_config-v0.34.3
- frontend bumped to frontend-v0.11.3
### Global changes

- - -

## v0.50.0 - 2024-06-27
### Package updates
- cac_client bumped to cac_client-v0.13.0
- experimentation_client bumped to experimentation_client-v0.8.0
- frontend bumped to frontend-v0.11.2
- context_aware_config bumped to context_aware_config-v0.34.2
### Global changes
#### Features
- Rework async library in CAC client (#88) - (5610b93) - Datron

- - -

## v0.49.1 - 2024-06-20
### Package updates
- experimentation_client bumped to experimentation_client-v0.7.2
- context_aware_config bumped to context_aware_config-v0.34.1
- cac_client bumped to cac_client-v0.12.2
- haskell_client bumped to haskell_client-v0.1.2
### Global changes
#### Bug Fixes
- Use jsonlogc's partial_apply for config and experiment filtering (#127) - (97bf39b) - Ayush Jain

- - -

## v0.49.0 - 2024-06-20
### Package updates
- service_utils bumped to service_utils-v0.18.0
- experimentation_platform bumped to experimentation_platform-v0.17.0
- context_aware_config bumped to context_aware_config-v0.34.0
- frontend bumped to frontend-v0.11.1
### Global changes
#### Bug Fixes
- updated instruction for local setup in mac OS (#76) - (95205ae) - Subhash Chandra
#### Features
- add config version header in api response (#87) - (213a21e) - PRATIK MISHRA

- - -

## v0.48.0 - 2024-06-11
### Package updates
- experimentation_platform bumped to experimentation_platform-v0.16.0
- context_aware_config bumped to context_aware_config-v0.33.0
- frontend bumped to frontend-v0.11.0
- service_utils bumped to service_utils-v0.17.0
### Global changes
#### Bug Fixes
- unify actions column for default_config and custom_types - (bdcc902) - Kartik
#### Features
- snapshot changes - apis - (69588f6) - Pratik Mishra
- add custom types API endpoints - (c63192d) - Kartik
- add type templates UI - (c8f2e3a) - Kartik
- integrate type templates in forms - (3954d21) - Kartik
#### Miscellaneous Chores
- simplify module structure and follow Rust 2018 edition module naming convention (#86) - (403ea59) - Sanchith Hegde
- address some clippy lints (#85) - (001b8d4) - Sanchith Hegde

- - -

## v0.47.0 - 2024-05-31
### Package updates
- context_aware_config bumped to context_aware_config-v0.32.0
- haskell_client bumped to haskell_client-v0.1.1
- frontend bumped to frontend-v0.10.0
- experimentation_client bumped to experimentation_client-v0.7.1
- cac_client bumped to cac_client-v0.12.1
- caclang bumped to caclang-v0.2.1
### Global changes
#### Bug Fixes
- Add prefix to client's methods (#84) - (ee2c54f) - Ankit Kumar Mahato
- stricter validation for key names in default config (#79) - (78a80c0) - Natarajan Kannan
#### Features
- Add local storage support (#78) - (9eee7f0) - Ankit Kumar Mahato

- - -

## v0.46.0 - 2024-05-29
### Package updates
- service_utils bumped to service_utils-v0.16.1
- experimentation_platform bumped to experimentation_platform-v0.15.1
- experimentation_client bumped to experimentation_client-v0.7.0
- js_client bumped to js_client-v0.2.0
- frontend bumped to frontend-v0.9.0
- context_aware_config bumped to context_aware_config-v0.31.0
### Global changes
#### Bug Fixes
- creating experiments for default-config (no context) (#38) - (4f6b92c) - Sauravcv98
- disallow trailing . in key name (#77) - (57807d6) - Natarajan Kannan
- Formatting and ECLINT issue (#80) - (1c34dfa) - Ayush Jain
- Temporarily disable eclint (#81) - (f649fef) - Ayush Jain
#### Features
- Add Juspay version and regex operations in jsonlogic (#73) - (eb14280) - Ayush Jain
#### Miscellaneous Chores
- Add CI check to lint the .sql files based on rules defined in .editorconfig - (16bf460) - Hao

- - -

## v0.44.0 - 2024-05-10
### Package updates
- frontend bumped to frontend-v0.8.1
- superposition_types bumped to superposition_types-v0.2.1
- context_aware_config bumped to context_aware_config-v0.30.0
### Global changes
#### Bug Fixes
- remove juspay email address for anon user - (f174428) - Kartik
- for the first page redirect to default config page - (361054b) - ankit.mahato
#### Documentation
- update README/experimentation - (4e0ed8f) - Natarajan Kannan
#### Features
- delete default config api (#34) - (d5c001c) - PRATIK MISHRA

- - -

## v0.43.0 - 2024-05-06
### Package updates
- js_client bumped to js_client-v0.1.0
- haskell_client bumped to haskell_client-v0.1.0
### Global changes
#### Features
- Add release tag generation for js and haskell client (#31) - (c8aa534) - Ayush Jain

- - -

## v0.42.0 - 2024-05-06
### Package updates
- superposition_types bumped to superposition_types-v0.2.0
- caclang bumped to caclang-v0.2.0
- experimentation_platform bumped to experimentation_platform-v0.15.0
- frontend bumped to frontend-v0.8.0
- cac_client bumped to cac_client-v0.12.0
- service_utils bumped to service_utils-v0.16.0
- context_aware_config bumped to context_aware_config-v0.29.0
- experimentation_client bumped to experimentation_client-v0.6.0
### Global changes
#### Bug Fixes
- corrected env for DB_PASSWORD and default for AWS_REGION - (d840100) - Ritick Madaan
- improved log for env not found - (19aaea5) - Ritick Madaan
- base64 decoding kms cypher - (a9a5516) - Ritick Madaan
- changed host to 0.0.0.0 - (9c76f91) - Ritick Madaan
-  dimension Table scaffolding, PUT<>DELETE apis `/dimension` - (a11d927) - Ritick Madaan
- storing pre calculated priority in contexts table - (e89389f) - Ritick Madaan
- removed delete dimension api - (224500f) - Ritick Madaan
-  DB password URI encoded. - (b3d2b5c) - Shrey Bana
-  moved override inside contexts table - (2b92946) - Ritick Madaan
-  removed unecessary and wrap over conditions - (71d4acb) - Ritick Madaan
-  added search path for schema in database_url - (8636c94) - Ritick Madaan
-  database schema url - (a5ebc02) - Ritick Madaan
-  added moved tables to cac_v1 schema - (edd84dd) - Ritick Madaan
-  enabled override updates in PUT /context by deep_merge - (10920fb) - Ritick Madaan
-  moved dimension_type to cac_v1 schema - (fb16bde) - Ritick Madaan
-  fixed ordering of /context endpoints - (857e1b7) - Ritick Madaan
-  updated response-type of /context/bulk-operations api - (8ff3bb3) - Ritick Madaan
- fixed context overlap check logic - (b7f81fe) - Shubhranshu Sanjeev
- added last_modified column and indexes - (8ba6cd6) - Shubhranshu Sanjeev
- moved tables and types under cac_v1 schema - (6e31830) - Shubhranshu Sanjeev
- calling cac apis for creating context - (97e8dcd) - Shubhranshu Sanjeev
-  updated last_modified in ramp - (19c9630) - ankit.mahato
-  minor fixes for exp client - (0494a96) - Kartik Gajendra
- removed traffic-percentage from experiment create request - (cae8b71) - Shubhranshu Sanjeev
-  added total items to list API response - (70ce4ff) - Kartik Gajendra
- using audit log tstamp for checking last-modified - (664a2aa) - Shubhranshu Sanjeev
-  sorting same priority contexts with created_at - (fc3344a) - Ritick Madaan
-  allowing cug users to fall under test variants - (8158524) - Ritick Madaan
-  removed unwanted parameter to prevent warning - (872dcc6) - Ritick Madaan
- added middleware to insert version in response headers - (3bdfac0) - Shubhranshu Sanjeev
-  logged env variable's value before kms decrypting - (1224359) - Ritick Madaan
- moved git init to separate stage - (2b0133a) - Shubhranshu Sanjeev
-  cleaned up Dockerfile - (abb7f9a) - Ritick Madaan
- fixed setting env in docker image - (be938dc) - Shubhranshu Sanjeev
- trimming newline character from version string - (374efb2) - Shubhranshu Sanjeev
- fixed random timeouts in internal http calls to CAC - (f80862d) - Shubhranshu Sanjeev
- failed build due to untracked schema.rs file changes - (b9e19b6) - Shubhranshu Sanjeev
-  eval param fix - (5963208) - Pratik Mishra
-  patching overrides on default-config instead of merge - (ba461ca) - Ritick Madaan
- ssh.bitbucket.juspay.net added to known hosts in docker bulid - (3a3dbd9) - Ritick Madaan
-  add user struct in delete context API - (698cc2c) - Kartik Gajendra
-  add migration for changing default_configs_keys - (1ed3cff) - Pratik Mishra
- validating override_keys for unique entries - (f759d38) - Shubhranshu Sanjeev
-  add all variants in manifest - (ca15d2b) - Pratik Mishra
- fixed failing health check (x-tenant header not set) - (850e7b2) - Shubhranshu Sanjeev
-  x-tenant header mandate removed for OPTIONS calls - (2254ad5) - Ritick Madaan
-  x-tenant header added for /config/resolve call in diff - (6b9516e) - Ritick Madaan
-  added external crate to cocogitto config - (23d34d6) - Ritick Madaan
- make sure envs with defaults prevent failure - (4f4cc82) - Kartik Gajendra
- cac service to set last_modified header - (6c7d792) - ankit.mahato
- Removing acceptance of override_keys in experiment create/update - (336b771) - ankit.mahato
- failing build due to update of schema.rs file - (f219dbf) - Shubhranshu Sanjeev
- add different auth types for exp requests to CAC - (a988be3) - Kartik Gajendra
- fixed deployment ConfigNotFound failure - (d0ba5f0) - Shubhranshu Sanjeev
-  sort json while context creation - (84e2693) - Pratik Mishra
- allow ramp 0 - (6666f3e) - Kartik Gajendra
-  - Cac client library changes to consume backend api response - (ccb40cd) - Prasanna P
-  fix json schema validation - (48017ea) - ankit.mahato
-  array validation for in condition - (f6b9d2e) - Pratik Mishra
- context parsing - (743fdcd) - Kartik Gajendra
- fixed experiment list page feedback - (4fa25e6) - Shubhranshu Sanjeev
- frontend multi-tenancy support + config and dimension page - (a632054) - Shubhranshu Sanjeev
- minor docs update - (0e3122a) - Kartik Gajendra
- fixed ci-test to support multi-tenant setup - (9ad6aa5) - Shubhranshu Sanjeev
- fixed build failure due to rust-version - (b6b0332) - Shubhranshu Sanjeev
- frontend build process - (3036d77) - Shubhranshu Sanjeev
- error resolving pages with internal call to server - (61c3909) - Shubhranshu Sanjeev
- fixed host resolve issue for internal calls in SSR. - (d2189f6) - Shubhranshu Sanjeev
- added partitions for 2025 and 2026 for audit table - (a3f75f1) - Shubhranshu Sanjeev
- getting api hostname from env for frontend - (b8dbbe9) - Shubhranshu Sanjeev
- added partitions for audit_log table in cac schema - (e882d6e) - Shubhranshu Sanjeev
- jenkinsfile now sends build alerts in channel - (4ac23ce) - Kartik
- fixing error message for experiment create and bulk context api - (85cb360) - Jenkins
-  better logging - (c8a8428) - Kartik
-  Do not remove keys with null value on merge - (c3d7aa5) - ayush.jain@juspay.in
-  fix copy of experiment ID - (283b5c3) - Kartik
- returning error response if CAC call not 200 - (a5d2e58) - Shubhranshu Sanjeev
-  transpose columns in single experiment page for variants - (df72896) - Kartik
- autodeploy - (940c294) - Kartik
- added frontend crate to cog.toml - (e3da94c) - Shubhranshu Sanjeev
-  moved to AWS Public ECR for docker images - (aec7ea4) - Shubhranshu Sanjeev
-  update cargo.lock - (48a872d) - Kartik
-  added routes without service prefix for b/w compatibility - (1d3fcb9) - Shubhranshu Sanjeev
-  auto-create variantIds dimension - (1170766) - ankit.mahato
-  Functions bug fixes - (2a5ba57) - ankit.mahato
-  filter config fix - (768108c) - ankit.mahato
-  JS validator functions to take config value and key - (79343f0) - ankit.mahato
-  add path to node_modules - (0f5afe6) - Pratik Mishra
-  added service-prefix to functions endpoints - (03214cb) - ankit.mahato
-  function route fix - (afae0d2) - Pratik Mishra
-  fixed error in client - (c3badc0) - ankit.mahato
-  removed audit log middleware and reduced max db connection pool size to 2 - (5f9fdae) - Saurav Suman
- post merge release tagging - (f589018) - Kartik
- run github merge action only on PR merge - (30cb550) - Kartik
- merge build setup - (dc99b92) - Kartik
- resolve page failing when any type other than string is used (#21) - (f496634) - Datron
- js-client bundle update (#30) - (beb3d0b) - Ayush Jain
#### Build system
-  installing ca-certificates for ssl verification - (dca83d2) - Ritick Madaan
- added version tag to docker images - (a30456b) - Shubhranshu Sanjeev
#### Continuous Integration
- **(flake.nix)** pin nodejs version to 18 in flake - (ab3a038) - Natarajan Kannan
- made some miscellaneous changes for local setup - (c3adbfa) - Ritick Madaan
-  Created pipeline for automated-deployment - (fc84fc5) - Shrey Bana
-  Commented out prod docker push. - (d158192) - Shrey Bana
-  Enabling production docker image push. - (8123e67) - Shrey Bana
- moved nixpkgs to nixos-22.11 as the unstable one had broken rustfmt - (5cb87c8) - Ritick Madaan
-  Upgraded nixos to 23.05. - (6b4f1e5) - Shrey Bana
- automated newman test setup - (13cef1d) - Natarajan Kannan
-  switch to using newman - (494b490) - Natarajan Kannan
- regenerated schema.patch with latest schema.rs - (770d1b9) - Ritick Madaan
-  enabling tests in pr builds - (34e65b5) - Ritick Madaan
-  udpated `docker container ls` filter - (dc1ab25) - Ritick Madaan
- fix newman dev dependency ref - (31a9991) - Natarajan Kannan
- added postman collection for experimentation-platform - (61d2354) - Shubhranshu Sanjeev
- added cocogitto config for automatic versioning - (1c7d60c) - Shubhranshu Sanjeev
-  updated integ AP tracker curl with new version - (e7ee5d3) - Ritick Madaan
- deleting postgres's docker image on every test - (5f05038) - Ritick Madaan
- added 20 minutes timeout on pipeline - (5b7252e) - Shubhranshu Sanjeev
- added NY ECR registry push to Jenkins - (d1363cb) - Shubhranshu Sanjeev
- removing test tenant sqls after ci-test - (1ae6a4c) - Shubhranshu Sanjeev
- pushing cac image to NY sbx ECR - (e33c638) - Shubhranshu Sanjeev
#### Documentation
- () added setup instruction - (5169c75) - Saurav Suman
-  context aware config docs - (a3734a8) - Kartik
-  add intro doc and features - (3a083a0) - Natarajan Kannan
-  add intro doc and features - (92cfb10) - Natarajan Kannan
-  add intro doc and features - (43bf9c8) - Natarajan Kannan
-  add intro doc and features - (28aea00) - Natarajan Kannan
-  add intro doc and features - (68c667b) - Natarajan Kannan
#### Features
- context/add api along with db setup - (6e2cca2) - Ritick Madaan
-  Added context fetch API - (c4c287a) - Shrey Bana
-  Added health-check endpoint. - (668c046) - Shrey Bana
-  added localstack setup along with kms - (8a315d8) - Ritick Madaan
- /default-config/<key> PUT api - (844ca38) - Ritick Madaan
- GET /config api - (3bf06b6) - Ritick Madaan
-  Added context list API - (8c1b54b) - Shrey Bana
-  Added support for validation via JSON schema. - (cf9b580) - Shrey Bana
-  rust library - (9c4bda2) - Ritick Madaan
-  removed properties constraint on objects in schema - (d1a3f73) - Ritick Madaan
-  Added authentication. - (a7ceff9) - Shrey Bana
-  added DELETE /context/{ctx_id} api - (ac1c5b5) - Ritick Madaan
-  PUT /context/move/{ctx_id} api - (7db1c71) - Ritick Madaan
- added 304 <> last-modified for GET /config - (f9040de) - Saurav Suman
-  added list experiments API - (9120ec6) - Kartik Gajendra
- added conclude functionality for experiments - (03c289d) - Shubhranshu Sanjeev
-  added experimentation client with few fixes - (5198f50) - Kartik Gajendra
-  add support for last - (fe01a3c) - Kartik Gajendra
- added log table for all cac_v1 tables - (2f9a018) - Shubhranshu Sanjeev
-  added support for CUG in super position client - (af8cc43) - Kartik Gajendra
-  Added Catch all error type for robust error handling - (b8c9192) - Kartik Gajendra
-  Added Catch all error type for robust error handling - (88d18df) - Kartik Gajendra
-  implemented tracing-actix-web for logging - (3717953) - Ritick Madaan
-  add audit log search endpoint - (4a203ca) - Kartik Gajendra
-  added pod information in response headers and logs - (28a6c73) - Kartik Gajendra
-  record the chosen variant after conclude - (edcdeb6) - Kartik Gajendra
- Schema addition for Dimension values - (8fbdedd) - Prasanna P
-  Adding generic eval - (7f3c73c) - Pratik Mishra
-  cors middleware attached - (f553880) - Ritick Madaan
-  added dashboard auth middleware - (3104c5b) - Kartik Gajendra
- server's keep-alive time and db connection pool max size made configurable - (2c66356) - Ritick Madaan
- support to update experiment override_keys and variants - (492a384) - Shubhranshu Sanjeev
-  - Dimension value schema validation on context-addition - (77c1fc8) - Prasanna P
- added middleware and FromRequest for tenant and app scope info - (7778851) - Shubhranshu Sanjeev
- added multi-tenant support - (b79de2b) - Shubhranshu Sanjeev
- multi-tenant support for client libraries - (1611c90) - Shubhranshu Sanjeev
-  integrate authorize middleware - (1b5af6a) - Kartik Gajendra
- added frontend crate,combined frontend and backend binaries () - (dcf18a6) - Saurav Suman
- added format check in the JenkinsFile() - (a7055f8) - Saurav Suman
- update default keys - (1006533) - ankit.mahato
- ui for cac and exp - (10932a1) - Shubhranshu Sanjeev
- experiment UI - (80af3ab) - Kartik Gajendra
- added experiment-list page - (3a51c7c) - Shubhranshu Sanjeev
- working experiments page - (1df7dc2) - Kartik Gajendra
- added default config page - (59ac86b) - Saurav Suman
- added default config and override screen - (2e13520) - Saurav Suman
- working experiments page - (35179c4) - Kartik Gajendra
- fixed theme + ui changes + form validation + context validation error handling - (dc8b7e5) - Saurav Suman
- experiment create form - (0fccb5d) - Shubhranshu Sanjeev
- fixed experiment suspense block , added generic button - (a3c63cc) - Saurav Suman
- working resolve page - (b3096ac) - Kartik Gajendra
-  added authentication header for frontend apis - (83ba631) - Saurav Suman
-  client-integration-doc - (e161ee5) - Pratik Mishra
- support for service prefix - (f34705a) - Shubhranshu Sanjeev
-  js eval with node exec - (7190868) - Pratik Mishra
-  CRUD APIs for function validator - (5c4c7c6) - ankit.mahato
- autodeploy - (556e40f) - Kartik
-  added test,publish api for functions - (232996e) - Pratik Mishra
-  add node to app directory - (6987507) - Pratik Mishra
-  Replace merge-strategy option for resolve/eval - (45a3979) - ayush.jain@juspay.in
-  url click and text wrap fixes - (fa2004d) - Saurav CV
- added CAC language support - (bc3d48c) - Kartik Gajendra
- support more operations - (68afd4a) - Kartik Gajendra
-  Added function validation for context and default_config - (b8a7e5f) - ankit.mahato
-  haskell client for superposition - (129fee5) - Kartik
-  Filter Config by prefix - (465949e) - ankit.mahato
-  haskell client for superposition - (f6cb874) - Kartik
-  client interface improvements - (1453395) - Kartik
- -js-secure-sandbox - (b94f798) - Pratik Mishra
-  added new result, error type and error macros - (0af0975) - Shubhranshu Sanjeev
-  Add filter support to client - (378ddf2) - ankit.mahato
- add auth_type so this can be used when making API calls - (4427384) - Kartik
- ready for open source! - (f48db35) - Kartik
- improved error communication on frontend, with toast component - (51838eb) - Shubhranshu Sanjeev
- Add JS experiment client (#26) - (dbd101e) - Ayush Jain
#### Miscellaneous Chores
- **(ci)** Build using Nix - (8eed6c1) - Sridhar Ratnakumar
- **(nix)** Disable clippy - (8043256) - Sridhar Ratnakumar
- **(version)** v0.1.0 [skip ci] - (aae44b9) - Jenkins
- **(version)** v0.2.0 [skip ci] - (9c8c946) - Jenkins
- **(version)** v0.3.0 [skip ci] - (dfe67cb) - Jenkins
- **(version)** v0.4.0 [skip ci] - (73eff21) - Jenkins
- **(version)** v0.4.1 [skip ci] - (8bea246) - Jenkins
- **(version)** v0.5.0 [skip ci] - (780226e) - Jenkins
- **(version)** v0.5.1 [skip ci] - (62f7017) - Jenkins
- **(version)** v0.6.0 [skip ci] - (238eb62) - Jenkins
- **(version)** v0.6.1 [skip ci] - (aada669) - Jenkins
- **(version)** v0.7.0 [skip ci] - (6e524e4) - Jenkins
- **(version)** v0.7.1 [skip ci] - (c29118b) - Jenkins
- **(version)** v0.8.0 [skip ci] - (01bdc19) - Jenkins
- **(version)** v0.8.1 [skip ci] - (a7b5805) - Jenkins
- **(version)** v0.9.0 [skip ci] - (e85c797) - Jenkins
- **(version)** v0.9.1 [skip ci] - (d47f4f1) - Jenkins
- **(version)** v0.10.0 [skip ci] - (8d0f259) - Jenkins
- **(version)** v0.11.0 [skip ci] - (7c192aa) - Jenkins
- **(version)** v0.12.0 [skip ci] - (935d91e) - Jenkins
- **(version)** v0.12.1 [skip ci] - (fa4732b) - Jenkins
- **(version)** v0.13.0 [skip ci] - (ce463c1) - Jenkins
- **(version)** v0.14.0 [skip ci] - (ec56f27) - Jenkins
- **(version)** v0.14.1 [skip ci] - (438d4ff) - Jenkins
- **(version)** v0.15.0 [skip ci] - (7625d5f) - Jenkins
- **(version)** v0.15.1 [skip ci] - (23f20aa) - Jenkins
- **(version)** v0.16.0 [skip ci] - (ca5b4dd) - Jenkins
- **(version)** v0.16.1 [skip ci] - (908cde2) - Jenkins
- **(version)** v0.16.2 [skip ci] - (02e5e7e) - Jenkins
- **(version)** v0.16.3 [skip ci] - (bbf6da1) - Jenkins
- **(version)** v0.17.0 [skip ci] - (2414ce5) - Jenkins
- **(version)** v0.17.1 [skip ci] - (ae6fdc9) - Jenkins
- **(version)** v0.17.2 [skip ci] - (05a322e) - Jenkins
- **(version)** v0.17.3 [skip ci] - (8f36036) - Jenkins
- **(version)** v0.17.4 [skip ci] - (71a59ae) - Jenkins
- **(version)** v0.18.0 [skip ci] - (cc4ac18) - Jenkins
- **(version)** v0.18.1 [skip ci] - (85d1dd2) - Jenkins
- **(version)** v0.19.0 [skip ci] - (bfb0d26) - Jenkins
- **(version)** v0.20.0 [skip ci] - (865f762) - Jenkins
- **(version)** v0.20.1 [skip ci] - (3be0ebe) - Jenkins
- **(version)** v0.21.0 [skip ci] - (78d7833) - Jenkins
- **(version)** v0.22.0 [skip ci] - (bfccc4d) - Jenkins
- **(version)** v0.23.0 [skip ci] - (ba29b3a) - Jenkins
- **(version)** v0.24.0 [skip ci] - (6cefcdb) - Jenkins
- **(version)** v0.24.1 [skip ci] - (e88165c) - Jenkins
- **(version)** v0.24.2 [skip ci] - (5559f60) - Jenkins
- **(version)** v0.25.0 [skip ci] - (0cf2490) - Jenkins
- **(version)** v0.26.0 [skip ci] - (4646fc7) - Jenkins
- **(version)** v0.27.0 [skip ci] - (55330ad) - Jenkins
- **(version)** v0.28.0 [skip ci] - (b781755) - Jenkins
- **(version)** v0.29.0 [skip ci] - (eeb03e7) - Jenkins
- **(version)** v0.30.0 [skip ci] - (a51d143) - Jenkins
- **(version)** v0.30.1 [skip ci] - (4fc303f) - Jenkins
- **(version)** v0.31.0 [skip ci] - (82529b8) - Jenkins
- **(version)** v0.32.0 [skip ci] - (1c123d2) - Jenkins
- **(version)** v0.32.1 [skip ci] - (0b7f9eb) - Jenkins
- **(version)** v0.32.2 [skip ci] - (50b098c) - Jenkins
- **(version)** v0.33.0 [skip ci] - (c2ba364) - Jenkins
- **(version)** v0.34.0 [skip ci] - (dd35ac6) - Jenkins
- **(version)** v0.34.1 [skip ci] - (5594b35) - Jenkins
- **(version)** v0.34.2 [skip ci] - (b69e8ca) - Jenkins
- **(version)** v0.35.0 [skip ci] - (faf984e) - Jenkins
- **(version)** v0.36.0 [skip ci] - (ae95229) - Jenkins
- **(version)** v0.36.1 [skip ci] - (c29f849) - Jenkins
- **(version)** v0.37.0 [skip ci] - (68b7afd) - Jenkins
- **(version)** v0.38.0 [skip ci] - (80f47d5) - Jenkins
- **(version)** v0.38.1 [skip ci] - (01bfa1f) - Jenkins
- **(version)** v0.38.2 [skip ci] - (577bccb) - Jenkins
- **(version)** v0.39.0 [skip ci] - (84b8d06) - Jenkins
- **(version)** v0.39.1 [skip ci] - (a1d5570) - Jenkins
- **(version)** v0.40.0 [skip ci] - (836b769) - Jenkins
- **(version)** v0.41.0 [skip ci] - (ceefd83) - Superposition Bot
- **(version)** v0.42.0 [skip ci] - (e4bc080) - Superposition Bot
- database migration for dimensions table - (2a8e73a) - Ritick Madaan
-  move dependencies to workspaces - (20229c6) - Kartik Gajendra
- formatted code + cleanup - (1658102) - Shubhranshu Sanjeev
- experimentation docs first cut - (7e8ff75) - Shubhranshu Sanjeev
-  autodeploy to sbx - (851bb21) - Kartik
- rename superposition to experimentation - (bfbf86c) - Kartik
- open source superposition - (b85a0a8) - Kartik
- add PR testing workflows (#6) - (d6bb8d4) - Datron
- update merge workflow to work (#8) - (b280539) - Datron
- trigger semver workflow manually (#18) - (01110e2) - Datron
- push the commit as well - (37e3157) - Kartik
#### Refactoring
-  moved db related modules to db crate - (d3a14fe) - Ritick Madaan
-  removed old contexts table - (bdf4c53) - Ritick Madaan
- moved AppState & utility fx to new crate - (93ea2d8) - Shubhranshu Sanjeev
- moved cac to cargo workspaces - (91be31e) - Shubhranshu Sanjeev
- moved fetching db connection in FromRequest trait impl - (e5b3d6f) - Shubhranshu Sanjeev
- improvements to APIs - (f525de9) - Shubhranshu Sanjeev
- resolved comments - (193bd3f) - Shubhranshu Sanjeev
- moved tables and types out of cac_v1 schema - (384f0db) - Shubhranshu Sanjeev
- fixed warnings, added redirection for home page and script for setting up the project - (8a70e77) - Saurav Suman
- fixed warnings, added redirection for home page and script for setting up the project - (5a70988) - Saurav Suman
-  refactored service to use new error type and better error handling - (267cda1) - Shubhranshu Sanjeev
#### Revert
- Revert "fix:  logged env variable's value before kms decrypting" - (20e5745) - Ritick Madaan
#### Tests
- fix newman version used in tests - (545f97a) - Natarajan Kannan
- update to latest newman that handles top level events and body lang type - (a98985d) - Natarajan Kannan
- added tests for experiment helper fnxs - (8c6d298) - Shubhranshu Sanjeev
- added postman test for update override_keys api - (5121816) - Shubhranshu Sanjeev

- - -

## v0.42.0 - 2024-05-06
### Package updates
- caclang bumped to caclang-v0.2.0
- cac_client bumped to cac_client-v0.12.0
- experimentation_platform bumped to experimentation_platform-v0.15.0
- experimentation_client bumped to experimentation_client-v0.6.0
- frontend bumped to frontend-v0.8.0
- service_utils bumped to service_utils-v0.16.0
- superposition_types bumped to superposition_types-v0.2.0
- context_aware_config bumped to context_aware_config-v0.29.0
### Global changes
#### Bug Fixes
- corrected env for DB_PASSWORD and default for AWS_REGION - (d840100) - Ritick Madaan
- improved log for env not found - (19aaea5) - Ritick Madaan
- base64 decoding kms cypher - (a9a5516) - Ritick Madaan
- changed host to 0.0.0.0 - (9c76f91) - Ritick Madaan
-  dimension Table scaffolding, PUT<>DELETE apis `/dimension` - (a11d927) - Ritick Madaan
- storing pre calculated priority in contexts table - (e89389f) - Ritick Madaan
- removed delete dimension api - (224500f) - Ritick Madaan
-  DB password URI encoded. - (b3d2b5c) - Shrey Bana
-  moved override inside contexts table - (2b92946) - Ritick Madaan
-  removed unecessary and wrap over conditions - (71d4acb) - Ritick Madaan
-  added search path for schema in database_url - (8636c94) - Ritick Madaan
-  database schema url - (a5ebc02) - Ritick Madaan
-  added moved tables to cac_v1 schema - (edd84dd) - Ritick Madaan
-  enabled override updates in PUT /context by deep_merge - (10920fb) - Ritick Madaan
-  moved dimension_type to cac_v1 schema - (fb16bde) - Ritick Madaan
-  fixed ordering of /context endpoints - (857e1b7) - Ritick Madaan
-  updated response-type of /context/bulk-operations api - (8ff3bb3) - Ritick Madaan
- fixed context overlap check logic - (b7f81fe) - Shubhranshu Sanjeev
- added last_modified column and indexes - (8ba6cd6) - Shubhranshu Sanjeev
- moved tables and types under cac_v1 schema - (6e31830) - Shubhranshu Sanjeev
- calling cac apis for creating context - (97e8dcd) - Shubhranshu Sanjeev
-  updated last_modified in ramp - (19c9630) - ankit.mahato
-  minor fixes for exp client - (0494a96) - Kartik Gajendra
- removed traffic-percentage from experiment create request - (cae8b71) - Shubhranshu Sanjeev
-  added total items to list API response - (70ce4ff) - Kartik Gajendra
- using audit log tstamp for checking last-modified - (664a2aa) - Shubhranshu Sanjeev
-  sorting same priority contexts with created_at - (fc3344a) - Ritick Madaan
-  allowing cug users to fall under test variants - (8158524) - Ritick Madaan
-  removed unwanted parameter to prevent warning - (872dcc6) - Ritick Madaan
- added middleware to insert version in response headers - (3bdfac0) - Shubhranshu Sanjeev
-  logged env variable's value before kms decrypting - (1224359) - Ritick Madaan
- moved git init to separate stage - (2b0133a) - Shubhranshu Sanjeev
-  cleaned up Dockerfile - (abb7f9a) - Ritick Madaan
- fixed setting env in docker image - (be938dc) - Shubhranshu Sanjeev
- trimming newline character from version string - (374efb2) - Shubhranshu Sanjeev
- fixed random timeouts in internal http calls to CAC - (f80862d) - Shubhranshu Sanjeev
- failed build due to untracked schema.rs file changes - (b9e19b6) - Shubhranshu Sanjeev
-  eval param fix - (5963208) - Pratik Mishra
-  patching overrides on default-config instead of merge - (ba461ca) - Ritick Madaan
- ssh.bitbucket.juspay.net added to known hosts in docker bulid - (3a3dbd9) - Ritick Madaan
-  add user struct in delete context API - (698cc2c) - Kartik Gajendra
-  add migration for changing default_configs_keys - (1ed3cff) - Pratik Mishra
- validating override_keys for unique entries - (f759d38) - Shubhranshu Sanjeev
-  add all variants in manifest - (ca15d2b) - Pratik Mishra
- fixed failing health check (x-tenant header not set) - (850e7b2) - Shubhranshu Sanjeev
-  x-tenant header mandate removed for OPTIONS calls - (2254ad5) - Ritick Madaan
-  x-tenant header added for /config/resolve call in diff - (6b9516e) - Ritick Madaan
-  added external crate to cocogitto config - (23d34d6) - Ritick Madaan
- make sure envs with defaults prevent failure - (4f4cc82) - Kartik Gajendra
- cac service to set last_modified header - (6c7d792) - ankit.mahato
- Removing acceptance of override_keys in experiment create/update - (336b771) - ankit.mahato
- failing build due to update of schema.rs file - (f219dbf) - Shubhranshu Sanjeev
- add different auth types for exp requests to CAC - (a988be3) - Kartik Gajendra
- fixed deployment ConfigNotFound failure - (d0ba5f0) - Shubhranshu Sanjeev
-  sort json while context creation - (84e2693) - Pratik Mishra
- allow ramp 0 - (6666f3e) - Kartik Gajendra
-  - Cac client library changes to consume backend api response - (ccb40cd) - Prasanna P
-  fix json schema validation - (48017ea) - ankit.mahato
-  array validation for in condition - (f6b9d2e) - Pratik Mishra
- context parsing - (743fdcd) - Kartik Gajendra
- fixed experiment list page feedback - (4fa25e6) - Shubhranshu Sanjeev
- frontend multi-tenancy support + config and dimension page - (a632054) - Shubhranshu Sanjeev
- minor docs update - (0e3122a) - Kartik Gajendra
- fixed ci-test to support multi-tenant setup - (9ad6aa5) - Shubhranshu Sanjeev
- fixed build failure due to rust-version - (b6b0332) - Shubhranshu Sanjeev
- frontend build process - (3036d77) - Shubhranshu Sanjeev
- error resolving pages with internal call to server - (61c3909) - Shubhranshu Sanjeev
- fixed host resolve issue for internal calls in SSR. - (d2189f6) - Shubhranshu Sanjeev
- added partitions for 2025 and 2026 for audit table - (a3f75f1) - Shubhranshu Sanjeev
- getting api hostname from env for frontend - (b8dbbe9) - Shubhranshu Sanjeev
- added partitions for audit_log table in cac schema - (e882d6e) - Shubhranshu Sanjeev
- jenkinsfile now sends build alerts in channel - (4ac23ce) - Kartik
- fixing error message for experiment create and bulk context api - (85cb360) - Jenkins
-  better logging - (c8a8428) - Kartik
-  Do not remove keys with null value on merge - (c3d7aa5) - ayush.jain@juspay.in
-  fix copy of experiment ID - (283b5c3) - Kartik
- returning error response if CAC call not 200 - (a5d2e58) - Shubhranshu Sanjeev
-  transpose columns in single experiment page for variants - (df72896) - Kartik
- autodeploy - (940c294) - Kartik
- added frontend crate to cog.toml - (e3da94c) - Shubhranshu Sanjeev
-  moved to AWS Public ECR for docker images - (aec7ea4) - Shubhranshu Sanjeev
-  update cargo.lock - (48a872d) - Kartik
-  added routes without service prefix for b/w compatibility - (1d3fcb9) - Shubhranshu Sanjeev
-  auto-create variantIds dimension - (1170766) - ankit.mahato
-  Functions bug fixes - (2a5ba57) - ankit.mahato
-  filter config fix - (768108c) - ankit.mahato
-  JS validator functions to take config value and key - (79343f0) - ankit.mahato
-  add path to node_modules - (0f5afe6) - Pratik Mishra
-  added service-prefix to functions endpoints - (03214cb) - ankit.mahato
-  function route fix - (afae0d2) - Pratik Mishra
-  fixed error in client - (c3badc0) - ankit.mahato
-  removed audit log middleware and reduced max db connection pool size to 2 - (5f9fdae) - Saurav Suman
- post merge release tagging - (f589018) - Kartik
- run github merge action only on PR merge - (30cb550) - Kartik
- merge build setup - (dc99b92) - Kartik
- resolve page failing when any type other than string is used (#21) - (f496634) - Datron
#### Build system
-  installing ca-certificates for ssl verification - (dca83d2) - Ritick Madaan
- added version tag to docker images - (a30456b) - Shubhranshu Sanjeev
#### Continuous Integration
- **(flake.nix)** pin nodejs version to 18 in flake - (ab3a038) - Natarajan Kannan
- made some miscellaneous changes for local setup - (c3adbfa) - Ritick Madaan
-  Created pipeline for automated-deployment - (fc84fc5) - Shrey Bana
-  Commented out prod docker push. - (d158192) - Shrey Bana
-  Enabling production docker image push. - (8123e67) - Shrey Bana
- moved nixpkgs to nixos-22.11 as the unstable one had broken rustfmt - (5cb87c8) - Ritick Madaan
-  Upgraded nixos to 23.05. - (6b4f1e5) - Shrey Bana
- automated newman test setup - (13cef1d) - Natarajan Kannan
-  switch to using newman - (494b490) - Natarajan Kannan
- regenerated schema.patch with latest schema.rs - (770d1b9) - Ritick Madaan
-  enabling tests in pr builds - (34e65b5) - Ritick Madaan
-  udpated `docker container ls` filter - (dc1ab25) - Ritick Madaan
- fix newman dev dependency ref - (31a9991) - Natarajan Kannan
- added postman collection for experimentation-platform - (61d2354) - Shubhranshu Sanjeev
- added cocogitto config for automatic versioning - (1c7d60c) - Shubhranshu Sanjeev
-  updated integ AP tracker curl with new version - (e7ee5d3) - Ritick Madaan
- deleting postgres's docker image on every test - (5f05038) - Ritick Madaan
- added 20 minutes timeout on pipeline - (5b7252e) - Shubhranshu Sanjeev
- added NY ECR registry push to Jenkins - (d1363cb) - Shubhranshu Sanjeev
- removing test tenant sqls after ci-test - (1ae6a4c) - Shubhranshu Sanjeev
- pushing cac image to NY sbx ECR - (e33c638) - Shubhranshu Sanjeev
#### Documentation
- () added setup instruction - (5169c75) - Saurav Suman
-  context aware config docs - (a3734a8) - Kartik
-  add intro doc and features - (3a083a0) - Natarajan Kannan
-  add intro doc and features - (92cfb10) - Natarajan Kannan
-  add intro doc and features - (43bf9c8) - Natarajan Kannan
-  add intro doc and features - (28aea00) - Natarajan Kannan
-  add intro doc and features - (68c667b) - Natarajan Kannan
#### Features
- context/add api along with db setup - (6e2cca2) - Ritick Madaan
-  Added context fetch API - (c4c287a) - Shrey Bana
-  Added health-check endpoint. - (668c046) - Shrey Bana
-  added localstack setup along with kms - (8a315d8) - Ritick Madaan
- /default-config/<key> PUT api - (844ca38) - Ritick Madaan
- GET /config api - (3bf06b6) - Ritick Madaan
-  Added context list API - (8c1b54b) - Shrey Bana
-  Added support for validation via JSON schema. - (cf9b580) - Shrey Bana
-  rust library - (9c4bda2) - Ritick Madaan
-  removed properties constraint on objects in schema - (d1a3f73) - Ritick Madaan
-  Added authentication. - (a7ceff9) - Shrey Bana
-  added DELETE /context/{ctx_id} api - (ac1c5b5) - Ritick Madaan
-  PUT /context/move/{ctx_id} api - (7db1c71) - Ritick Madaan
- added 304 <> last-modified for GET /config - (f9040de) - Saurav Suman
-  added list experiments API - (9120ec6) - Kartik Gajendra
- added conclude functionality for experiments - (03c289d) - Shubhranshu Sanjeev
-  added experimentation client with few fixes - (5198f50) - Kartik Gajendra
-  add support for last - (fe01a3c) - Kartik Gajendra
- added log table for all cac_v1 tables - (2f9a018) - Shubhranshu Sanjeev
-  added support for CUG in super position client - (af8cc43) - Kartik Gajendra
-  Added Catch all error type for robust error handling - (b8c9192) - Kartik Gajendra
-  Added Catch all error type for robust error handling - (88d18df) - Kartik Gajendra
-  implemented tracing-actix-web for logging - (3717953) - Ritick Madaan
-  add audit log search endpoint - (4a203ca) - Kartik Gajendra
-  added pod information in response headers and logs - (28a6c73) - Kartik Gajendra
-  record the chosen variant after conclude - (edcdeb6) - Kartik Gajendra
- Schema addition for Dimension values - (8fbdedd) - Prasanna P
-  Adding generic eval - (7f3c73c) - Pratik Mishra
-  cors middleware attached - (f553880) - Ritick Madaan
-  added dashboard auth middleware - (3104c5b) - Kartik Gajendra
- server's keep-alive time and db connection pool max size made configurable - (2c66356) - Ritick Madaan
- support to update experiment override_keys and variants - (492a384) - Shubhranshu Sanjeev
-  - Dimension value schema validation on context-addition - (77c1fc8) - Prasanna P
- added middleware and FromRequest for tenant and app scope info - (7778851) - Shubhranshu Sanjeev
- added multi-tenant support - (b79de2b) - Shubhranshu Sanjeev
- multi-tenant support for client libraries - (1611c90) - Shubhranshu Sanjeev
-  integrate authorize middleware - (1b5af6a) - Kartik Gajendra
- added frontend crate,combined frontend and backend binaries () - (dcf18a6) - Saurav Suman
- added format check in the JenkinsFile() - (a7055f8) - Saurav Suman
- update default keys - (1006533) - ankit.mahato
- ui for cac and exp - (10932a1) - Shubhranshu Sanjeev
- experiment UI - (80af3ab) - Kartik Gajendra
- added experiment-list page - (3a51c7c) - Shubhranshu Sanjeev
- working experiments page - (1df7dc2) - Kartik Gajendra
- added default config page - (59ac86b) - Saurav Suman
- added default config and override screen - (2e13520) - Saurav Suman
- working experiments page - (35179c4) - Kartik Gajendra
- fixed theme + ui changes + form validation + context validation error handling - (dc8b7e5) - Saurav Suman
- experiment create form - (0fccb5d) - Shubhranshu Sanjeev
- fixed experiment suspense block , added generic button - (a3c63cc) - Saurav Suman
- working resolve page - (b3096ac) - Kartik Gajendra
-  added authentication header for frontend apis - (83ba631) - Saurav Suman
-  client-integration-doc - (e161ee5) - Pratik Mishra
- support for service prefix - (f34705a) - Shubhranshu Sanjeev
-  js eval with node exec - (7190868) - Pratik Mishra
-  CRUD APIs for function validator - (5c4c7c6) - ankit.mahato
- autodeploy - (556e40f) - Kartik
-  added test,publish api for functions - (232996e) - Pratik Mishra
-  add node to app directory - (6987507) - Pratik Mishra
-  Replace merge-strategy option for resolve/eval - (45a3979) - ayush.jain@juspay.in
-  url click and text wrap fixes - (fa2004d) - Saurav CV
- added CAC language support - (bc3d48c) - Kartik Gajendra
- support more operations - (68afd4a) - Kartik Gajendra
-  Added function validation for context and default_config - (b8a7e5f) - ankit.mahato
-  haskell client for superposition - (129fee5) - Kartik
-  Filter Config by prefix - (465949e) - ankit.mahato
-  haskell client for superposition - (f6cb874) - Kartik
-  client interface improvements - (1453395) - Kartik
- -js-secure-sandbox - (b94f798) - Pratik Mishra
-  added new result, error type and error macros - (0af0975) - Shubhranshu Sanjeev
-  Add filter support to client - (378ddf2) - ankit.mahato
- add auth_type so this can be used when making API calls - (4427384) - Kartik
- ready for open source! - (f48db35) - Kartik
- improved error communication on frontend, with toast component - (51838eb) - Shubhranshu Sanjeev
- Add JS experiment client (#26) - (dbd101e) - Ayush Jain
#### Miscellaneous Chores
- **(ci)** Build using Nix - (8eed6c1) - Sridhar Ratnakumar
- **(nix)** Disable clippy - (8043256) - Sridhar Ratnakumar
- **(version)** v0.1.0 [skip ci] - (aae44b9) - Jenkins
- **(version)** v0.2.0 [skip ci] - (9c8c946) - Jenkins
- **(version)** v0.3.0 [skip ci] - (dfe67cb) - Jenkins
- **(version)** v0.4.0 [skip ci] - (73eff21) - Jenkins
- **(version)** v0.4.1 [skip ci] - (8bea246) - Jenkins
- **(version)** v0.5.0 [skip ci] - (780226e) - Jenkins
- **(version)** v0.5.1 [skip ci] - (62f7017) - Jenkins
- **(version)** v0.6.0 [skip ci] - (238eb62) - Jenkins
- **(version)** v0.6.1 [skip ci] - (aada669) - Jenkins
- **(version)** v0.7.0 [skip ci] - (6e524e4) - Jenkins
- **(version)** v0.7.1 [skip ci] - (c29118b) - Jenkins
- **(version)** v0.8.0 [skip ci] - (01bdc19) - Jenkins
- **(version)** v0.8.1 [skip ci] - (a7b5805) - Jenkins
- **(version)** v0.9.0 [skip ci] - (e85c797) - Jenkins
- **(version)** v0.9.1 [skip ci] - (d47f4f1) - Jenkins
- **(version)** v0.10.0 [skip ci] - (8d0f259) - Jenkins
- **(version)** v0.11.0 [skip ci] - (7c192aa) - Jenkins
- **(version)** v0.12.0 [skip ci] - (935d91e) - Jenkins
- **(version)** v0.12.1 [skip ci] - (fa4732b) - Jenkins
- **(version)** v0.13.0 [skip ci] - (ce463c1) - Jenkins
- **(version)** v0.14.0 [skip ci] - (ec56f27) - Jenkins
- **(version)** v0.14.1 [skip ci] - (438d4ff) - Jenkins
- **(version)** v0.15.0 [skip ci] - (7625d5f) - Jenkins
- **(version)** v0.15.1 [skip ci] - (23f20aa) - Jenkins
- **(version)** v0.16.0 [skip ci] - (ca5b4dd) - Jenkins
- **(version)** v0.16.1 [skip ci] - (908cde2) - Jenkins
- **(version)** v0.16.2 [skip ci] - (02e5e7e) - Jenkins
- **(version)** v0.16.3 [skip ci] - (bbf6da1) - Jenkins
- **(version)** v0.17.0 [skip ci] - (2414ce5) - Jenkins
- **(version)** v0.17.1 [skip ci] - (ae6fdc9) - Jenkins
- **(version)** v0.17.2 [skip ci] - (05a322e) - Jenkins
- **(version)** v0.17.3 [skip ci] - (8f36036) - Jenkins
- **(version)** v0.17.4 [skip ci] - (71a59ae) - Jenkins
- **(version)** v0.18.0 [skip ci] - (cc4ac18) - Jenkins
- **(version)** v0.18.1 [skip ci] - (85d1dd2) - Jenkins
- **(version)** v0.19.0 [skip ci] - (bfb0d26) - Jenkins
- **(version)** v0.20.0 [skip ci] - (865f762) - Jenkins
- **(version)** v0.20.1 [skip ci] - (3be0ebe) - Jenkins
- **(version)** v0.21.0 [skip ci] - (78d7833) - Jenkins
- **(version)** v0.22.0 [skip ci] - (bfccc4d) - Jenkins
- **(version)** v0.23.0 [skip ci] - (ba29b3a) - Jenkins
- **(version)** v0.24.0 [skip ci] - (6cefcdb) - Jenkins
- **(version)** v0.24.1 [skip ci] - (e88165c) - Jenkins
- **(version)** v0.24.2 [skip ci] - (5559f60) - Jenkins
- **(version)** v0.25.0 [skip ci] - (0cf2490) - Jenkins
- **(version)** v0.26.0 [skip ci] - (4646fc7) - Jenkins
- **(version)** v0.27.0 [skip ci] - (55330ad) - Jenkins
- **(version)** v0.28.0 [skip ci] - (b781755) - Jenkins
- **(version)** v0.29.0 [skip ci] - (eeb03e7) - Jenkins
- **(version)** v0.30.0 [skip ci] - (a51d143) - Jenkins
- **(version)** v0.30.1 [skip ci] - (4fc303f) - Jenkins
- **(version)** v0.31.0 [skip ci] - (82529b8) - Jenkins
- **(version)** v0.32.0 [skip ci] - (1c123d2) - Jenkins
- **(version)** v0.32.1 [skip ci] - (0b7f9eb) - Jenkins
- **(version)** v0.32.2 [skip ci] - (50b098c) - Jenkins
- **(version)** v0.33.0 [skip ci] - (c2ba364) - Jenkins
- **(version)** v0.34.0 [skip ci] - (dd35ac6) - Jenkins
- **(version)** v0.34.1 [skip ci] - (5594b35) - Jenkins
- **(version)** v0.34.2 [skip ci] - (b69e8ca) - Jenkins
- **(version)** v0.35.0 [skip ci] - (faf984e) - Jenkins
- **(version)** v0.36.0 [skip ci] - (ae95229) - Jenkins
- **(version)** v0.36.1 [skip ci] - (c29f849) - Jenkins
- **(version)** v0.37.0 [skip ci] - (68b7afd) - Jenkins
- **(version)** v0.38.0 [skip ci] - (80f47d5) - Jenkins
- **(version)** v0.38.1 [skip ci] - (01bfa1f) - Jenkins
- **(version)** v0.38.2 [skip ci] - (577bccb) - Jenkins
- **(version)** v0.39.0 [skip ci] - (84b8d06) - Jenkins
- **(version)** v0.39.1 [skip ci] - (a1d5570) - Jenkins
- **(version)** v0.40.0 [skip ci] - (836b769) - Jenkins
- **(version)** v0.41.0 [skip ci] - (ceefd83) - Superposition Bot
- database migration for dimensions table - (2a8e73a) - Ritick Madaan
-  move dependencies to workspaces - (20229c6) - Kartik Gajendra
- formatted code + cleanup - (1658102) - Shubhranshu Sanjeev
- experimentation docs first cut - (7e8ff75) - Shubhranshu Sanjeev
-  autodeploy to sbx - (851bb21) - Kartik
- rename superposition to experimentation - (bfbf86c) - Kartik
- open source superposition - (b85a0a8) - Kartik
- add PR testing workflows (#6) - (d6bb8d4) - Datron
- update merge workflow to work (#8) - (b280539) - Datron
- trigger semver workflow manually (#18) - (01110e2) - Datron
- push the commit as well - (37e3157) - Kartik
#### Refactoring
-  moved db related modules to db crate - (d3a14fe) - Ritick Madaan
-  removed old contexts table - (bdf4c53) - Ritick Madaan
- moved AppState & utility fx to new crate - (93ea2d8) - Shubhranshu Sanjeev
- moved cac to cargo workspaces - (91be31e) - Shubhranshu Sanjeev
- moved fetching db connection in FromRequest trait impl - (e5b3d6f) - Shubhranshu Sanjeev
- improvements to APIs - (f525de9) - Shubhranshu Sanjeev
- resolved comments - (193bd3f) - Shubhranshu Sanjeev
- moved tables and types out of cac_v1 schema - (384f0db) - Shubhranshu Sanjeev
- fixed warnings, added redirection for home page and script for setting up the project - (8a70e77) - Saurav Suman
- fixed warnings, added redirection for home page and script for setting up the project - (5a70988) - Saurav Suman
-  refactored service to use new error type and better error handling - (267cda1) - Shubhranshu Sanjeev
#### Revert
- Revert "fix:  logged env variable's value before kms decrypting" - (20e5745) - Ritick Madaan
#### Tests
- fix newman version used in tests - (545f97a) - Natarajan Kannan
- update to latest newman that handles top level events and body lang type - (a98985d) - Natarajan Kannan
- added tests for experiment helper fnxs - (8c6d298) - Shubhranshu Sanjeev
- added postman test for update override_keys api - (5121816) - Shubhranshu Sanjeev

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