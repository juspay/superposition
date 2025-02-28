# Changelog
All notable changes to this project will be documented in this file. See [conventional commits](https://www.conventionalcommits.org/) for commit guidelines.

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