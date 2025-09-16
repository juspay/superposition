# Changelog
All notable changes to this project will be documented in this file. See [conventional commits](https://www.conventionalcommits.org/) for commit guidelines.

- - -
## experimentation_client-v0.20.0 - 2025-09-16
#### Features
- Exact match option for context and experiment filter (#700) - (0944a86) - Ayush Jain
#### Miscellaneous Chores
- **(version)** v0.89.0 [skip ci] - (d68f44e) - Superposition Bot

- - -

## experimentation_client-v0.19.0 - 2025-09-09
#### Bug Fixes
- Experiment applicable group resolution for system generated (#692) - (fa1c5a6) - Ayush Jain
#### Features
- Add option to fetch global experiments only (#691) - (c65dd9a) - Ayush Jain
- Add QueryString proc_macro_derive (#656) - (30b4584) - Ayush Jain
#### Miscellaneous Chores
- **(version)** v0.88.0 [skip ci] - (e23bb63) - Superposition Bot

- - -

## experimentation_client-v0.18.0 - 2025-08-19
#### Features
- Move jsonlogic behind compile time flag - (328671c) - ayush.jain@juspay.in
#### Miscellaneous Chores
- **(version)** v0.86.0 [skip ci] - (f11fc97) - Superposition Bot

- - -

## experimentation_client-v0.17.1 - 2025-08-12
#### Bug Fixes
- bucketing logic - (e7a74d9) - Ankit.Mahato
#### Miscellaneous Chores
- **(version)** v0.83.0 [skip ci] - (2e2d48c) - Superposition Bot

- - -

## experimentation_client-v0.17.0 - 2025-08-04
#### Features
- publish rust libraries through crates.io - (8b62d45) - datron

- - -

## experimentation_client-v0.14.1 - 2025-07-31
#### Bug Fixes
- bucketing logic (#629) - (ebb2fab) - Ankit Kumar Mahato

- - -

## experimentation_client-v0.14.0 - 2025-07-30
#### Features
- experiment bucketing - (274afdf) - Ankit.Mahato

- - -

## experimentation_client-v0.13.3 - 2025-07-10
#### Bug Fixes
- Experiment list group id filter - (588e469) - ayush.jain@juspay.in

- - -

## experimentation_client-v0.13.2 - 2025-07-07
#### Bug Fixes
- exp client (#579) - (53a87f6) - PRATIK MISHRA

- - -

## experimentation_client-v0.13.1 - 2025-07-03
#### Bug Fixes
- Show filters in all pages - (8ba1619) - ayush.jain@juspay.in
- experiment client fetch fix (#555) - (cb08467) - PRATIK MISHRA

- - -

## experimentation_client-v0.13.0 - 2025-04-17
#### Features
- Add types for experiment apis in superposition_types - (219a2eb) - Ayush Jain

- - -

## experimentation_client-v0.12.0 - 2025-03-26
#### Features
- generate multiple binaries for clients based on platform (#445) - (cd6d30b) - Datron

- - -

## experimentation_client-v0.11.0 - 2025-02-14
#### Features
- Discard experiment - (42ac967) - ayush.jain@juspay.in

- - -

## experimentation_client-v0.10.0 - 2025-01-23
#### Features
- Use Common db model types in frontend (#291) - (e68782d) - Ayush Jain
#### Refactoring
- merge cac and experimentation schemas - (51367a6) - Kartik

- - -

## experimentation_client-v0.9.0 - 2024-10-23
#### Features
- Search contexts by dimension values (#264) - (12743af) - Ayush Jain

- - -

## experimentation_client-v0.8.2 - 2024-09-30
#### Bug Fixes
- Dependency pruning (#250) - (8b68900) - Ayush Jain

- - -

## experimentation_client-v0.8.1 - 2024-09-06
#### Bug Fixes
- python client (#219) - (31a9887) - PRATIK MISHRA

- - -

## experimentation_client-v0.8.0 - 2024-06-27
#### Features
- Rework async library in CAC client (#88) - (5610b93) - Datron

- - -

## experimentation_client-v0.7.2 - 2024-06-20
#### Bug Fixes
- Use jsonlogc's partial_apply for config and experiment filtering (#127) - (97bf39b) - Ayush Jain
#### Miscellaneous Chores
- rename upper case enum variants to use `PascalCase` names (#94) - (6d3f4ca) - Sanchith Hegde
- address some clippy lints (#85) - (001b8d4) - Sanchith Hegde
- simplify module structure and follow Rust 2018 edition module naming convention (#86) - (403ea59) - Sanchith Hegde

- - -

## experimentation_client-v0.7.1 - 2024-05-31
#### Bug Fixes
- Add prefix to client's methods (#84) - (ee2c54f) - Ankit Kumar Mahato

- - -

## experimentation_client-v0.7.0 - 2024-05-29
#### Bug Fixes
- creating experiments for default-config (no context) (#38) - (4f6b92c) - Sauravcv98
#### Features
- filter support in experimentation client (#35) - (0c9d070) - Ankit Kumar Mahato

- - -

## experimentation_client-v0.6.0 - 2024-05-06
#### Features
-  client interface improvements - (1453395) - Kartik
#### Miscellaneous Chores
- **(version)** v0.42.0 [skip ci] - (e4bc080) - Superposition Bot
- rename superposition to experimentation - (bfbf86c) - Kartik

- - -

## experimentation_client-v0.6.0 - 2024-05-06
#### Features
-  client interface improvements - (1453395) - Kartik
#### Miscellaneous Chores
- rename superposition to experimentation - (bfbf86c) - Kartik

- - -

## experimentation_client-v0.5.0 - 2024-03-18
#### Documentation
- PICAF-25981: add intro doc and features - (d09ba53) - Natarajan Kannan
#### Features
- [PICAF-26126] haskell client for superposition - (7106b56) - Kartik

- - -

## experimentation_client-v0.4.0 - 2023-11-11
#### Features
- added format check in the JenkinsFile(PICAF-24813) - (4fdf864) - Saurav Suman
#### Miscellaneous Chores
- [PICAF-24778] move dependencies to workspaces - (38a524f) - Kartik Gajendra

- - -

## experimentation_client-v0.3.0 - 2023-10-27
#### Features
- multi-tenant support for client libraries - (c603be0) - Shubhranshu Sanjeev

- - -

## experimentation_client-v0.2.0 - 2023-10-20
#### Features
- PICAF-23643 - Dimension value schema validation on context-addition - (b2fad9e) - Prasanna P

- - -

## experimentation_client-v0.1.3 - 2023-10-13
#### Bug Fixes
- PICAF-24612 add all variants in manifest - (0f15ac9) - Pratik Mishra

- - -

## experimentation_client-v0.1.2 - 2023-09-06
#### Bug Fixes
- trimming newline character from version string - (2c61077) - Shubhranshu Sanjeev

- - -

## experimentation_client-v0.1.1 - 2023-09-06
#### Bug Fixes
- fixed setting env in docker image - (272454b) - Shubhranshu Sanjeev
#### Continuous Integration
- PICAF-24114 updated integ AP tracker curl with new version - (1e0fa5b) - Ritick Madaan

- - -

## experimentation_client-v0.1.0 - 2023-09-01
#### Bug Fixes
- PICAF-24114 removed unwanted parameter to prevent warning - (3de7fe7) - Ritick Madaan
- PICAF-24114 allowing cug users to fall under test variants - (c095333) - Ritick Madaan
- [PICAF-23846] added total items to list API response - (17955fa) - Kartik Gajendra
- PICAF-23632 - (247542e) - Ritick Madaan
- [PICAF-23632] minor fixes for exp client - (64deee5) - Kartik Gajendra
#### Features
- [PICAF-24010] added support for CUG in super position client - (4eeae99) - Kartik Gajendra
- [PICAF-23632] added experimentation client with few fixes - (9a31815) - Kartik Gajendra

- - -

Changelog generated by [cocogitto](https://github.com/cocogitto/cocogitto).