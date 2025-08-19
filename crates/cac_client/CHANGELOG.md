# Changelog
All notable changes to this project will be documented in this file. See [conventional commits](https://www.conventionalcommits.org/) for commit guidelines.

- - -
## cac_client-v0.21.0 - 2025-08-19
#### Features
- Move jsonlogic behind compile time flag - (328671c) - ayush.jain@juspay.in
#### Miscellaneous Chores
- **(version)** v0.83.0 [skip ci] - (2e2d48c) - Superposition Bot

- - -

## cac_client-v0.20.0 - 2025-08-04
#### Features
- publish rust libraries through crates.io - (8b62d45) - datron

- - -

## cac_client-v0.17.0 - 2025-03-26
#### Features
- generate multiple binaries for clients based on platform (#445) - (cd6d30b) - Datron

- - -

## cac_client-v0.16.2 - 2025-02-28
#### Bug Fixes
- Fixed clippy warnings - (88bbfe9) - ShreyBana

- - -

## cac_client-v0.16.1 - 2024-12-04
#### Bug Fixes
- invalidate client cache on config change (#298) - (16f4477) - PRATIK MISHRA

- - -

## cac_client-v0.16.0 - 2024-11-20
#### Features
- add cache in cac client (#268) - (0cd82dd) - PRATIK MISHRA

- - -

## cac_client-v0.15.0 - 2024-10-23
#### Features
- Search contexts by dimension values (#264) - (12743af) - Ayush Jain

- - -

## cac_client-v0.14.3 - 2024-09-30
#### Bug Fixes
- Dependency pruning (#250) - (8b68900) - Ayush Jain

- - -

## cac_client-v0.14.2 - 2024-09-06
#### Bug Fixes
- python client (#219) - (31a9887) - PRATIK MISHRA

- - -

## cac_client-v0.14.1 - 2024-08-08
#### Bug Fixes
- api validation with new types (#146) - (66ad741) - PRATIK MISHRA

- - -

## cac_client-v0.14.0 - 2024-07-11
#### Features
- move apperror to superposition_types - (f1c8395) - Pratik Mishra

- - -

## cac_client-v0.13.0 - 2024-06-27
#### Features
- Rework async library in CAC client (#88) - (5610b93) - Datron

- - -

## cac_client-v0.12.2 - 2024-06-20
#### Bug Fixes
- Use jsonlogc's partial_apply for config and experiment filtering (#127) - (97bf39b) - Ayush Jain
#### Miscellaneous Chores
- address some clippy lints (#85) - (001b8d4) - Sanchith Hegde
- simplify module structure and follow Rust 2018 edition module naming convention (#86) - (403ea59) - Sanchith Hegde

- - -

## cac_client-v0.12.1 - 2024-05-31
#### Bug Fixes
- Add prefix to client's methods (#84) - (ee2c54f) - Ankit Kumar Mahato

- - -

## cac_client-v0.12.0 - 2024-05-06
#### Bug Fixes
-  removed audit log middleware and reduced max db connection pool size to 2 - (5f9fdae) - Saurav Suman
-  fixed error in client - (c3badc0) - ankit.mahato
-  empty key filters should return all keys - (e053b5e) - Kartik
-  Do not remove keys with null value on merge - (c3d7aa5) - ayush.jain@juspay.in
- cac service to set last_modified header - (6c7d792) - ankit.mahato
-  patching overrides on default-config instead of merge - (ba461ca) - Ritick Madaan
-  eval param fix - (5963208) - Pratik Mishra
#### Documentation
-  add intro doc and features - (68c667b) - Natarajan Kannan
-  add intro doc and features - (28aea00) - Natarajan Kannan
#### Features
- ready for open source! - (f48db35) - Kartik
-  Add filter support to client - (378ddf2) - ankit.mahato
-  client interface improvements - (1453395) - Kartik
-  haskell client for superposition - (f6cb874) - Kartik
-  haskell client for superposition - (129fee5) - Kartik
-  Replace merge-strategy option for resolve/eval - (45a3979) - ayush.jain@juspay.in
- working resolve page - (b3096ac) - Kartik Gajendra
- added format check in the JenkinsFile() - (a7055f8) - Saurav Suman
- multi-tenant support for client libraries - (1611c90) - Shubhranshu Sanjeev
-  cac eval return update - (06b1cb2) - Pratik Mishra
-  Adding generic eval - (7f3c73c) - Pratik Mishra
-  added experimentation client with few fixes - (5198f50) - Kartik Gajendra
#### Miscellaneous Chores
- **(version)** v0.42.0 [skip ci] - (e4bc080) - Superposition Bot
- **(version)** v0.41.0 [skip ci] - (ceefd83) - Superposition Bot
- **(version)** v0.40.0 [skip ci] - (836b769) - Jenkins
- **(version)** v0.39.1 [skip ci] - (a1d5570) - Jenkins
- **(version)** v0.39.0 [skip ci] - (84b8d06) - Jenkins
- **(version)** v0.35.0 [skip ci] - (faf984e) - Jenkins
- **(version)** v0.32.0 [skip ci] - (1c123d2) - Jenkins
- **(version)** v0.28.0 [skip ci] - (b781755) - Jenkins
- **(version)** v0.24.0 [skip ci] - (6cefcdb) - Jenkins
- **(version)** v0.17.0 [skip ci] - (2414ce5) - Jenkins
- **(version)** v0.15.0 [skip ci] - (7625d5f) - Jenkins
- **(version)** v0.12.0 [skip ci] - (935d91e) - Jenkins
- **(version)** v0.6.1 [skip ci] - (aada669) - Jenkins
- **(version)** v0.6.0 [skip ci] - (238eb62) - Jenkins
- **(version)** v0.1.0 [skip ci] - (aae44b9) - Jenkins
-  move dependencies to workspaces - (20229c6) - Kartik Gajendra

- - -

## cac_client-v0.12.0 - 2024-05-06
#### Bug Fixes
-  removed audit log middleware and reduced max db connection pool size to 2 - (5f9fdae) - Saurav Suman
-  fixed error in client - (c3badc0) - ankit.mahato
-  empty key filters should return all keys - (e053b5e) - Kartik
-  Do not remove keys with null value on merge - (c3d7aa5) - ayush.jain@juspay.in
- cac service to set last_modified header - (6c7d792) - ankit.mahato
-  patching overrides on default-config instead of merge - (ba461ca) - Ritick Madaan
-  eval param fix - (5963208) - Pratik Mishra
#### Documentation
-  add intro doc and features - (68c667b) - Natarajan Kannan
-  add intro doc and features - (28aea00) - Natarajan Kannan
#### Features
- ready for open source! - (f48db35) - Kartik
-  Add filter support to client - (378ddf2) - ankit.mahato
-  client interface improvements - (1453395) - Kartik
-  haskell client for superposition - (f6cb874) - Kartik
-  haskell client for superposition - (129fee5) - Kartik
-  Replace merge-strategy option for resolve/eval - (45a3979) - ayush.jain@juspay.in
- working resolve page - (b3096ac) - Kartik Gajendra
- added format check in the JenkinsFile() - (a7055f8) - Saurav Suman
- multi-tenant support for client libraries - (1611c90) - Shubhranshu Sanjeev
-  cac eval return update - (06b1cb2) - Pratik Mishra
-  Adding generic eval - (7f3c73c) - Pratik Mishra
-  added experimentation client with few fixes - (5198f50) - Kartik Gajendra
#### Miscellaneous Chores
- **(version)** v0.41.0 [skip ci] - (ceefd83) - Superposition Bot
- **(version)** v0.40.0 [skip ci] - (836b769) - Jenkins
- **(version)** v0.39.1 [skip ci] - (a1d5570) - Jenkins
- **(version)** v0.39.0 [skip ci] - (84b8d06) - Jenkins
- **(version)** v0.35.0 [skip ci] - (faf984e) - Jenkins
- **(version)** v0.32.0 [skip ci] - (1c123d2) - Jenkins
- **(version)** v0.28.0 [skip ci] - (b781755) - Jenkins
- **(version)** v0.24.0 [skip ci] - (6cefcdb) - Jenkins
- **(version)** v0.17.0 [skip ci] - (2414ce5) - Jenkins
- **(version)** v0.15.0 [skip ci] - (7625d5f) - Jenkins
- **(version)** v0.12.0 [skip ci] - (935d91e) - Jenkins
- **(version)** v0.6.1 [skip ci] - (aada669) - Jenkins
- **(version)** v0.6.0 [skip ci] - (238eb62) - Jenkins
- **(version)** v0.1.0 [skip ci] - (aae44b9) - Jenkins
-  move dependencies to workspaces - (20229c6) - Kartik Gajendra

- - -

## cac_client-v0.11.0 - 2024-04-24
#### Features
- ready for open source! - (5f7af15) - Kartik
#### Miscellaneous Chores
- **(version)** v0.40.0 [skip ci] - (cdfe3f4) - Jenkins

- - -

## v0.39.1 - 2024-04-24
#### Bug Fixes
-  removed audit log middleware and reduced max db connection pool size to 2 - (bdfdaed) - Saurav Suman
#### Miscellaneous Chores
- **(version)** v0.39.1 [skip ci] - (9cf8f81) - Jenkins

- - -

## v0.39.0 - 2024-04-24
#### Bug Fixes
-  fixed error in client - (5db1ba4) - ankit.mahato
#### Features
-  Add filter support to client - (0cc2e6d) - ankit.mahato
#### Miscellaneous Chores
- **(version)** v0.39.0 [skip ci] - (3405b5d) - Jenkins

- - -

## v0.35.0 - 2024-04-24
#### Bug Fixes
-  empty key filters should return all keys - (de71bb2) - Kartik
#### Documentation
-  add intro doc and features - (85751e0) - Natarajan Kannan
#### Features
-  client interface improvements - (659d384) - Kartik
-  haskell client for superposition - (af6ea75) - Kartik
#### Miscellaneous Chores
- **(version)** v0.35.0 [skip ci] - (233bf7d) - Jenkins

- - -

## v0.32.0 - 2024-04-24
#### Documentation
-  add intro doc and features - (39bd090) - Natarajan Kannan
#### Features
-  haskell client for superposition - (0ce569b) - Kartik
#### Miscellaneous Chores
- **(version)** v0.32.0 [skip ci] - (ae88083) - Jenkins

- - -

## v0.28.0 - 2024-04-24
#### Features
-  Replace merge-strategy option for resolve/eval - (032dc3a) - ayush.jain@juspay.in
#### Miscellaneous Chores
- **(version)** v0.28.0 [skip ci] - (75396e5) - Jenkins

- - -

## v0.24.0 - 2024-04-24
#### Bug Fixes
-  Do not remove keys with null value on merge - (06974f1) - ayush.jain@juspay.in
#### Miscellaneous Chores
- **(version)** v0.24.0 [skip ci] - (372c9de) - Jenkins

- - -

## v0.17.0 - 2024-04-24
#### Features
- working resolve page - (27fbb99) - Kartik Gajendra
#### Miscellaneous Chores
- **(version)** v0.17.0 [skip ci] - (5759a15) - Jenkins

- - -

## v0.15.0 - 2024-04-24
#### Bug Fixes
- cac service to set last_modified header - (8ad81c3) - ankit.mahato
#### Features
- added format check in the JenkinsFile() - (3611ef3) - Saurav Suman
#### Miscellaneous Chores
- **(version)** v0.15.0 [skip ci] - (3afb567) - Jenkins
-  move dependencies to workspaces - (bb87c89) - Kartik Gajendra

- - -

## v0.12.0 - 2024-04-24
#### Features
- multi-tenant support for client libraries - (f524388) - Shubhranshu Sanjeev
#### Miscellaneous Chores
- **(version)** v0.12.0 [skip ci] - (1612123) - Jenkins

- - -

## v0.6.1 - 2024-04-24
#### Bug Fixes
-  patching overrides on default-config instead of merge - (8e311c7) - Ritick Madaan
#### Miscellaneous Chores
- **(version)** v0.6.1 [skip ci] - (5c471b2) - Jenkins

- - -

## v0.6.0 - 2024-04-24
#### Bug Fixes
-  eval param fix - (a9c5b04) - Pratik Mishra
#### Features
-  cac eval return update - (a2fde06) - Pratik Mishra
-  Adding generic eval - (b63c753) - Pratik Mishra
#### Miscellaneous Chores
- **(version)** v0.6.0 [skip ci] - (a5f8757) - Jenkins

- - -

## v0.1.0 - 2024-04-24
#### Features
-  added experimentation client with few fixes - (be0b70f) - Kartik Gajendra
#### Miscellaneous Chores
- **(version)** v0.1.0 [skip ci] - (bfbc1f5) - Jenkins

- - -

## cac_client-v0.10.0 - 2024-04-18
#### Features
- ready for open source! - (b7d36be) - Kartik

- - -

## cac_client-v0.9.1 - 2024-04-17
#### Bug Fixes
- [PICAF-26653] removed audit log middleware and reduced max db connection pool size to 2 - (82022eb) - Saurav Suman

- - -

## cac_client-v0.9.0 - 2024-04-16
#### Bug Fixes
- PICAF-26366 fixed error in client - (d1b1f03) - ankit.mahato
#### Features
- PICAF-26366 Add filter support to client - (f4c12c7) - ankit.mahato

- - -

## cac_client-v0.8.0 - 2024-04-05
#### Bug Fixes
- [PICAF-26101] empty key filters should return all keys - (f9dd889) - Kartik
#### Documentation
- PICAF-25981: add intro doc and features - (64fa30f) - Natarajan Kannan
#### Features
- [PICAF-26101] client interface improvements - (d606cb1) - Kartik
- [PICAF-26126] haskell client for superposition - (651a66d) - Kartik

- - -

## cac_client-v0.7.0 - 2024-03-18
#### Documentation
- PICAF-25981: add intro doc and features - (d09ba53) - Natarajan Kannan
#### Features
- [PICAF-26126] haskell client for superposition - (7106b56) - Kartik

- - -

## cac_client-v0.6.0 - 2024-03-04
#### Features
- PICAF-26185 Replace merge-strategy option for resolve/eval - (453cfb9) - ayush.jain@juspay.in

- - -

## cac_client-v0.5.1 - 2024-02-22
#### Bug Fixes
- PICAF-26157 Do not remove keys with null value on merge - (bd3c196) - ayush.jain@juspay.in

- - -

## cac_client-v0.5.0 - 2024-01-04
#### Features
- working resolve page - (803dfbd) - Kartik Gajendra

- - -

## cac_client-v0.4.0 - 2023-11-11
#### Features
- added format check in the JenkinsFile(PICAF-24813) - (4fdf864) - Saurav Suman
#### Miscellaneous Chores
- [PICAF-24778] move dependencies to workspaces - (38a524f) - Kartik Gajendra

- - -

## cac_client-v0.3.0 - 2023-10-27
#### Features
- multi-tenant support for client libraries - (c603be0) - Shubhranshu Sanjeev

- - -

## cac_client-v0.2.1 - 2023-09-20
#### Bug Fixes
- PICAF-24507 patching overrides on default-config instead of merge - (2c09e32) - Ritick Madaan

- - -

## cac_client-v0.2.0 - 2023-09-12
#### Bug Fixes
- PICAF-24223 eval param fix - (9d4d678) - Pratik Mishra
#### Features
- PICAF-24223 cac eval return update - (d558ddc) - Pratik Mishra
- PICAF-24223 Adding generic eval - (b94ce46) - Pratik Mishra

- - -

## cac_client-v0.1.0 - 2023-09-01
#### Features
- [PICAF-23632] added experimentation client with few fixes - (9a31815) - Kartik Gajendra

- - -

Changelog generated by [cocogitto](https://github.com/cocogitto/cocogitto).