# Changelog
All notable changes to this project will be documented in this file. See [conventional commits](https://www.conventionalcommits.org/) for commit guidelines.

- - -
## frontend-v0.12.1 - 2024-07-17
#### Bug Fixes
- Improve form inputs - (ebd2f57) - Ankit Mahato

- - -

## frontend-v0.12.0 - 2024-07-11
#### Bug Fixes
- modify the default dropdown message for types during edits - (08b4de5) - Kartik
- remove null from type template form when no input is provided - (f0d2990) - Kartik
- Fix context parsing (#145) - (cdd6607) - Ankit Kumar Mahato
#### Features
- added delete override modal (#130) - (a3d498e) - sauraww

- - -

## frontend-v0.11.3 - 2024-07-01
#### Bug Fixes
- handled parsing for multiple possible types (#134) - (9518737) - Shubhranshu Sanjeev

- - -

## frontend-v0.11.2 - 2024-06-27
#### Bug Fixes
- allow positive i32 number for dimension priority in dimension create (#135) - (5e79ad8) - Ayush Jain
- align inputs in override form (#100) - (3a40973) - Datron
- display order of override keys (#99) - (7db3437) - Datron

- - -

## frontend-v0.11.1 - 2024-06-20
#### Bug Fixes
- reset experiment form after closing drawer (#125) - (5b091a9) - Shubhranshu Sanjeev
- display order of override keys - (1745d63) - Shubhranshu Sanjeev

- - -

## frontend-v0.11.0 - 2024-06-11
#### Bug Fixes
- unify actions column for default_config and custom_types - (bdcc902) - Kartik
#### Features
- Support relative path for frontend - (5ed0e46) - ayush.jain@juspay.in
- integrate type templates in forms - (3954d21) - Kartik
- add type templates UI - (c8f2e3a) - Kartik
#### Miscellaneous Chores
- address some clippy lints (#85) - (001b8d4) - Sanchith Hegde
- simplify module structure and follow Rust 2018 edition module naming convention (#86) - (403ea59) - Sanchith Hegde

- - -

## frontend-v0.10.0 - 2024-05-31
#### Features
- Add local storage support (#78) - (9eee7f0) - Ankit Kumar Mahato
- delete support for default config in UI (#63) - (c94fc57) - Shubhranshu Sanjeev

- - -

## frontend-v0.9.0 - 2024-05-29
#### Bug Fixes
- row numbering in paginated table (#72) - (999581a) - Shubhranshu Sanjeev
- creating experiments for default-config (no context) (#38) - (4f6b92c) - Sauravcv98
- reject experiment contexts with `variantIds` (#29) - (092e568) - Shubhranshu Sanjeev
- fixed condition parsing and display (#37) - (023bc6b) - Shubhranshu Sanjeev
- fixed override_flow (#49) - (1130f25) - sauraww
#### Features
- added support for update , create and clone of override - (ebf38bd) - Saurav Suman
#### Miscellaneous Chores
- Add CI check to lint the .sql files based on rules defined in .editorconfig - (16bf460) - Hao

- - -

## frontend-v0.8.1 - 2024-05-10
#### Bug Fixes
- added skeleton loader (#25) - (6d4fb66) - Shubhranshu Sanjeev
- sorted grouped config keys (#40) - (62a712d) - Ankit Kumar Mahato

- - -

## frontend-v0.8.0 - 2024-05-06
#### Bug Fixes
- resolve page failing when any type other than string is used (#21) - (f496634) - Datron
- fixed function template (#16) - (bb2df67) - Ankit Kumar Mahato
- Do not lowercase dmension inputs while resolving (#11) - (8536a84) - Ayush Jain
-  function route fix - (afae0d2) - Pratik Mishra
-  added type for condition - (70914b6) - Saurav CV
-  ui bug fix for contexts - (abe2911) - Saurav CV
- adding min-width settings for table component - (65f71b4) - Kartik
- added drawer, improved UX & single click override addition to variants - (f96d14a) - Shubhranshu Sanjeev
-  transpose columns in single experiment page for variants - (df72896) - Kartik
-  add traffic percentage to experiments table - (c7f4146) - Kartik
-  fix copy of experiment ID - (283b5c3) - Kartik
- using SERVICE_NAME in is_server instead of SERVER_NAME(wrong var name) - (ef028a8) - Shubhranshu Sanjeev
- fixing error message for experiment create and bulk context api - (85cb360) - Jenkins
- refactored experiment page and fixed experiment edit flow - (8af924a) - Shubhranshu Sanjeev
- getting api hostname from env for frontend - (b8dbbe9) - Shubhranshu Sanjeev
- fixed host resolve issue for internal calls in SSR. - (d2189f6) - Shubhranshu Sanjeev
- error resolving pages with internal call to server - (61c3909) - Shubhranshu Sanjeev
- refactored DefaultConfig component + fixed edit flow - (a589c61) - Shubhranshu Sanjeev
- fixed dimension form edit flow + fixed table component CellFormatter to accept move closures - (82a8c2f) - Shubhranshu Sanjeev
- frontend build process - (3036d77) - Shubhranshu Sanjeev
- fixed tenant hydration bug - (11e7705) - Saurav Suman
- fixed ci-test to support multi-tenant setup - (9ad6aa5) - Shubhranshu Sanjeev
- cleanup code - (c98f00d) - Kartik Gajendra
- UI fixes for demo - (e9b3825) - Kartik Gajendra
- frontend multi-tenancy support + config and dimension page - (a632054) - Shubhranshu Sanjeev
- fixed experiment list page feedback - (4fa25e6) - Shubhranshu Sanjeev
- context parsing - (743fdcd) - Kartik Gajendra
- resolve UI bugs - (f4b7912) - Kartik Gajendra
- dimensions page updates - (392cdf6) - ankit.mahato
#### Features
- improved error communication on frontend, with toast component - (51838eb) - Shubhranshu Sanjeev
- Added grouping in default_config page (#9) - (9f4a46a) - Ankit Kumar Mahato
- ready for open source! - (f48db35) - Kartik
-  Add function support in dimension and default config form - (f4542c1) - ankit.mahato
-  added decimal support in context and override form and fixed dimension modal - (a6834b4) - Saurav Suman
-  added support for dynamic json schema in frontend - (16b6556) - Saurav Suman
-  function ui - (7f15a62) - Pratik Mishra
-  refactor resolve page - (98a6aae) - Kartik
-  url click and text wrap fixes - (fa2004d) - Saurav CV
- support for service prefix - (f34705a) - Shubhranshu Sanjeev
- added bool, i64 and decimal in default config form - (81c9717) - Saurav Suman
-  added authentication header for frontend apis - (83ba631) - Saurav Suman
- added between in frontend - (b6043ce) - Akhilesh Bhadauriya
- added validation inside default config form , formatted dates , added disable feature of edit - (cf3beee) - Saurav Suman
- resolve page with unified UI - (f7b759d) - Kartik Gajendra
- working resolve page - (b3096ac) - Kartik Gajendra
- fixed experiment suspense block , added generic button - (a3c63cc) - Saurav Suman
- experiment create form - (0fccb5d) - Shubhranshu Sanjeev
- fixed theme + ui changes + form validation + context validation error handling - (dc8b7e5) - Saurav Suman
- working resolve page - (ed844f1) - Kartik Gajendra
- added state changes in the form - (815b13e) - Saurav Suman
- testing create form - (f6fa783) - Kartik Gajendra
- working experiments page - (35179c4) - Kartik Gajendra
- experiment UI - (e40619f) - Kartik Gajendra
- added default config and override screen - (2e13520) - Saurav Suman
- added default config page - (59ac86b) - Saurav Suman
- working experiments page - (1df7dc2) - Kartik Gajendra
- override and context form - (96dbb81) - Shubhranshu Sanjeev
- dimensions - (8ea5510) - ankit.mahato
- added experiment-list page - (3a51c7c) - Shubhranshu Sanjeev
- experiment UI - (80af3ab) - Kartik Gajendra
- ui for cac and exp - (10932a1) - Shubhranshu Sanjeev
- added frontend crate,combined frontend and backend binaries () - (dcf18a6) - Saurav Suman
#### Miscellaneous Chores
- **(version)** v0.42.0 [skip ci] - (e4bc080) - Superposition Bot
- **(version)** v0.41.0 [skip ci] - (ceefd83) - Superposition Bot
- **(version)** v0.40.0 [skip ci] - (836b769) - Jenkins
- **(version)** v0.38.2 [skip ci] - (577bccb) - Jenkins
- **(version)** v0.38.0 [skip ci] - (80f47d5) - Jenkins
- **(version)** v0.36.0 [skip ci] - (ae95229) - Jenkins
- **(version)** v0.35.0 [skip ci] - (faf984e) - Jenkins
- **(version)** v0.34.0 [skip ci] - (dd35ac6) - Jenkins
- **(version)** v0.33.0 [skip ci] - (c2ba364) - Jenkins
- **(version)** v0.30.1 [skip ci] - (4fc303f) - Jenkins
- **(version)** v0.29.0 [skip ci] - (eeb03e7) - Jenkins
- add PR testing workflows (#6) - (d6bb8d4) - Datron
- formatted code + cleanup - (1658102) - Shubhranshu Sanjeev
- formatted frontend code - (b1ce60c) - Shubhranshu Sanjeev
#### Refactoring
-  refactored service to use new error type and better error handling - (267cda1) - Shubhranshu Sanjeev
- using snake case for component fxn names - (dc773a7) - Shubhranshu Sanjeev
- fixed warnings, added redirection for home page and script for setting up the project - (8a70e77) - Saurav Suman

- - -

## frontend-v0.8.0 - 2024-05-06
#### Bug Fixes
- resolve page failing when any type other than string is used (#21) - (f496634) - Datron
- fixed function template (#16) - (bb2df67) - Ankit Kumar Mahato
- Do not lowercase dmension inputs while resolving (#11) - (8536a84) - Ayush Jain
-  function route fix - (afae0d2) - Pratik Mishra
-  added type for condition - (70914b6) - Saurav CV
-  ui bug fix for contexts - (abe2911) - Saurav CV
- adding min-width settings for table component - (65f71b4) - Kartik
- added drawer, improved UX & single click override addition to variants - (f96d14a) - Shubhranshu Sanjeev
-  transpose columns in single experiment page for variants - (df72896) - Kartik
-  add traffic percentage to experiments table - (c7f4146) - Kartik
-  fix copy of experiment ID - (283b5c3) - Kartik
- using SERVICE_NAME in is_server instead of SERVER_NAME(wrong var name) - (ef028a8) - Shubhranshu Sanjeev
- fixing error message for experiment create and bulk context api - (85cb360) - Jenkins
- refactored experiment page and fixed experiment edit flow - (8af924a) - Shubhranshu Sanjeev
- getting api hostname from env for frontend - (b8dbbe9) - Shubhranshu Sanjeev
- fixed host resolve issue for internal calls in SSR. - (d2189f6) - Shubhranshu Sanjeev
- error resolving pages with internal call to server - (61c3909) - Shubhranshu Sanjeev
- refactored DefaultConfig component + fixed edit flow - (a589c61) - Shubhranshu Sanjeev
- fixed dimension form edit flow + fixed table component CellFormatter to accept move closures - (82a8c2f) - Shubhranshu Sanjeev
- frontend build process - (3036d77) - Shubhranshu Sanjeev
- fixed tenant hydration bug - (11e7705) - Saurav Suman
- fixed ci-test to support multi-tenant setup - (9ad6aa5) - Shubhranshu Sanjeev
- cleanup code - (c98f00d) - Kartik Gajendra
- UI fixes for demo - (e9b3825) - Kartik Gajendra
- frontend multi-tenancy support + config and dimension page - (a632054) - Shubhranshu Sanjeev
- fixed experiment list page feedback - (4fa25e6) - Shubhranshu Sanjeev
- context parsing - (743fdcd) - Kartik Gajendra
- resolve UI bugs - (f4b7912) - Kartik Gajendra
- dimensions page updates - (392cdf6) - ankit.mahato
#### Features
- improved error communication on frontend, with toast component - (51838eb) - Shubhranshu Sanjeev
- Added grouping in default_config page (#9) - (9f4a46a) - Ankit Kumar Mahato
- ready for open source! - (f48db35) - Kartik
-  Add function support in dimension and default config form - (f4542c1) - ankit.mahato
-  added decimal support in context and override form and fixed dimension modal - (a6834b4) - Saurav Suman
-  added support for dynamic json schema in frontend - (16b6556) - Saurav Suman
-  function ui - (7f15a62) - Pratik Mishra
-  refactor resolve page - (98a6aae) - Kartik
-  url click and text wrap fixes - (fa2004d) - Saurav CV
- support for service prefix - (f34705a) - Shubhranshu Sanjeev
- added bool, i64 and decimal in default config form - (81c9717) - Saurav Suman
-  added authentication header for frontend apis - (83ba631) - Saurav Suman
- added between in frontend - (b6043ce) - Akhilesh Bhadauriya
- added validation inside default config form , formatted dates , added disable feature of edit - (cf3beee) - Saurav Suman
- resolve page with unified UI - (f7b759d) - Kartik Gajendra
- working resolve page - (b3096ac) - Kartik Gajendra
- fixed experiment suspense block , added generic button - (a3c63cc) - Saurav Suman
- experiment create form - (0fccb5d) - Shubhranshu Sanjeev
- fixed theme + ui changes + form validation + context validation error handling - (dc8b7e5) - Saurav Suman
- working resolve page - (ed844f1) - Kartik Gajendra
- added state changes in the form - (815b13e) - Saurav Suman
- testing create form - (f6fa783) - Kartik Gajendra
- working experiments page - (35179c4) - Kartik Gajendra
- experiment UI - (e40619f) - Kartik Gajendra
- added default config and override screen - (2e13520) - Saurav Suman
- added default config page - (59ac86b) - Saurav Suman
- working experiments page - (1df7dc2) - Kartik Gajendra
- override and context form - (96dbb81) - Shubhranshu Sanjeev
- dimensions - (8ea5510) - ankit.mahato
- added experiment-list page - (3a51c7c) - Shubhranshu Sanjeev
- experiment UI - (80af3ab) - Kartik Gajendra
- ui for cac and exp - (10932a1) - Shubhranshu Sanjeev
- added frontend crate,combined frontend and backend binaries () - (dcf18a6) - Saurav Suman
#### Miscellaneous Chores
- **(version)** v0.41.0 [skip ci] - (ceefd83) - Superposition Bot
- **(version)** v0.40.0 [skip ci] - (836b769) - Jenkins
- **(version)** v0.38.2 [skip ci] - (577bccb) - Jenkins
- **(version)** v0.38.0 [skip ci] - (80f47d5) - Jenkins
- **(version)** v0.36.0 [skip ci] - (ae95229) - Jenkins
- **(version)** v0.35.0 [skip ci] - (faf984e) - Jenkins
- **(version)** v0.34.0 [skip ci] - (dd35ac6) - Jenkins
- **(version)** v0.33.0 [skip ci] - (c2ba364) - Jenkins
- **(version)** v0.30.1 [skip ci] - (4fc303f) - Jenkins
- **(version)** v0.29.0 [skip ci] - (eeb03e7) - Jenkins
- add PR testing workflows (#6) - (d6bb8d4) - Datron
- formatted code + cleanup - (1658102) - Shubhranshu Sanjeev
- formatted frontend code - (b1ce60c) - Shubhranshu Sanjeev
#### Refactoring
-  refactored service to use new error type and better error handling - (267cda1) - Shubhranshu Sanjeev
- using snake case for component fxn names - (dc773a7) - Shubhranshu Sanjeev
- fixed warnings, added redirection for home page and script for setting up the project - (8a70e77) - Saurav Suman

- - -

## frontend-v0.7.0 - 2024-04-24
#### Features
- ready for open source! - (5f7af15) - Kartik
#### Miscellaneous Chores
- **(version)** v0.40.0 [skip ci] - (cdfe3f4) - Jenkins
- add PR testing workflows (#6) - (6085588) - Datron

- - -

## v0.38.2 - 2024-04-24
#### Bug Fixes
-  function route fix - (6735635) - Pratik Mishra
#### Miscellaneous Chores
- **(version)** v0.38.2 [skip ci] - (64560fb) - Jenkins

- - -

## v0.38.0 - 2024-04-24
#### Features
-  Add function support in dimension and default config form - (59ecc4c) - ankit.mahato
#### Miscellaneous Chores
- **(version)** v0.38.0 [skip ci] - (9208b3e) - Jenkins
#### Refactoring
-  refactored service to use new error type and better error handling - (a99f4af) - Shubhranshu Sanjeev

- - -

## v0.36.0 - 2024-04-24
#### Features
-  added decimal support in context and override form and fixed dimension modal - (c86b89a) - Saurav Suman
#### Miscellaneous Chores
- **(version)** v0.36.0 [skip ci] - (cdc46ce) - Jenkins

- - -

## v0.35.0 - 2024-04-24
#### Features
-  added support for dynamic json schema in frontend - (e5164de) - Saurav Suman
-  function ui - (7e859fc) - Pratik Mishra
#### Miscellaneous Chores
- **(version)** v0.35.0 [skip ci] - (233bf7d) - Jenkins

- - -

## v0.34.0 - 2024-04-24
#### Bug Fixes
-  added type for condition - (fb14036) - Saurav CV
-  ui bug fix for contexts - (7856b95) - Saurav CV
#### Miscellaneous Chores
- **(version)** v0.34.0 [skip ci] - (bd807ec) - Jenkins

- - -

## v0.33.0 - 2024-04-24
#### Features
-  refactor resolve page - (3005cfc) - Kartik
#### Miscellaneous Chores
- **(version)** v0.33.0 [skip ci] - (693a023) - Jenkins

- - -

## v0.30.1 - 2024-04-24
#### Bug Fixes
- adding min-width settings for table component - (6f70186) - Kartik
#### Miscellaneous Chores
- **(version)** v0.30.1 [skip ci] - (fd38192) - Jenkins

- - -

## v0.29.0 - 2024-04-24
#### Bug Fixes
- added drawer, improved UX & single click override addition to variants - (50c8a96) - Shubhranshu Sanjeev
-  transpose columns in single experiment page for variants - (f3bfcc0) - Kartik
-  add traffic percentage to experiments table - (a270dee) - Kartik
-  fix copy of experiment ID - (f039558) - Kartik
- using SERVICE_NAME in is_server instead of SERVER_NAME(wrong var name) - (b018117) - Shubhranshu Sanjeev
- fixing error message for experiment create and bulk context api - (3d64762) - Jenkins
- refactored experiment page and fixed experiment edit flow - (1c678b9) - Shubhranshu Sanjeev
- getting api hostname from env for frontend - (8141696) - Shubhranshu Sanjeev
- fixed host resolve issue for internal calls in SSR. - (5c0ebc3) - Shubhranshu Sanjeev
- error resolving pages with internal call to server - (46ca970) - Shubhranshu Sanjeev
- refactored DefaultConfig component + fixed edit flow - (dfd8abb) - Shubhranshu Sanjeev
- fixed dimension form edit flow + fixed table component CellFormatter to accept move closures - (a297aca) - Shubhranshu Sanjeev
- frontend build process - (c937b68) - Shubhranshu Sanjeev
- fixed tenant hydration bug - (3311c5e) - Saurav Suman
- fixed ci-test to support multi-tenant setup - (cad8e7f) - Shubhranshu Sanjeev
- cleanup code - (00c36a6) - Kartik Gajendra
- UI fixes for demo - (caa9739) - Kartik Gajendra
- frontend multi-tenancy support + config and dimension page - (adafb49) - Shubhranshu Sanjeev
- fixed experiment list page feedback - (41905a6) - Shubhranshu Sanjeev
- context parsing - (ba2d33b) - Kartik Gajendra
- resolve UI bugs - (8b0d0a3) - Kartik Gajendra
- dimensions page updates - (b04d3f5) - ankit.mahato
#### Features
-  url click and text wrap fixes - (6553e30) - Saurav CV
- support for service prefix - (19223be) - Shubhranshu Sanjeev
- added bool, i64 and decimal in default config form - (adad811) - Saurav Suman
-  added authentication header for frontend apis - (e1ab466) - Saurav Suman
- added between in frontend - (4bf9ac8) - Akhilesh Bhadauriya
- added validation inside default config form , formatted dates , added disable feature of edit - (97f7f1b) - Saurav Suman
- resolve page with unified UI - (83d0f03) - Kartik Gajendra
- working resolve page - (27fbb99) - Kartik Gajendra
- fixed experiment suspense block , added generic button - (93a3abd) - Saurav Suman
- experiment create form - (588d15a) - Shubhranshu Sanjeev
- fixed theme + ui changes + form validation + context validation error handling - (6376194) - Saurav Suman
- working resolve page - (c651e4a) - Kartik Gajendra
- added state changes in the form - (3843763) - Saurav Suman
- testing create form - (c697e4d) - Kartik Gajendra
- working experiments page - (ba2eb38) - Kartik Gajendra
- experiment UI - (3ce29f0) - Kartik Gajendra
- added default config and override screen - (27f1236) - Saurav Suman
- added default config page - (c8bcfcc) - Saurav Suman
- working experiments page - (bf94e31) - Kartik Gajendra
- override and context form - (3a6036c) - Shubhranshu Sanjeev
- dimensions - (24b4cbf) - ankit.mahato
- added experiment-list page - (d00db1c) - Shubhranshu Sanjeev
- experiment UI - (1f4bd77) - Kartik Gajendra
- ui for cac and exp - (7e87921) - Shubhranshu Sanjeev
- added frontend crate,combined frontend and backend binaries () - (345b4af) - Saurav Suman
#### Miscellaneous Chores
- **(version)** v0.29.0 [skip ci] - (53a20b5) - Jenkins
- formatted code + cleanup - (ec7aecc) - Shubhranshu Sanjeev
- formatted frontend code - (4db1f16) - Shubhranshu Sanjeev
#### Refactoring
- using snake case for component fxn names - (8a5b1b1) - Shubhranshu Sanjeev
- fixed warnings, added redirection for home page and script for setting up the project - (0ee0618) - Saurav Suman

- - -

## frontend-v0.6.0 - 2024-04-18
#### Features
- ready for open source! - (b7d36be) - Kartik

- - -

## frontend-v0.5.1 - 2024-04-12
#### Bug Fixes
- [PICAF-26529] function route fix - (aba54da) - Pratik Mishra

- - -

## frontend-v0.5.0 - 2024-04-10
#### Features
- PICAF-26529 Add function support in dimension and default config form - (6a942fe) - ankit.mahato
#### Refactoring
- [PICAF-26558] refactored service to use new error type and better error handling - (741f391) - Shubhranshu Sanjeev

- - -

## frontend-v0.4.0 - 2024-04-05
#### Features
- [PICAF-26360] added decimal support in context and override form and fixed dimension modal - (3f1f998) - Saurav Suman

- - -

## frontend-v0.3.0 - 2024-04-05
#### Features
- [PICAF-26522] added support for dynamic json schema in frontend - (b02f08f) - Saurav Suman
- [PICAF-26346] function ui - (9360aca) - Pratik Mishra

- - -

## frontend-v0.2.1 - 2024-03-21
#### Bug Fixes
- PICAF-26324 added type for condition - (3607336) - Saurav CV
- PICAF-26324 ui bug fix for contexts - (7ec15ec) - Saurav CV

- - -

## frontend-v0.2.0 - 2024-03-21
#### Features
- [PICAF-26197] refactor resolve page - (acc763a) - Kartik

- - -

## frontend-v0.1.1 - 2024-03-07
#### Bug Fixes
- adding min-width settings for table component - (0fcd0c1) - Kartik

- - -

## frontend-v0.1.0 - 2024-03-06
#### Bug Fixes
- added drawer, improved UX & single click override addition to variants - (14a1ead) - Shubhranshu Sanjeev
- [PICAF-26199] transpose columns in single experiment page for variants - (a1a8ac8) - Kartik
- [PICAF-26196] add traffic percentage to experiments table - (5fb0221) - Kartik
- [PICAF-26195] fix copy of experiment ID - (37e4c24) - Kartik
- using SERVICE_NAME in is_server instead of SERVER_NAME(wrong var name) - (efe97f0) - Shubhranshu Sanjeev
- fixing error message for experiment create and bulk context api - (bc0d7be) - Jenkins
- refactored experiment page and fixed experiment edit flow - (b153486) - Shubhranshu Sanjeev
- getting api hostname from env for frontend - (837899d) - Shubhranshu Sanjeev
- fixed host resolve issue for internal calls in SSR. - (3cc9d6e) - Shubhranshu Sanjeev
- error resolving pages with internal call to server - (084d08b) - Shubhranshu Sanjeev
- refactored DefaultConfig component + fixed edit flow - (f2d38cc) - Shubhranshu Sanjeev
- fixed dimension form edit flow + fixed table component CellFormatter to accept move closures - (9c3a364) - Shubhranshu Sanjeev
- frontend build process - (cbdad01) - Shubhranshu Sanjeev
- fixed tenant hydration bug - (cf0e633) - Saurav Suman
- fixed ci-test to support multi-tenant setup - (916b75d) - Shubhranshu Sanjeev
- cleanup code - (4820f31) - Kartik Gajendra
- UI fixes for demo - (4927766) - Kartik Gajendra
- frontend multi-tenancy support + config and dimension page - (a1689a1) - Shubhranshu Sanjeev
- fixed experiment list page feedback - (f406264) - Shubhranshu Sanjeev
- context parsing - (d46ca42) - Kartik Gajendra
- resolve UI bugs - (98695a8) - Kartik Gajendra
- dimensions page updates - (5220b36) - ankit.mahato
#### Features
- PICAF-26266 url click and text wrap fixes - (643c54d) - Saurav CV
- support for service prefix - (a2915b4) - Shubhranshu Sanjeev
- added bool, i64 and decimal in default config form - (fca1ca6) - Saurav Suman
- [PICAF-25817] added authentication header for frontend apis - (3f90592) - Saurav Suman
- added between in frontend - (0eb60e5) - Akhilesh Bhadauriya
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
- added frontend crate,combined frontend and backend binaries (PICAF-24540) - (ee084ba) - Saurav Suman
#### Miscellaneous Chores
- formatted code + cleanup - (6d4874b) - Shubhranshu Sanjeev
- formatted frontend code - (70f873f) - Shubhranshu Sanjeev
#### Refactoring
- using snake case for component fxn names - (19e9aca) - Shubhranshu Sanjeev
- fixed warnings, added redirection for home page and script for setting up the project - (6b21fb9) - Saurav Suman

- - -

Changelog generated by [cocogitto](https://github.com/cocogitto/cocogitto).