# Changelog
All notable changes to this project will be documented in this file. See [conventional commits](https://www.conventionalcommits.org/) for commit guidelines.

- - -
## frontend-v0.7.1 - 2024-05-02
#### Bug Fixes
- fixed function template (#16) - (1a9861c) - Ankit Kumar Mahato

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