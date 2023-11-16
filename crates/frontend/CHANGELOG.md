# Changelog
All notable changes to this project will be documented in this file. See [conventional commits](https://www.conventionalcommits.org/) for commit guidelines.

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