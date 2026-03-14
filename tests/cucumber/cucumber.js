/**
 * Cucumber profiles for API and UI testing.
 *
 * Both profiles run the SAME Gherkin feature files from features/.
 * The only difference is which step definitions and support files are loaded:
 *
 *   - api: loads step_definitions/ + support/    → drives the SDK directly
 *   - ui:  loads step_definitions_ui/ + support_ui/ → drives a Playwright browser
 */
export default {
  api: {
    requireModule: ["ts-node/esm"],
    require: ["step_definitions/**/*.ts", "support/**/*.ts"],
    format: ["progress", "html:reports/api-report.html"],
    formatOptions: { snippetInterface: "async-await" },
    publishQuiet: true,
  },
  ui: {
    requireModule: ["ts-node/esm"],
    require: ["step_definitions_ui/**/*.ts", "support_ui/**/*.ts"],
    format: ["progress", "html:reports/ui-report.html"],
    formatOptions: { snippetInterface: "async-await" },
    publishQuiet: true,
  },
};
