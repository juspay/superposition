/**
 * Cucumber profiles for API and UI testing.
 *
 * Both profiles run the SAME Gherkin feature files from features/.
 * The only difference is which step definitions and support files are loaded:
 *
 *   npm run test:api  → step_definitions/ + support/    (drives the SDK)
 *   npm run test:ui   → step_definitions_ui/ + support_ui/ (drives Playwright)
 */
export default function () {
  return {
    api: {
      import: ["step_definitions/**/*.ts", "support/**/*.ts"],
      format: ["progress", "html:reports/api-report.html"],
      formatOptions: { snippetInterface: "async-await" },
    },
    ui: {
      import: ["step_definitions_ui/**/*.ts", "support_ui/**/*.ts"],
      format: ["progress", "html:reports/ui-report.html"],
      formatOptions: { snippetInterface: "async-await" },
    },
  };
}
