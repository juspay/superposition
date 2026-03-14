export default {
  requireModule: ["ts-node/esm"],
  require: ["step_definitions/**/*.ts", "support/**/*.ts"],
  format: ["progress", "html:reports/cucumber-report.html"],
  formatOptions: { snippetInterface: "async-await" },
  publishQuiet: true,
};
