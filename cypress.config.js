const { defineConfig } = require('cypress')

module.exports = defineConfig({
  e2e: {
    baseUrl: 'http://localhost:8080',
    specPattern: 'cypress/integration/**/*.spec.js',
    supportFile: false,
    defaultCommandTimeout: 10000,
    viewportWidth: 1280,
    viewportHeight: 720,
    video: false,
    screenshotOnRunFailure: true,
    experimentalRunAllSpecs: true,
    retries: {
      runMode: 2,
      openMode: 0,
    },
  },
});