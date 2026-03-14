import { Before, After, BeforeAll, AfterAll, Status } from "@cucumber/cucumber";
import { chromium, Browser } from "playwright";
import { PlaywrightWorld } from "./world.ts";

let browser: Browser;

BeforeAll(async function () {
  const headless = process.env.HEADLESS !== "false";
  browser = await chromium.launch({
    headless,
    slowMo: process.env.SLOW_MO ? parseInt(process.env.SLOW_MO) : 0,
  });
});

AfterAll(async function () {
  if (browser) {
    await browser.close();
  }
});

Before(async function (this: PlaywrightWorld) {
  this.browser = browser;
  this.context = await browser.newContext({
    viewport: { width: 1280, height: 720 },
    ignoreHTTPSErrors: true,
  });
  this.page = await this.context.newPage();

  // Inject auth token via localStorage or cookie if needed
  // This depends on how the Leptos app handles authentication
  // For token-based auth, you might set a cookie:
  // await this.context.addCookies([{
  //   name: "auth_token",
  //   value: this.token,
  //   url: this.appUrl,
  // }]);

  // Reset state
  this.lastResponse = undefined;
  this.lastError = undefined;
  this.lastToastText = "";
});

After(async function (this: PlaywrightWorld, scenario) {
  // Take screenshot on failure for debugging
  if (scenario.result?.status === Status.FAILED) {
    const name = scenario.pickle.name.replace(/\s+/g, "-").toLowerCase();
    await this.page.screenshot({
      path: `reports/screenshots/${name}.png`,
      fullPage: true,
    });
  }

  // Cleanup browser context
  if (this.context) {
    await this.context.close();
  }
});
