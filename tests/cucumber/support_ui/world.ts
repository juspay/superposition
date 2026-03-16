import { World, setWorldConstructor, setDefaultTimeout } from "@cucumber/cucumber";
import { Browser, BrowserContext, Page } from "playwright";
import { SuperpositionClient } from "@juspay/superposition-sdk";

// UI tests need more time for page loads, animations, etc.
setDefaultTimeout(120000);

/**
 * PlaywrightWorld is the shared context for UI-based Cucumber step definitions.
 *
 * It mirrors the same state properties as the API SuperpositionWorld so that
 * the same Gherkin feature files work for both profiles. The difference is
 * that this world holds a Playwright browser/page instead of an SDK client.
 */
export class PlaywrightWorld extends World {
  // ── SDK client ──────────────────────────────────────────────────
  public client!: SuperpositionClient;

  // ── Playwright ────────────────────────────────────────────────────
  public browser!: Browser;
  public context!: BrowserContext;
  public page!: Page;

  // ── Environment ───────────────────────────────────────────────────
  public appUrl: string = process.env.SUPERPOSITION_UI_URL || "http://localhost:8080";
  public baseUrl: string = process.env.SUPERPOSITION_BASE_URL || "http://127.0.0.1:8080";
  public token: string = process.env.SUPERPOSITION_TOKEN || "some-token";

  // ── Organisation state ────────────────────────────────────────────
  public orgId: string = "";
  public orgName: string = "";
  public createdOrgId: string = "";

  // ── Workspace state ───────────────────────────────────────────────
  public workspaceId: string = "";
  public workspaceName: string = "";

  // ── Dimension state ───────────────────────────────────────────────
  public dimensionName: string = "";
  public createdDimensions: string[] = [];

  // ── Default config state ──────────────────────────────────────────
  public configKey: string = "";
  public configValue: any = undefined;
  public configSchema: any = undefined;
  public createdConfigs: string[] = [];

  // ── Config version state ──────────────────────────────────────────
  public configVersionId: string | undefined = undefined;

  // ── Context state ─────────────────────────────────────────────────
  public contextId: string = "";
  public createdContextIds: string[] = [];

  // ── Experiment state ──────────────────────────────────────────────
  public experimentId: string = "";
  public experimentVariants: any[] = [];
  public createdExperimentIds: string[] = [];

  // ── Experiment group state ────────────────────────────────────────
  public experimentGroupId: string = "";

  // ── Function state ────────────────────────────────────────────────
  public functionName: string = "";
  public createdFunctions: string[] = [];

  // ── Variable state ────────────────────────────────────────────────
  public variableName: string = "";
  public createdVariables: string[] = [];

  // ── Secret state ──────────────────────────────────────────────────
  public secretName: string = "";
  public createdSecrets: string[] = [];

  // ── Type template state ───────────────────────────────────────────
  public typeTemplateName: string = "";
  public createdTypeTemplates: string[] = [];

  // ── Response / error tracking ─────────────────────────────────────
  public lastResponse: any = undefined;
  public lastError: any = undefined;

  // ── Toast / alert tracking (UI-specific) ──────────────────────────
  public lastToastText: string = "";
  public lastAlertType: string = "";

  // ── Unique suffix for test isolation ──────────────────────────────
  public testRunId: string = Date.now().toString(36);

  constructor(options: any) {
    super(options);
    this.client = new SuperpositionClient({
      endpoint: this.baseUrl,
      token: { token: this.token },
    });
  }

  uniqueName(prefix: string): string {
    return `${prefix}${this.testRunId}`;
  }

  // ── Navigation helpers ────────────────────────────────────────────

  /** Navigate to the organisations page */
  async goToOrganisations(): Promise<void> {
    await this.page.goto(`${this.appUrl}/admin/organisations`);
    await this.page.waitForLoadState("networkidle");
  }

  /** Navigate to a specific org's workspace list */
  async goToWorkspaces(): Promise<void> {
    await this.page.goto(`${this.appUrl}/admin/${this.orgId}/workspaces`);
    await this.page.waitForLoadState("networkidle");
  }

  /** Navigate to a workspace page (e.g. "dimensions", "variables", "experiments") */
  async goToWorkspacePage(section: string): Promise<void> {
    await this.page.goto(
      `${this.appUrl}/admin/${this.orgId}/${this.workspaceId}/${section}`
    );
    await this.page.waitForLoadState("networkidle");
  }

  // ── UI interaction helpers ────────────────────────────────────────

  /** Open a drawer by clicking its trigger button */
  async openDrawer(drawerId: string): Promise<void> {
    await this.page.locator(`#${drawerId}-btn`).click();
    await this.page.waitForTimeout(300); // wait for drawer animation
  }

  /** Close the currently open drawer */
  async closeDrawer(drawerId: string): Promise<void> {
    await this.page.locator(`label[for="${drawerId}"]`).click();
    await this.page.waitForTimeout(300);
  }

  /** Fill a form input by its label text */
  async fillByLabel(label: string, value: string): Promise<void> {
    await this.page.getByLabel(label).fill(value);
  }

  /** Fill a form input by its placeholder text */
  async fillByPlaceholder(placeholder: string, value: string): Promise<void> {
    await this.page.getByPlaceholder(placeholder).fill(value);
  }

  /** Click a button by its visible text */
  async clickButton(text: string): Promise<void> {
    await this.page.getByRole("button", { name: text }).click();
  }

  /** Wait for a toast/alert and capture its text */
  async waitForToast(): Promise<string> {
    const alert = this.page.locator("div[role='alert']").first();
    await alert.waitFor({ state: "visible", timeout: 10000 });
    this.lastToastText = (await alert.textContent()) ?? "";
    return this.lastToastText;
  }

  /** Wait for a success toast */
  async expectSuccessToast(): Promise<void> {
    const toast = await this.waitForToast();
    this.lastAlertType = "success";
    this.lastResponse = { toast };
    this.lastError = undefined;
  }

  /** Wait for an error toast */
  async expectErrorToast(): Promise<void> {
    const toast = await this.waitForToast();
    this.lastAlertType = "error";
    this.lastError = { message: toast };
    this.lastResponse = undefined;
  }

  /** Check if an element with given text exists in a table */
  async tableContainsText(text: string): Promise<boolean> {
    const table = this.page.locator("table");
    return (await table.textContent())?.includes(text) ?? false;
  }

  /** Get the count of table rows (excluding header) */
  async tableRowCount(): Promise<number> {
    return this.page.locator("table tbody tr").count();
  }

  /** Fill Monaco editor content */
  async fillMonacoEditor(editorId: string, content: string): Promise<void> {
    // Monaco editors need special handling - click to focus, then use keyboard
    const editor = this.page.locator(`#${editorId} .monaco-editor`);
    await editor.click();
    // Select all and replace
    await this.page.keyboard.press("Control+a");
    await this.page.keyboard.type(content, { delay: 10 });
  }

  /** Select an option from a dropdown by text */
  async selectDropdownOption(dropdownText: string, optionText: string): Promise<void> {
    await this.page.locator(`.dropdown:has-text("${dropdownText}")`).click();
    await this.page.locator(`.dropdown-content >> text="${optionText}"`).click();
  }
}

setWorldConstructor(PlaywrightWorld);
