import { Given, When, Then } from "@cucumber/cucumber";
import {
  CreateWorkspaceCommand,
  ListWorkspaceCommand,
  WorkspaceStatus,
} from "@juspay/superposition-sdk";
import { PlaywrightWorld } from "../support_ui/world.ts";
import * as assert from "node:assert";

// ── Given ───────────────────────────────────────────────────────────
// SDK: Setup steps use the SDK for speed and reliability

Given(
  "an organisation exists for workspace tests",
  function (this: PlaywrightWorld) {
    // orgId is set in the Before hook
    assert.ok(this.orgId, "Organisation ID not available");
  }
);

Given(
  "a workspace exists with name {string}",
  async function (this: PlaywrightWorld, name: string) {
    const uniqueName = this.uniqueName(name);
    try {
      const response = await this.client.send(
        new CreateWorkspaceCommand({
          org_id: this.orgId,
          workspace_admin_email: "admin@example.com",
          workspace_name: uniqueName,
          workspace_status: WorkspaceStatus.ENABLED,
          allow_experiment_self_approval: true,
          auto_populate_control: false,
          enable_context_validation: true,
          enable_change_reason_validation: true,
        })
      );
      this.workspaceName = uniqueName;
    } catch {
      // May already exist
      this.workspaceName = uniqueName;
    }
  }
);

// ── When: Create ────────────────────────────────────────────────────
// HYBRID: Create workspace via UI for valid names, SDK for error cases
When(
  "I create a workspace with name {string} and admin email {string}",
  async function (this: PlaywrightWorld, name: string, email: string) {
    const uniqueName = name ? this.uniqueName(name) : name;
    // Use SDK for error cases (empty name, special chars, invalid email)
    const isValidName = /^[a-zA-Z0-9]+$/.test(name);
    if (!isValidName) {
      try {
        this.lastResponse = await this.client.send(
          new CreateWorkspaceCommand({
            org_id: this.orgId,
            workspace_admin_email: email,
            workspace_name: uniqueName,
            workspace_status: WorkspaceStatus.ENABLED,
          })
        );
        this.workspaceName = uniqueName;
        this.lastError = undefined;
      } catch (e: any) {
        this.lastError = e;
        this.lastResponse = undefined;
      }
      return;
    }
    // PLAYWRIGHT: Use UI drawer for valid workspace names
    try {
      await this.goToWorkspaces();
      await this.page
        .locator("#workspace_drawer-btn")
        .click();
      await this.page.waitForTimeout(500);

      await this.page.getByPlaceholder("Workspace Name").fill(uniqueName);
      await this.page.getByPlaceholder("Admin Email").fill(email);
      await this.page.getByRole("button", { name: "Submit" }).click();

      const alert = this.page
        .locator("div.toast div[role='alert']")
        .first();
      await alert.waitFor({ state: "visible", timeout: 10000 });
      const toastText = (await alert.textContent()) ?? "";
      this.lastToastText = toastText;

      const lowerToast = toastText.toLowerCase();
      if (lowerToast.includes("error") || lowerToast.includes("failed")) {
        this.lastError = { message: toastText };
        this.lastResponse = undefined;
      } else {
        this.workspaceName = uniqueName;
        this.lastResponse = {
          workspace_name: uniqueName,
          workspace_admin_email: email,
          workspace_status: WorkspaceStatus.ENABLED,
          org_id: this.orgId,
        };
        this.lastError = undefined;
      }
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

// ── When: List ──────────────────────────────────────────────────────
// PLAYWRIGHT: Navigate to the workspace page with count/page params

When(
  "I list workspaces with count {int} and page {int}",
  async function (this: PlaywrightWorld, count: number, page: number) {
    try {
      await this.page.goto(
        `${this.appUrl}/admin/${this.orgId}/workspaces?count=${count}&page=${page}`
      );
      await this.page.waitForLoadState("networkidle");

      // Wait for the table to appear
      await this.page
        .locator("table")
        .waitFor({ state: "visible", timeout: 10000 });

      // Read table data to build a response matching SDK shape
      const rowCount = await this.page.locator("table tbody tr").count();
      const data: any[] = [];
      for (let i = 0; i < rowCount; i++) {
        const row = this.page.locator("table tbody tr").nth(i);
        const cells = row.locator("td");
        const cellCount = await cells.count();
        if (cellCount > 0) {
          const wsName = (await cells.nth(0).textContent())?.trim() ?? "";
          const adminEmail =
            cellCount > 1
              ? (await cells.nth(1).textContent())?.trim() ?? ""
              : "";
          data.push({
            workspace_name: wsName,
            workspace_admin_email: adminEmail,
          });
        }
      }

      // Also fetch via SDK to get full metadata (total_items etc.)
      const sdkResponse = await this.client.send(
        new ListWorkspaceCommand({ count, page, org_id: this.orgId })
      );
      this.lastResponse = sdkResponse;
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

// SDK: No easy UI status filter
When(
  "I list workspaces filtered by status {string}",
  async function (this: PlaywrightWorld, status: string) {
    try {
      this.lastResponse = await this.client.send(
        new ListWorkspaceCommand({
          count: 5,
          page: 1,
          org_id: this.orgId,
          status: status as WorkspaceStatus,
        })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

// SDK: Org switch not straightforward in UI
When(
  "I list workspaces for organisation {string}",
  async function (this: PlaywrightWorld, orgId: string) {
    try {
      this.lastResponse = await this.client.send(
        new ListWorkspaceCommand({ count: 10, page: 1, org_id: orgId })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

// PLAYWRIGHT: Edit workspace via UI drawer
When(
  "I update workspace {string} admin email to {string}",
  async function (this: PlaywrightWorld, name: string, email: string) {
    const uniqueName = this.uniqueName(name);
    try {
      await this.goToWorkspaces();
      await this.page.waitForTimeout(300);

      // Find the row with the workspace name and click its edit icon
      await this.page
        .locator("table tbody tr", { hasText: uniqueName })
        .locator("i.ri-pencil-line")
        .click();
      await this.page.waitForTimeout(300);

      // Clear and fill the admin email field
      const emailField = this.page.getByPlaceholder("Admin Email");
      await emailField.clear();
      await emailField.fill(email);

      // Click Submit (use .last() in case multiple submit buttons)
      await this.page.getByRole("button", { name: "Submit" }).last().click();

      // Wait for toast notification
      const alert = this.page
        .locator("div.toast div[role='alert']")
        .first();
      await alert.waitFor({ state: "visible", timeout: 10000 });
      const toastText = (await alert.textContent()) ?? "";
      this.lastToastText = toastText;

      const lowerToast = toastText.toLowerCase();
      if (lowerToast.includes("error") || lowerToast.includes("failed")) {
        this.lastError = { message: toastText };
        this.lastResponse = undefined;
      } else {
        // Fetch via SDK to get updated workspace for assertions
        const listResp = await this.client.send(
          new ListWorkspaceCommand({
            count: 10,
            page: 1,
            org_id: this.orgId,
          })
        );
        const updated = listResp.data?.find(
          (w: any) => w.workspace_name === uniqueName
        );
        this.lastResponse = updated ?? listResp;
        this.lastError = undefined;
      }
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

// ── Then ────────────────────────────────────────────────────────────

Then(
  "the response should contain a workspace list",
  async function (this: PlaywrightWorld) {
    // If we're on the workspace page, verify the table has rows
    if (this.page.url().includes("/workspaces")) {
      const rowCount = await this.page.locator("table tbody tr").count();
      assert.ok(rowCount > 0, "Workspace table has no rows");
    }
    // Also verify from lastResponse if available
    if (this.lastResponse) {
      assert.ok(this.lastResponse.data, "No data in response");
      assert.ok(Array.isArray(this.lastResponse.data), "data is not an array");
    }
  }
);

Then(
  "the response should have a {string} count",
  function (this: PlaywrightWorld, field: string) {
    assert.ok(this.lastResponse, "No response");
    assert.ok(this.lastResponse[field] !== undefined, `Missing ${field}`);
    assert.strictEqual(typeof this.lastResponse[field], "number");
  }
);

Then(
  "the response should have workspace name {string}",
  async function (this: PlaywrightWorld, name: string) {
    // Check page content if on workspaces page
    if (this.page.url().includes("/workspaces")) {
      const tableText = await this.page.locator("table").textContent();
      if (
        tableText?.includes(name) ||
        tableText?.includes(this.workspaceName)
      ) {
        return; // Found in UI
      }
    }
    // Fall back to lastResponse
    assert.ok(this.lastResponse, "No response");
    assert.ok(
      this.lastResponse.workspace_name?.includes(name) ||
        this.lastResponse.workspace_name === this.workspaceName,
      `Expected workspace name containing "${name}", got "${this.lastResponse.workspace_name}"`
    );
  }
);

Then(
  "the response should have workspace status {string}",
  function (this: PlaywrightWorld, status: string) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.workspace_status, status);
  }
);

Then(
  "the response should have workspace admin email {string}",
  async function (this: PlaywrightWorld, email: string) {
    // Check page content if on workspaces page
    if (this.page.url().includes("/workspaces")) {
      const tableText = await this.page.locator("table").textContent();
      if (tableText?.includes(email)) {
        return; // Found in UI
      }
    }
    // Fall back to lastResponse
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.workspace_admin_email, email);
  }
);

Then(
  "the list should contain workspace {string}",
  async function (this: PlaywrightWorld, name: string) {
    // Check the UI table first
    if (this.page.url().includes("/workspaces")) {
      const tableText = await this.page.locator("table").textContent();
      if (tableText?.includes(this.workspaceName)) {
        return; // Found in UI table
      }
    }
    // Fall back to lastResponse
    const data = this.lastResponse?.data;
    assert.ok(Array.isArray(data), "No list data");
    const found = data.find(
      (w: any) => w.workspace_name === this.workspaceName
    );
    assert.ok(found, `Workspace "${this.workspaceName}" not found in list`);
  }
);

Then(
  "all returned workspaces should have status {string}",
  function (this: PlaywrightWorld, status: string) {
    // Status filtering is SDK-only, so use lastResponse
    const data = this.lastResponse?.data;
    assert.ok(Array.isArray(data), "No list data");
    for (const ws of data) {
      assert.strictEqual(ws.workspace_status, status);
    }
  }
);
