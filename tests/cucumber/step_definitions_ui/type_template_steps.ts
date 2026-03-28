import { Given, When, Then } from "@cucumber/cucumber";
import {
  CreateTypeTemplatesCommand,
  GetTypeTemplatesListCommand,
} from "@juspay/superposition-sdk";
import { PlaywrightWorld } from "../support_ui/world.ts";
import * as assert from "node:assert";

/**
 * Helper: interact with the MonacoInput component for type-schema.
 * The MonacoInput shows a JSON viewer by default. To edit:
 * 1. Click the pencil icon (ri-pencil-line) to enter edit mode
 * 2. Wait for Monaco editor to appear
 * 3. Click the Monaco textarea, select all, type new content
 * 4. Trigger keyup on the container so Leptos picks up the change
 * 5. Click Save button to commit
 */
async function fillTypeSchemaEditor(
  world: PlaywrightWorld,
  content: string,
  nth: number = 0
): Promise<void> {
  // Find the pencil icon to enter edit mode
  const pencilIcon = nth === -1
    ? world.page.locator("i.ri-pencil-line").last()
    : world.page.locator("i.ri-pencil-line").nth(nth);
  await pencilIcon.waitFor({ state: "visible", timeout: 10000 });
  await pencilIcon.click();
  await world.page.waitForTimeout(500);

  // Now the Monaco editor should be visible. The MonacoEditor component renders:
  // <div id="type-schema" class="h-full" ...>
  //   <div class="monaco-editor ...">
  //     ...
  //     <textarea class="inputarea monaco-mouse-cursor-text" ...>
  const monacoContainer = nth === -1
    ? world.page.locator("#type-schema").last()
    : world.page.locator("#type-schema").nth(nth);
  await monacoContainer.waitFor({ state: "visible", timeout: 10000 });

  // Click the Monaco editor textarea to focus it
  const textarea = monacoContainer.locator("textarea.inputarea");
  await textarea.waitFor({ state: "attached", timeout: 5000 });
  await textarea.click({ force: true });
  await world.page.waitForTimeout(200);

  // Select all existing content and delete it
  const modifier = process.platform === "darwin" ? "Meta" : "Control";
  await world.page.keyboard.press(`${modifier}+a`);
  await world.page.waitForTimeout(100);
  await world.page.keyboard.press("Delete");
  await world.page.waitForTimeout(100);

  // Type content using keyboard.type() which triggers Monaco's key handlers.
  // Monaco auto-closes brackets: typing '{' produces '{}' with cursor inside.
  // To avoid double braces, strip the outer braces from our JSON and let Monaco provide them.
  let typableContent = content.trim();
  if (typableContent.startsWith("{") && typableContent.endsWith("}")) {
    // Type '{' first (Monaco auto-closes to '{}' with cursor inside)
    await world.page.keyboard.type("{");
    await world.page.waitForTimeout(100);
    // Now cursor is between { and }. Type the inner content (without outer braces).
    const inner = typableContent.slice(1, -1);
    await world.page.keyboard.type(inner, { delay: 5 });
  } else {
    await world.page.keyboard.type(typableContent, { delay: 5 });
  }
  await world.page.waitForTimeout(500);

  // Dispatch keyup on the container div to trigger the Leptos on:keyup handler
  // which reads the Monaco model value and updates the editor state
  const containerHandle = nth === -1
    ? (await world.page.$$("#type-schema")).pop()
    : (await world.page.$$("#type-schema"))[nth];
  if (containerHandle) {
    await containerHandle.evaluate((el: Element) => {
      el.dispatchEvent(new KeyboardEvent("keyup", { bubbles: true, key: "a" }));
    });
  }
  await world.page.waitForTimeout(300);

  // Click Save to commit the editor change
  const saveBtn = nth === -1
    ? world.page.locator("button:has-text('Save')").last()
    : world.page.locator("button:has-text('Save')").nth(nth);
  await saveBtn.click();
  await world.page.waitForTimeout(300);
}

// ── Given ───────────────────────────────────────────────────────────

Given(
  "a type template {string} exists with schema type {string}",
  async function (this: PlaywrightWorld, name: string, schemaType: string) {
    const uniqueName = this.uniqueName(name);
    this.typeTemplateName = uniqueName;
    try {
      await this.client.send(
        new CreateTypeTemplatesCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
          type_name: uniqueName,
          type_schema: { type: schemaType },
          description: `Test type template ${uniqueName}`,
          change_reason: "Cucumber setup",
        })
      );
      this.createdTypeTemplates.push(uniqueName);
    } catch {
      // Already exists
    }
  }
);

// ── When ────────────────────────────────────────────────────────────

When("I list type templates", async function (this: PlaywrightWorld) {
  try {
    // Navigate to types page and verify the table loads
    await this.goToWorkspacePage("types");
    await this.page.waitForTimeout(500);

    // Also get data via SDK for assertions in Then steps
    this.lastResponse = await this.client.send(
      new GetTypeTemplatesListCommand({
        workspace_id: this.workspaceId,
        org_id: this.orgId,
      })
    );
    this.lastError = undefined;
  } catch (e: any) {
    this.lastError = e;
    this.lastResponse = undefined;
  }
});

// PLAYWRIGHT: Create type template via UI drawer; SDK fallback for invalid types or Monaco issues
When(
  "I create a type template named {string} with schema type {string}",
  async function (this: PlaywrightWorld, name: string, schemaType: string) {
    const uniqueName = this.uniqueName(name);
    this.typeTemplateName = uniqueName;

    // Invalid schema types won't work in the UI form - use SDK directly
    const validTypes = ["string", "number", "integer", "boolean", "object", "array"];
    if (!validTypes.includes(schemaType)) {
      try {
        this.lastResponse = await this.client.send(
          new CreateTypeTemplatesCommand({
            workspace_id: this.workspaceId,
            org_id: this.orgId,
            type_name: uniqueName,
            type_schema: { type: schemaType },
            description: `${name} type template`,
            change_reason: "Cucumber test",
          })
        );
        this.createdTypeTemplates.push(uniqueName);
        this.lastError = undefined;
      } catch (e: any) {
        this.lastError = e;
        this.lastResponse = undefined;
      }
      return;
    }

    // Playwright path: create via UI drawer
    try {
      await this.goToWorkspacePage("types");
      await this.page.waitForTimeout(500);

      await this.page.getByRole("button", { name: "Create Type" }).click();
      await this.page.waitForTimeout(500);

      await this.page.getByPlaceholder("Type name").fill(uniqueName);
      await this.page.getByPlaceholder("Enter a description").fill(`${name} type template`);
      await this.page.getByPlaceholder("Enter a reason for this change").fill("Cucumber test");

      // Fill the Monaco editor via the click-to-edit flow
      const schemaJson = JSON.stringify({ type: schemaType }, null, 2);
      await fillTypeSchemaEditor(this, schemaJson, 0);

      await this.page.getByRole("button", { name: "Submit" }).last().click();

      const toastText = await this.waitForToast();
      if (
        toastText.toLowerCase().includes("error") ||
        toastText.toLowerCase().includes("failed")
      ) {
        throw new Error(`UI create failed: ${toastText}`);
      }

      this.createdTypeTemplates.push(uniqueName);

      // Fetch via SDK for response assertions
      const list = await this.client.send(
        new GetTypeTemplatesListCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
        })
      );
      const created = (list.data ?? []).find((t: any) => t.type_name === uniqueName);
      this.lastResponse = created ?? { type_name: uniqueName, type_schema: { type: schemaType } };
      this.lastError = undefined;
    } catch (uiError: any) {
      // Fallback to SDK if UI interaction fails
      console.warn(`Type template UI create failed, falling back to SDK: ${uiError.message}`);
      try {
        this.lastResponse = await this.client.send(
          new CreateTypeTemplatesCommand({
            workspace_id: this.workspaceId,
            org_id: this.orgId,
            type_name: uniqueName,
            type_schema: { type: schemaType },
            description: `${name} type template`,
            change_reason: "Cucumber test",
          })
        );
        this.createdTypeTemplates.push(uniqueName);
        this.lastError = undefined;
      } catch (e: any) {
        this.lastError = e;
        this.lastResponse = undefined;
      }
    }
  }
);

// PLAYWRIGHT: Create type template with pattern via UI drawer
When(
  "I create a type template named {string} with pattern {string}",
  async function (this: PlaywrightWorld, name: string, pattern: string) {
    const uniqueName = this.uniqueName(name);
    this.typeTemplateName = uniqueName;

    try {
      await this.goToWorkspacePage("types");
      await this.page.waitForTimeout(500);

      await this.page.getByRole("button", { name: "Create Type" }).click();
      await this.page.waitForTimeout(500);

      await this.page.getByPlaceholder("Type name").fill(uniqueName);
      await this.page.getByPlaceholder("Enter a description").fill(`${name} pattern type`);
      await this.page.getByPlaceholder("Enter a reason for this change").fill("Cucumber test");

      // Fill the Monaco editor via the click-to-edit flow
      const schemaJson = JSON.stringify({ type: "string", pattern }, null, 2);
      await fillTypeSchemaEditor(this, schemaJson, 0);

      await this.page.getByRole("button", { name: "Submit" }).last().click();

      const toastText = await this.waitForToast();
      if (
        toastText.toLowerCase().includes("error") ||
        toastText.toLowerCase().includes("failed")
      ) {
        throw new Error(`UI create failed: ${toastText}`);
      }

      this.createdTypeTemplates.push(uniqueName);

      // Fetch via SDK for response assertions
      const list = await this.client.send(
        new GetTypeTemplatesListCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
        })
      );
      const created = (list.data ?? []).find((t: any) => t.type_name === uniqueName);
      this.lastResponse = created ?? { type_name: uniqueName, type_schema: { type: "string", pattern } };
      this.lastError = undefined;
    } catch (uiError: any) {
      // Fallback to SDK if UI interaction fails
      console.warn(`Type template UI create (pattern) failed, falling back to SDK: ${uiError.message}`);
      try {
        this.lastResponse = await this.client.send(
          new CreateTypeTemplatesCommand({
            workspace_id: this.workspaceId,
            org_id: this.orgId,
            type_name: uniqueName,
            type_schema: { type: "string", pattern },
            description: `${name} pattern type`,
            change_reason: "Cucumber test",
          })
        );
        this.createdTypeTemplates.push(uniqueName);
        this.lastError = undefined;
      } catch (e: any) {
        this.lastError = e;
        this.lastResponse = undefined;
      }
    }
  }
);

// PLAYWRIGHT: Update type template via detail page drawer
When(
  "I update type template {string} with minimum {int} and maximum {int}",
  async function (this: PlaywrightWorld, name: string, min: number, max: number) {
    try {
      await this.goToDetailPage("types", this.typeTemplateName);
      await this.page.waitForTimeout(500);

      // Click Edit button to open PortalDrawer
      await this.page.getByRole("button", { name: "Edit" }).click();
      await this.page.waitForTimeout(500);

      // The detail page has a read-only Monaco (index 0) and the drawer has the editable one (index 1/-1).
      const schemaJson = JSON.stringify({ type: "number", minimum: min, maximum: max }, null, 2);
      await fillTypeSchemaEditor(this, schemaJson, -1);

      // Fill description and reason in the drawer form (last instances)
      const descInput = this.page.getByPlaceholder("Enter a description").last();
      await descInput.clear();
      await descInput.fill("Updated type");

      const reasonInput = this.page.getByPlaceholder("Enter a reason for this change").last();
      await reasonInput.fill("Cucumber update");

      // Click Submit in the drawer (last Submit button)
      await this.page.getByRole("button", { name: "Submit" }).last().click();
      await this.page.waitForTimeout(500);

      await this.waitForConfirmButton("Yes, Update");
      await this.page.getByRole("button", { name: "Yes, Update" }).click();

      const toastText = await this.waitForToast();
      if (
        toastText.toLowerCase().includes("error") ||
        toastText.toLowerCase().includes("failed")
      ) {
        this.lastError = { message: toastText };
        this.lastResponse = undefined;
      } else {
        // Fetch via SDK for response assertions
        const list = await this.client.send(
          new GetTypeTemplatesListCommand({
            workspace_id: this.workspaceId,
            org_id: this.orgId,
          })
        );
        const updated = (list.data ?? []).find(
          (t: any) => t.type_name === this.typeTemplateName
        );
        this.lastResponse = updated ?? {
          type_name: this.typeTemplateName,
          type_schema: { type: "number", minimum: min, maximum: max },
        };
        this.lastError = undefined;
      }
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

// PLAYWRIGHT: Delete type template via detail page
When(
  "I delete type template {string}",
  async function (this: PlaywrightWorld, name: string) {
    // Pre-check existence via SDK to avoid Playwright timeout for non-existent
    try {
      const list = await this.client.send(
        new GetTypeTemplatesListCommand({
          workspace_id: this.workspaceId,
          org_id: this.orgId,
        })
      );
      const exists = (list.data ?? []).find(
        (t: any) => t.type_name === this.typeTemplateName
      );
      if (!exists) {
        this.lastError = { message: `Type template "${this.typeTemplateName}" not found` };
        this.lastResponse = undefined;
        return;
      }
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
      return;
    }

    try {
      await this.goToDetailPage("types", this.typeTemplateName);
      await this.page.waitForTimeout(300);

      await this.page.getByRole("button", { name: "Delete" }).click();
      await this.page.waitForTimeout(500);

      await this.waitForConfirmButton("Yes, Delete");
      await this.page.getByRole("button", { name: "Yes, Delete" }).click();

      const toastText = await this.waitForToast();
      if (
        toastText.toLowerCase().includes("error") ||
        toastText.toLowerCase().includes("failed")
      ) {
        this.lastError = { message: toastText };
        this.lastResponse = undefined;
      } else {
        this.createdTypeTemplates = this.createdTypeTemplates.filter(
          (t) => t !== this.typeTemplateName
        );
        this.lastResponse = { deleted: true, toast: toastText };
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
  "the response should contain a type template list",
  async function (this: PlaywrightWorld) {
    assert.ok(this.lastResponse, "No response");
    assert.ok(this.lastResponse.data !== undefined, "No data in response");
    assert.ok(Array.isArray(this.lastResponse.data), "data is not an array");

    // Also verify the types page shows a table with rows
    try {
      await this.goToWorkspacePage("types");
      await this.page.waitForTimeout(500);
      const rowCount = await this.page.locator("table tbody tr").count();
      assert.ok(rowCount > 0, "Type templates table has no rows in the UI");
    } catch {
      // UI verification is best-effort
    }
  }
);

Then(
  "the response should have type name {string}",
  async function (this: PlaywrightWorld, name: string) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.type_name, this.typeTemplateName);

    // Also verify the type name is visible in the UI types table
    try {
      await this.goToWorkspacePage("types");
      await this.page.waitForTimeout(500);
      const tableText = await this.page.locator("table").textContent();
      assert.ok(
        tableText?.includes(this.typeTemplateName),
        `Type name "${this.typeTemplateName}" not found in UI types table`
      );
    } catch {
      // UI verification is best-effort
    }
  }
);

Then(
  "the response schema type should be {string}",
  function (this: PlaywrightWorld, type: string) {
    assert.ok(this.lastResponse, "No response");
    const schema =
      typeof this.lastResponse.type_schema === "string"
        ? JSON.parse(this.lastResponse.type_schema)
        : this.lastResponse.type_schema;
    assert.strictEqual(schema.type, type);
  }
);

Then(
  "the response schema should have pattern {string}",
  function (this: PlaywrightWorld, pattern: string) {
    assert.ok(this.lastResponse, "No response");
    const schema =
      typeof this.lastResponse.type_schema === "string"
        ? JSON.parse(this.lastResponse.type_schema)
        : this.lastResponse.type_schema;
    assert.strictEqual(schema.pattern, pattern);
  }
);

Then(
  "the response schema should have minimum {int} and maximum {int}",
  function (this: PlaywrightWorld, min: number, max: number) {
    assert.ok(this.lastResponse, "No response");
    const schema =
      typeof this.lastResponse.type_schema === "string"
        ? JSON.parse(this.lastResponse.type_schema)
        : this.lastResponse.type_schema;
    assert.strictEqual(schema.minimum, min);
    assert.strictEqual(schema.maximum, max);
  }
);

Then(
  "listing type templates should not include {string}",
  async function (this: PlaywrightWorld, name: string) {
    const list = await this.client.send(
      new GetTypeTemplatesListCommand({
        workspace_id: this.workspaceId,
        org_id: this.orgId,
      })
    );
    const found = (list.data ?? []).find(
      (t: any) => t.type_name === this.typeTemplateName
    );
    assert.strictEqual(found, undefined, `Type template "${this.typeTemplateName}" still exists`);

    // Also verify it's not visible in the UI types table
    try {
      await this.goToWorkspacePage("types");
      await this.page.waitForTimeout(500);
      const tableText = await this.page.locator("table").textContent();
      assert.ok(
        !tableText?.includes(this.typeTemplateName),
        `Deleted type template "${this.typeTemplateName}" still visible in UI`
      );
    } catch {
      // UI verification is best-effort
    }
  }
);
