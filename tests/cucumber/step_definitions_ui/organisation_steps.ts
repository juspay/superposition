import { Given, When, Then } from "@cucumber/cucumber";
import {
  CreateOrganisationCommand,
  ListOrganisationCommand,
  GetOrganisationCommand,
  UpdateOrganisationCommand,
} from "@juspay/superposition-sdk";
import { PlaywrightWorld } from "../support_ui/world.ts";
import * as assert from "node:assert";

// ── Given ───────────────────────────────────────────────────────────

Given(
  "an organisation exists with name {string} and admin email {string}",
  async function (this: PlaywrightWorld, name: string, email: string) {
    try {
      const response = await this.client.send(
        new CreateOrganisationCommand({ admin_email: email, name })
      );
      this.createdOrgId = response.id ?? "";
      this.orgName = name;
    } catch (e: any) {
      // May already exist, try to find it
      const list = await this.client.send(new ListOrganisationCommand({}));
      const existing = list.data?.find((o) => o.name === name);
      if (existing) {
        this.createdOrgId = existing.id ?? "";
        this.orgName = name;
      } else {
        throw e;
      }
    }
  }
);

// ── When: Create ────────────────────────────────────────────────────

When(
  "I create an organisation with name {string} and admin email {string}",
  async function (this: PlaywrightWorld, name: string, email: string) {
    try {
      this.lastResponse = await this.client.send(
        new CreateOrganisationCommand({ admin_email: email, name })
      );
      this.createdOrgId = this.lastResponse.id ?? "";
      this.orgName = name;
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

// ── When: Get ───────────────────────────────────────────────────────

When(
  "I get the organisation by its ID",
  async function (this: PlaywrightWorld) {
    try {
      this.lastResponse = await this.client.send(
        new GetOrganisationCommand({ id: this.createdOrgId })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I get an organisation with ID {string}",
  async function (this: PlaywrightWorld, id: string) {
    try {
      this.lastResponse = await this.client.send(
        new GetOrganisationCommand({ id })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

// ── When: List ──────────────────────────────────────────────────────

When("I list all organisations", async function (this: PlaywrightWorld) {
  try {
    this.lastResponse = await this.client.send(
      new ListOrganisationCommand({})
    );
    this.lastError = undefined;
  } catch (e: any) {
    this.lastError = e;
    this.lastResponse = undefined;
  }
});

When(
  "I list organisations with count {int} and page {int}",
  async function (this: PlaywrightWorld, count: number, page: number) {
    try {
      this.lastResponse = await this.client.send(
        new ListOrganisationCommand({ count, page })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

// ── When: Update ────────────────────────────────────────────────────

When(
  "I update the organisation's admin email to {string}",
  async function (this: PlaywrightWorld, email: string) {
    try {
      this.lastResponse = await this.client.send(
        new UpdateOrganisationCommand({ id: this.createdOrgId, admin_email: email })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

When(
  "I update organisation {string} admin email to {string}",
  async function (this: PlaywrightWorld, id: string, email: string) {
    try {
      this.lastResponse = await this.client.send(
        new UpdateOrganisationCommand({ id, admin_email: email })
      );
      this.lastError = undefined;
    } catch (e: any) {
      this.lastError = e;
      this.lastResponse = undefined;
    }
  }
);

// ── Then ────────────────────────────────────────────────────────────

Then(
  "the response should have name {string}",
  function (this: PlaywrightWorld, name: string) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.name, name);
  }
);

Then(
  "the response should have admin email {string}",
  function (this: PlaywrightWorld, email: string) {
    assert.ok(this.lastResponse, "No response");
    assert.strictEqual(this.lastResponse.admin_email, email);
  }
);

Then(
  "the list should contain the created organisation",
  function (this: PlaywrightWorld) {
    const data = this.lastResponse?.data;
    assert.ok(Array.isArray(data), "Response is not a list");
    const found = data.find((o: any) => o.id === this.createdOrgId);
    assert.ok(found, `Organisation ${this.createdOrgId} not found in list`);
  }
);

Then(
  "getting the organisation by ID should show admin email {string}",
  async function (this: PlaywrightWorld, email: string) {
    const response = await this.client.send(
      new GetOrganisationCommand({ id: this.createdOrgId })
    );
    assert.strictEqual(response.admin_email, email);
  }
);
