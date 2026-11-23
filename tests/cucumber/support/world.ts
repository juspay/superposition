import { World, setWorldConstructor, setDefaultTimeout } from "@cucumber/cucumber";
import { SuperpositionClient } from "@juspay/superposition-sdk";

// Set a generous timeout for API calls
setDefaultTimeout(60000);

/**
 * SuperpositionWorld is the shared context for all Cucumber step definitions.
 *
 * Design note: This world class is intentionally abstracted so that the same
 * Gherkin feature files can be reused for both API testing and Web UI testing.
 * For API testing, step definitions call the SDK directly.
 * For UI testing, step definitions would drive a browser instead.
 */
export class SuperpositionWorld extends World {
  // Client and environment
  public client!: SuperpositionClient;
  public baseUrl: string = process.env.SUPERPOSITION_BASE_URL || "http://127.0.0.1:8080";
  public token: string = process.env.SUPERPOSITION_TOKEN || "some-token";

  // Organisation state
  public orgId: string = "";
  public orgName: string = "";
  public createdOrgId: string = "";

  // Workspace state
  public workspaceId: string = "";
  public workspaceName: string = "";

  // Dimension state
  public dimensionName: string = "";
  public createdDimensions: string[] = [];

  // Default config state
  public configKey: string = "";
  public configValue: any = undefined;
  public configSchema: any = undefined;
  public createdConfigs: string[] = [];

  // Config version state
  public configVersionId: string | undefined = undefined;

  // Context state
  public contextId: string = "";
  public createdContextIds: string[] = [];

  // Experiment state
  public experimentId: string = "";
  public experimentVariants: any[] = [];
  public createdExperimentIds: string[] = [];
  public _pendingExpName: string = "";
  public _pendingExpContext: Record<string, any> = {};
  public _pendingExpVariants: any[] = [];

  // Workspace metric definitions currently configured (name + direction)
  public metricDefinitions: { name: string; direction: string }[] = [];

  // Experiment group state
  public experimentGroupId: string = "";
  public groupExpIds: { valid: string; valid2: string; inProgress: string; conflicting: string } = {
    valid: "", valid2: "", inProgress: "", conflicting: "",
  };

  // Function state
  public functionName: string = "";
  public createdFunctions: string[] = [];

  // Variable state
  public variableName: string = "";
  public createdVariables: string[] = [];

  // Secret state
  public secretName: string = "";
  public createdSecrets: string[] = [];

  // Webhook state
  public webhookName: string = "";
  public createdWebhooks: string[] = [];

  // Type template state
  public typeTemplateName: string = "";
  public createdTypeTemplates: string[] = [];

  // Response tracking (for assertions)
  public lastResponse: any = undefined;
  public lastError: any = undefined;

  // Unique suffix for test isolation
  public testRunId: string = Date.now().toString(36);

  constructor(options: any) {
    super(options);
    this.client = new SuperpositionClient({
      endpoint: this.baseUrl,
      token: { token: this.token },
    });
  }

  /** Generate a unique name for test resources */
  uniqueName(prefix: string): string {
    return `${prefix}${this.testRunId}`;
  }

  /** Parse "name:direction,name:direction" into workspace metric definitions.
   *  Direction defaults to "maximize"; an empty name is passed through so the
   *  server's validation can be exercised. */
  parseMetricDefinitions(spec: string): { name: string; direction: string }[] {
    return spec
      .split(",")
      .map((entry) => entry.trim())
      .filter((entry) => entry.length > 0)
      .map((entry) => {
        const [name, direction] = entry.split(":");
        return {
          name: (name ?? "").trim(),
          direction: (direction ?? "maximize").trim() || "maximize",
        };
      });
  }
}

setWorldConstructor(SuperpositionWorld);
