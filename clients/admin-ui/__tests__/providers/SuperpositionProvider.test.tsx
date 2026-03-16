import { describe, it, expect, vi } from "vitest";
import { render, screen } from "@testing-library/react";
import {
  SuperpositionProvider,
  useSuperposition,
} from "../../src/providers/SuperpositionProvider";

function TestConsumer() {
  const { config, client, dimensions, defaultConfigs, overrides, experiments } =
    useSuperposition();
  return (
    <div>
      <span data-testid="host">{config.host}</span>
      <span data-testid="org">{config.orgId}</span>
      <span data-testid="ws">{config.workspace}</span>
      <span data-testid="has-client">{client ? "yes" : "no"}</span>
      <span data-testid="has-dims">{dimensions ? "yes" : "no"}</span>
      <span data-testid="has-configs">{defaultConfigs ? "yes" : "no"}</span>
      <span data-testid="has-overrides">{overrides ? "yes" : "no"}</span>
      <span data-testid="has-experiments">{experiments ? "yes" : "no"}</span>
    </div>
  );
}

describe("SuperpositionProvider", () => {
  it("provides config and API instances to children", () => {
    render(
      <SuperpositionProvider
        config={{
          host: "https://sp.test",
          orgId: "my-org",
          workspace: "prod",
        }}
      >
        <TestConsumer />
      </SuperpositionProvider>,
    );

    expect(screen.getByTestId("host").textContent).toBe("https://sp.test");
    expect(screen.getByTestId("org").textContent).toBe("my-org");
    expect(screen.getByTestId("ws").textContent).toBe("prod");
    expect(screen.getByTestId("has-client").textContent).toBe("yes");
    expect(screen.getByTestId("has-dims").textContent).toBe("yes");
    expect(screen.getByTestId("has-configs").textContent).toBe("yes");
    expect(screen.getByTestId("has-overrides").textContent).toBe("yes");
    expect(screen.getByTestId("has-experiments").textContent).toBe("yes");
  });

  it("throws when used outside provider", () => {
    // Suppress React error boundary output
    const spy = vi.spyOn(console, "error").mockImplementation(() => {});
    expect(() => render(<TestConsumer />)).toThrow(
      "useSuperposition must be used within a <SuperpositionProvider>",
    );
    spy.mockRestore();
  });
});
