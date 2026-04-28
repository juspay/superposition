import { fireEvent, render, screen } from "@testing-library/react";
import { describe, expect, it, vi } from "vitest";
import {
  SuperpositionProvider,
  useSuperposition,
} from "../../src/providers/SuperpositionProvider";
import { useSuperpositionTheme } from "../../src/providers/useSuperpositionTheme";

function TestConsumer() {
  const { config, client, dimensions, defaultConfigs, overrides, experiments, scope } =
    useSuperposition();
  const theme = useSuperpositionTheme();
  const host = config.apiBaseUrl;
  return (
    <div>
      <span data-testid="host">{host}</span>
      <span data-testid="org">{config.orgId}</span>
      <span data-testid="ws">{config.workspace}</span>
      <span data-testid="theme-mode">{theme.mode}</span>
      <span data-testid="has-client">{client ? "yes" : "no"}</span>
      <span data-testid="has-dims">{dimensions ? "yes" : "no"}</span>
      <span data-testid="has-configs">{defaultConfigs ? "yes" : "no"}</span>
      <span data-testid="has-overrides">{overrides ? "yes" : "no"}</span>
      <span data-testid="has-experiments">{experiments ? "yes" : "no"}</span>
      <span data-testid="effective-scope">
        {JSON.stringify(scope.effectiveContext ?? null)}
      </span>
    </div>
  );
}

function ScopeConsumer() {
  const { scope } = useSuperposition();

  return (
    <div>
      <span data-testid="host-scope">{JSON.stringify(scope.hostContext ?? null)}</span>
      <span data-testid="boundary-scope">
        {JSON.stringify(scope.boundaryContext ?? null)}
      </span>
      <span data-testid="merged-scope">
        {JSON.stringify(scope.effectiveContext ?? null)}
      </span>
      <button onClick={() => scope.setBoundaryContext({ env: "prod" })}>
        Apply boundary
      </button>
      <button onClick={() => scope.clearBoundaryContext()}>Clear boundary</button>
    </div>
  );
}

describe("SuperpositionProvider", () => {
  it("provides config and API instances to children", () => {
    render(
      <SuperpositionProvider
        config={{
          apiBaseUrl: "https://sp.test",
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
    expect(screen.getByTestId("theme-mode").textContent).toBe("light");
    expect(screen.getByTestId("has-client").textContent).toBe("yes");
    expect(screen.getByTestId("has-dims").textContent).toBe("yes");
    expect(screen.getByTestId("has-configs").textContent).toBe("yes");
    expect(screen.getByTestId("has-overrides").textContent).toBe("yes");
    expect(screen.getByTestId("has-experiments").textContent).toBe("yes");
  });

  it("accepts the embeddable config contract", () => {
    render(
      <SuperpositionProvider
        config={{
          apiBaseUrl: "/api",
          apiBasePath: "/sp",
          orgId: "my-org",
          workspace: "prod",
          routing: { mode: "external", currentFeature: "config" },
        }}
      >
        <TestConsumer />
      </SuperpositionProvider>,
    );

    expect(screen.getByTestId("host").textContent).toBe("/api");
    expect(screen.getByTestId("org").textContent).toBe("my-org");
  });

  it("provides theme mode from config", () => {
    const { container } = render(
      <SuperpositionProvider
        config={{
          apiBaseUrl: "/api",
          orgId: "my-org",
          workspace: "prod",
          theme: {
            mode: "dark",
            colorPrimary: "oklch(0.68 0.18 250)",
            colorDanger: "oklch(0.61 0.2 25)",
            radiusMd: "8px",
          },
        }}
      >
        <TestConsumer />
      </SuperpositionProvider>,
    );

    expect(screen.getByTestId("theme-mode").textContent).toBe("dark");
    const themeRoot = container.querySelector(".sp-ui") as HTMLElement;
    expect(themeRoot.style.getPropertyValue("--sp-color-primary")).toBe(
      "oklch(0.68 0.18 250)",
    );
    expect(themeRoot.style.getPropertyValue("--sp-color-danger")).toBe(
      "oklch(0.61 0.2 25)",
    );
    expect(themeRoot.style.getPropertyValue("--sp-control-radius")).toBe(
      "var(--sp-radius-md)",
    );
    expect(themeRoot.style.getPropertyValue("--sp-radius-md")).toBe("8px");
  });

  it("merges host scope with a top-level boundary filter", () => {
    render(
      <SuperpositionProvider
        config={{
          apiBaseUrl: "/api",
          orgId: "my-org",
          workspace: "prod",
          scope: { context: { region: "in" } },
        }}
      >
        <ScopeConsumer />
      </SuperpositionProvider>,
    );

    expect(screen.getByTestId("host-scope").textContent).toBe('{"region":"in"}');
    expect(screen.getByTestId("boundary-scope").textContent).toBe("null");
    expect(screen.getByTestId("merged-scope").textContent).toBe('{"region":"in"}');

    fireEvent.click(screen.getByText("Apply boundary"));

    expect(screen.getByTestId("boundary-scope").textContent).toBe('{"env":"prod"}');
    expect(screen.getByTestId("merged-scope").textContent).toBe(
      '{"env":"prod","region":"in"}',
    );

    fireEvent.click(screen.getByText("Clear boundary"));
    expect(screen.getByTestId("boundary-scope").textContent).toBe("null");
    expect(screen.getByTestId("merged-scope").textContent).toBe('{"region":"in"}');
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
