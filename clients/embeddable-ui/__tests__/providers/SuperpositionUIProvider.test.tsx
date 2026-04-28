import { fireEvent, render, screen } from "@testing-library/react";
import { describe, expect, it, vi } from "vitest";
import {
  SuperpositionUIProvider,
  useSuperposition,
  useSuperpositionTheme,
} from "../../src/providers/SuperpositionUIProvider";

function TestConsumer() {
  const { config, client, dimensions, defaultConfigs, overrides, scope } =
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

describe("SuperpositionUIProvider", () => {
  it("provides config and API instances to children", () => {
    render(
      <SuperpositionUIProvider
        config={{
          apiBaseUrl: "https://sp.test",
          orgId: "my-org",
          workspace: "prod",
        }}
      >
        <TestConsumer />
      </SuperpositionUIProvider>,
    );

    expect(screen.getByTestId("host").textContent).toBe("https://sp.test");
    expect(screen.getByTestId("org").textContent).toBe("my-org");
    expect(screen.getByTestId("ws").textContent).toBe("prod");
    expect(screen.getByTestId("theme-mode").textContent).toBe("light");
    expect(screen.getByTestId("has-client").textContent).toBe("yes");
    expect(screen.getByTestId("has-dims").textContent).toBe("yes");
    expect(screen.getByTestId("has-configs").textContent).toBe("yes");
    expect(screen.getByTestId("has-overrides").textContent).toBe("yes");
  });

  it("accepts the embeddable config contract", () => {
    render(
      <SuperpositionUIProvider
        config={{
          apiBaseUrl: "/api",
          apiBasePath: "/sp",
          orgId: "my-org",
          workspace: "prod",
          routing: { mode: "external", currentFeature: "config" },
        }}
      >
        <TestConsumer />
      </SuperpositionUIProvider>,
    );

    expect(screen.getByTestId("host").textContent).toBe("/api");
    expect(screen.getByTestId("org").textContent).toBe("my-org");
  });

  it("provides theme mode from config", () => {
    const { container } = render(
      <SuperpositionUIProvider
        config={{
          apiBaseUrl: "/api",
          orgId: "my-org",
          workspace: "prod",
          theme: {
            mode: "dark",
            colors: {
              primary: "oklch(0.68 0.18 250)",
              danger: "oklch(0.61 0.2 25)",
            },
            radius: { md: "8px" },
            button: {
              disabledOpacity: "0.42",
              primary: {
                bgColor: "#0f766e",
                textColor: "#ffffff",
                borderColor: "#115e59",
                shadow: "none",
              },
              secondary: {
                bgColor: "#f8fafc",
                textColor: "#111827",
                borderColor: "#cbd5e1",
              },
              danger: {
                bgColor: "#fee2e2",
                textColor: "#991b1b",
                borderColor: "#fecaca",
              },
            },
            tooltip: {
              bgColor: "#111827",
              textColor: "#f8fafc",
              borderColor: "#334155",
              borderRadius: "6px",
              shadow: "0 8px 18px rgba(15, 23, 42, 0.16)",
              fontSize: "11px",
            },
          },
        }}
      >
        <TestConsumer />
      </SuperpositionUIProvider>,
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
    expect(themeRoot.style.getPropertyValue("--sp-button-primary-bg")).toBe("#0f766e");
    expect(themeRoot.style.getPropertyValue("--sp-button-primary-text")).toBe("#ffffff");
    expect(themeRoot.style.getPropertyValue("--sp-button-primary-border")).toBe(
      "#115e59",
    );
    expect(themeRoot.style.getPropertyValue("--sp-button-primary-shadow")).toBe("none");
    expect(themeRoot.style.getPropertyValue("--sp-button-secondary-bg")).toBe("#f8fafc");
    expect(themeRoot.style.getPropertyValue("--sp-button-secondary-text")).toBe(
      "#111827",
    );
    expect(themeRoot.style.getPropertyValue("--sp-button-secondary-border")).toBe(
      "#cbd5e1",
    );
    expect(themeRoot.style.getPropertyValue("--sp-button-danger-bg")).toBe("#fee2e2");
    expect(themeRoot.style.getPropertyValue("--sp-button-danger-text")).toBe("#991b1b");
    expect(themeRoot.style.getPropertyValue("--sp-button-danger-border")).toBe("#fecaca");
    expect(themeRoot.style.getPropertyValue("--sp-button-disabled-opacity")).toBe("0.42");
    expect(themeRoot.style.getPropertyValue("--sp-tooltip-bg")).toBe("#111827");
    expect(themeRoot.style.getPropertyValue("--sp-tooltip-text")).toBe("#f8fafc");
    expect(themeRoot.style.getPropertyValue("--sp-tooltip-border")).toBe("#334155");
    expect(themeRoot.style.getPropertyValue("--sp-tooltip-radius")).toBe("6px");
    expect(themeRoot.style.getPropertyValue("--sp-tooltip-shadow")).toBe(
      "0 8px 18px rgba(15, 23, 42, 0.16)",
    );
    expect(themeRoot.style.getPropertyValue("--sp-tooltip-font-size")).toBe("11px");
  });

  it("maps structured theme config to CSS variables", () => {
    const { container } = render(
      <SuperpositionUIProvider
        config={{
          apiBaseUrl: "/api",
          orgId: "my-org",
          workspace: "prod",
          theme: {
            colors: {
              primary: "#2563eb",
              panel: "#fbfdff",
              text: "#172033",
            },
            radius: { md: "9px" },
            spacing: { sm: "9px", md: "15px" },
            button: {
              padding: "11px 17px",
              borderRadius: "10px",
              fontSize: "15px",
              fontWeight: "750",
              disabledOpacity: "0.5",
              primary: {
                bgColor: "#7c3aed",
                textColor: "#f8fafc",
                borderColor: "#6d28d9",
                shadow: "none",
              },
              secondary: {
                bgColor: "#f8fafc",
                textColor: "#172033",
                borderColor: "#cbd5e1",
              },
            },
            table: {
              header: {
                bgColor: "#eef2ff",
                textColor: "#1e293b",
                fontSize: "13px",
                fontWeight: "800",
                padding: "12px 16px",
                textTransform: "uppercase",
              },
            },
            form: {
              label: {
                textColor: "#1e293b",
                fontSize: "13px",
                fontWeight: "750",
              },
              removeButton: {
                bgColor: "#9333ea",
                textColor: "#ffffff",
                borderColor: "#7e22ce",
                borderRadius: "8px",
                width: "48px",
                height: "44px",
              },
              helperTextColor: "#64748b",
            },
            icon: { size: "18px", color: "#475569", lock: { color: "#64748b" } },
            search: {
              width: "420px",
              padding: "12px 16px",
              bgColor: "#ffffff",
              textColor: "#172033",
              borderColor: "#dbe3ef",
              borderRadius: "12px",
              placeholderColor: "#94a3b8",
              icon: { size: "19px", color: "#64748b" },
            },
            pageTitle: {
              textColor: "#334155",
              fontSize: "28px",
              fontWeight: "800",
            },
            banner: {
              bgColor: "#fff7ed",
              textColor: "#92400e",
              borderColor: "#fed7aa",
              borderRadius: "4px",
              padding: "14px 16px",
            },
            toast: {
              bgColor: "#ffffff",
              textColor: "#172033",
              borderColor: "#dbe3ef",
              borderRadius: "10px",
              padding: "14px 18px",
              success: { borderColor: "#22c55e" },
            },
            tooltip: {
              bgColor: "#111827",
              textColor: "#f8fafc",
              borderColor: "#334155",
              borderRadius: "7px",
              shadow: "0 6px 18px rgba(15, 23, 42, 0.16)",
              fontSize: "11px",
            },
          },
        }}
      >
        <TestConsumer />
      </SuperpositionUIProvider>,
    );

    const themeRoot = container.querySelector(".sp-ui") as HTMLElement;
    expect(themeRoot.style.getPropertyValue("--sp-color-primary")).toBe("#2563eb");
    expect(themeRoot.style.getPropertyValue("--sp-color-panel")).toBe("#fbfdff");
    expect(themeRoot.style.getPropertyValue("--sp-radius-md")).toBe("9px");
    expect(themeRoot.style.getPropertyValue("--sp-space-sm")).toBe("9px");
    expect(themeRoot.style.getPropertyValue("--sp-button-primary-bg")).toBe("#7c3aed");
    expect(themeRoot.style.getPropertyValue("--sp-button-primary-text")).toBe("#f8fafc");
    expect(themeRoot.style.getPropertyValue("--sp-button-secondary-bg")).toBe("#f8fafc");
    expect(themeRoot.style.getPropertyValue("--sp-button-padding")).toBe("11px 17px");
    expect(themeRoot.style.getPropertyValue("--sp-button-radius")).toBe("10px");
    expect(themeRoot.style.getPropertyValue("--sp-table-header-bg")).toBe("#eef2ff");
    expect(themeRoot.style.getPropertyValue("--sp-table-header-text")).toBe("#1e293b");
    expect(themeRoot.style.getPropertyValue("--sp-table-header-text-transform")).toBe(
      "uppercase",
    );
    expect(themeRoot.style.getPropertyValue("--sp-form-label-color")).toBe("#1e293b");
    expect(themeRoot.style.getPropertyValue("--sp-form-helper-color")).toBe("#64748b");
    expect(themeRoot.style.getPropertyValue("--sp-form-remove-button-bg")).toBe(
      "#9333ea",
    );
    expect(themeRoot.style.getPropertyValue("--sp-form-remove-button-width")).toBe(
      "48px",
    );
    expect(themeRoot.style.getPropertyValue("--sp-icon-size")).toBe("18px");
    expect(themeRoot.style.getPropertyValue("--sp-icon-color")).toBe("#475569");
    expect(themeRoot.style.getPropertyValue("--sp-lock-icon-color")).toBe("#64748b");
    expect(themeRoot.style.getPropertyValue("--sp-search-width")).toBe("420px");
    expect(themeRoot.style.getPropertyValue("--sp-search-border")).toBe("#dbe3ef");
    expect(themeRoot.style.getPropertyValue("--sp-search-placeholder")).toBe("#94a3b8");
    expect(themeRoot.style.getPropertyValue("--sp-page-title-text")).toBe("#334155");
    expect(themeRoot.style.getPropertyValue("--sp-banner-bg")).toBe("#fff7ed");
    expect(themeRoot.style.getPropertyValue("--sp-toast-bg")).toBe("#ffffff");
    expect(themeRoot.style.getPropertyValue("--sp-toast-success-border")).toBe("#22c55e");
    expect(themeRoot.style.getPropertyValue("--sp-tooltip-bg")).toBe("#111827");
    expect(themeRoot.style.getPropertyValue("--sp-tooltip-radius")).toBe("7px");
  });

  it("merges host scope with a top-level boundary filter", () => {
    render(
      <SuperpositionUIProvider
        config={{
          apiBaseUrl: "/api",
          orgId: "my-org",
          workspace: "prod",
          scope: { context: { region: "in" } },
        }}
      >
        <ScopeConsumer />
      </SuperpositionUIProvider>,
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

  it("ignores boundary filters when host scope is strict", () => {
    render(
      <SuperpositionUIProvider
        config={{
          apiBaseUrl: "/api",
          orgId: "my-org",
          workspace: "prod",
          strict: true,
          scope: { context: { region: "in" } },
        }}
      >
        <ScopeConsumer />
      </SuperpositionUIProvider>,
    );

    fireEvent.click(screen.getByText("Apply boundary"));

    expect(screen.getByTestId("boundary-scope").textContent).toBe("null");
    expect(screen.getByTestId("merged-scope").textContent).toBe('{"region":"in"}');
  });

  it("throws when used outside provider", () => {
    // Suppress React error boundary output
    const spy = vi.spyOn(console, "error").mockImplementation(() => {});
    expect(() => render(<TestConsumer />)).toThrow(
      "useSuperposition must be used within a <SuperpositionUIProvider>",
    );
    spy.mockRestore();
  });
});
