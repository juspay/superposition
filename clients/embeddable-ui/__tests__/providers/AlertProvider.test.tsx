import { describe, it, expect, vi } from "vitest";
import { render, screen, act } from "@testing-library/react";
import { AlertProvider, useAlerts } from "../../src/providers/AlertProvider";
import { SuperpositionUIProvider } from "../../src/providers/SuperpositionUIProvider";

function TestAlerts() {
  const { alerts, addAlert, removeAlert } = useAlerts();
  return (
    <div>
      <button onClick={() => addAlert("success", "Test alert")}>Add</button>
      <span data-testid="count">{alerts.length}</span>
      {alerts.map((a) => (
        <div key={a.id} data-testid={`alert-${a.id}`}>
          {a.type}: {a.message}
          <button onClick={() => removeAlert(a.id)}>Remove</button>
        </div>
      ))}
    </div>
  );
}

function TestConfirm() {
  const { confirmAction } = useAlerts();
  return (
    <button
      onClick={() =>
        void confirmAction({
          title: "Delete it?",
          description: "This will remove the record.",
          confirmLabel: "Delete",
        })
      }
    >
      Confirm
    </button>
  );
}

describe("AlertProvider", () => {
  it("adds and displays alerts", async () => {
    render(
      <AlertProvider>
        <TestAlerts />
      </AlertProvider>,
    );

    expect(screen.getByTestId("count").textContent).toBe("0");

    act(() => {
      screen.getByText("Add").click();
    });

    expect(screen.getByTestId("count").textContent).toBe("1");
  });

  it("removes alerts manually", () => {
    render(
      <AlertProvider>
        <TestAlerts />
      </AlertProvider>,
    );

    act(() => {
      screen.getByText("Add").click();
    });
    expect(screen.getByTestId("count").textContent).toBe("1");

    act(() => {
      screen.getByText("Remove").click();
    });
    expect(screen.getByTestId("count").textContent).toBe("0");
  });

  it("auto-removes alerts after timeout", () => {
    vi.useFakeTimers();

    render(
      <AlertProvider>
        <TestAlerts />
      </AlertProvider>,
    );

    act(() => {
      screen.getByText("Add").click();
    });
    expect(screen.getByTestId("count").textContent).toBe("1");

    act(() => {
      vi.advanceTimersByTime(5100);
    });
    expect(screen.getByTestId("count").textContent).toBe("0");

    vi.useRealTimers();
  });

  it("uses the host modal renderer for fallback confirmations", async () => {
    const renderModal = vi.fn(({ title, children, footer }) => (
      <section data-testid="host-confirm">
        <h1>{title}</h1>
        {children}
        <footer>{footer}</footer>
      </section>
    ));

    render(
      <SuperpositionUIProvider
        config={{
          apiBaseUrl: "/api",
          orgId: "my-org",
          workspace: "prod",
          ui: { renderModal },
        }}
      >
        <AlertProvider>
          <TestConfirm />
        </AlertProvider>
      </SuperpositionUIProvider>,
    );

    await act(async () => {
      screen.getByText("Confirm").click();
    });

    expect(screen.getByTestId("host-confirm")).toBeDefined();
    expect(screen.getByText("Delete it?")).toBeDefined();
    expect(screen.getByText("This will remove the record.")).toBeDefined();
    expect(renderModal).toHaveBeenCalled();
  });

  it("throws when used outside provider", () => {
    const spy = vi.spyOn(console, "error").mockImplementation(() => {});
    expect(() => render(<TestAlerts />)).toThrow(
      "useAlerts must be used within an <AlertProvider>",
    );
    spy.mockRestore();
  });
});
