import { describe, it, expect, vi } from "vitest";
import { render, screen, act } from "@testing-library/react";
import { AlertProvider, useAlerts } from "../../src/providers/AlertProvider";

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

  it("throws when used outside provider", () => {
    const spy = vi.spyOn(console, "error").mockImplementation(() => {});
    expect(() => render(<TestAlerts />)).toThrow(
      "useAlerts must be used within an <AlertProvider>",
    );
    spy.mockRestore();
  });
});
