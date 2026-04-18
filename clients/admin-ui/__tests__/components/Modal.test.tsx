import { describe, it, expect, vi } from "vitest";
import { render, screen, fireEvent } from "@testing-library/react";
import { Modal } from "../../src/components/Modal";

describe("Modal", () => {
  it("renders nothing when closed", () => {
    const { container } = render(
      <Modal open={false} onClose={() => {}} title="Test">
        Content
      </Modal>,
    );
    expect(container.innerHTML).toBe("");
  });

  it("renders content when open", () => {
    render(
      <Modal open={true} onClose={() => {}} title="Test Modal">
        Modal content here
      </Modal>,
    );

    expect(screen.getByText("Test Modal")).toBeDefined();
    expect(screen.getByText("Modal content here")).toBeDefined();
  });

  it("calls onClose when X button clicked", () => {
    const onClose = vi.fn();
    render(
      <Modal open={true} onClose={onClose} title="Test">
        Content
      </Modal>,
    );

    fireEvent.click(screen.getByLabelText("Close"));
    expect(onClose).toHaveBeenCalledOnce();
  });

  it("calls onClose when Escape is pressed", () => {
    const onClose = vi.fn();
    render(
      <Modal open={true} onClose={onClose} title="Test">
        Content
      </Modal>,
    );

    fireEvent.keyDown(document, { key: "Escape" });
    expect(onClose).toHaveBeenCalledOnce();
  });

  it("renders footer when provided", () => {
    render(
      <Modal
        open={true}
        onClose={() => {}}
        title="Test"
        footer={<button>Save</button>}
      >
        Content
      </Modal>,
    );

    expect(screen.getByText("Save")).toBeDefined();
  });

  it("calls onClose when overlay clicked", () => {
    const onClose = vi.fn();
    render(
      <Modal open={true} onClose={onClose} title="Test">
        Content
      </Modal>,
    );

    // Click overlay (the outer div), not the dialog content
    const dialog = screen.getByRole("dialog");
    fireEvent.click(dialog.parentElement!);
    expect(onClose).toHaveBeenCalledOnce();
  });

  it("does not call onClose when dialog content clicked", () => {
    const onClose = vi.fn();
    render(
      <Modal open={true} onClose={onClose} title="Test">
        Content
      </Modal>,
    );

    fireEvent.click(screen.getByRole("dialog"));
    expect(onClose).not.toHaveBeenCalled();
  });
});
