import { describe, it, expect, vi } from "vitest";
import { render, screen, fireEvent } from "@testing-library/react";
import { Pagination } from "../../src/components/Pagination";

describe("Pagination", () => {
  it("renders nothing for single page", () => {
    const { container } = render(
      <Pagination currentPage={1} totalPages={1} onPageChange={() => {}} />,
    );
    expect(container.innerHTML).toBeFalsy();
  });

  it("renders page buttons", () => {
    render(
      <Pagination currentPage={2} totalPages={5} onPageChange={() => {}} />,
    );

    expect(screen.getByText("1")).toBeDefined();
    expect(screen.getByText("2")).toBeDefined();
    expect(screen.getByText("3")).toBeDefined();
    expect(screen.getByText("Previous")).toBeDefined();
    expect(screen.getByText("Next")).toBeDefined();
  });

  it("calls onPageChange when page clicked", () => {
    const onPageChange = vi.fn();
    render(
      <Pagination currentPage={1} totalPages={5} onPageChange={onPageChange} />,
    );

    fireEvent.click(screen.getByText("3"));
    expect(onPageChange).toHaveBeenCalledWith(3);
  });

  it("calls onPageChange for Next button", () => {
    const onPageChange = vi.fn();
    render(
      <Pagination currentPage={2} totalPages={5} onPageChange={onPageChange} />,
    );

    fireEvent.click(screen.getByText("Next"));
    expect(onPageChange).toHaveBeenCalledWith(3);
  });

  it("disables Previous on first page", () => {
    render(
      <Pagination currentPage={1} totalPages={5} onPageChange={() => {}} />,
    );

    expect(screen.getByText("Previous")).toBeDisabled();
  });

  it("disables Next on last page", () => {
    render(
      <Pagination currentPage={5} totalPages={5} onPageChange={() => {}} />,
    );

    expect(screen.getByText("Next")).toBeDisabled();
  });
});
