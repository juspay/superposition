import React, { cloneElement, isValidElement, useId, useState } from "react";

export interface TooltipProps {
  content: React.ReactNode;
  children: React.ReactElement;
}

export function Tooltip({ content, children }: TooltipProps) {
  const tooltipId = useId();
  const [open, setOpen] = useState(false);

  const trigger = children as React.ReactElement<
    React.HTMLAttributes<HTMLElement> & { "aria-describedby"?: string }
  >;
  const child = isValidElement(trigger)
    ? cloneElement(trigger, {
        "aria-describedby": open ? tooltipId : undefined,
        onBlur: (event: React.FocusEvent<HTMLElement>) => {
          trigger.props.onBlur?.(event);
          setOpen(false);
        },
        onFocus: (event: React.FocusEvent<HTMLElement>) => {
          trigger.props.onFocus?.(event);
          setOpen(true);
        },
        onMouseEnter: (event: React.MouseEvent<HTMLElement>) => {
          trigger.props.onMouseEnter?.(event);
          setOpen(true);
        },
        onMouseLeave: (event: React.MouseEvent<HTMLElement>) => {
          trigger.props.onMouseLeave?.(event);
          setOpen(false);
        },
      })
    : children;

  return (
    <span
      style={{
        position: "relative",
        display: "inline-flex",
        width: "fit-content",
      }}
    >
      {child}
      {open && (
        <span
          id={tooltipId}
          role="tooltip"
          style={{
            position: "absolute",
            zIndex: 20,
            left: "50%",
            bottom: "calc(100% + 8px)",
            transform: "translateX(-50%)",
            width: "max-content",
            maxWidth: 220,
            padding: "6px 8px",
            borderRadius: "var(--sp-tooltip-radius)",
            border: "1px solid var(--sp-tooltip-border)",
            background: "var(--sp-tooltip-bg)",
            color: "var(--sp-tooltip-text)",
            boxShadow: "var(--sp-tooltip-shadow)",
            fontSize: "var(--sp-tooltip-font-size)",
            fontWeight: 600,
            lineHeight: 1.3,
            pointerEvents: "none",
            whiteSpace: "normal",
          }}
        >
          {content}
        </span>
      )}
    </span>
  );
}
