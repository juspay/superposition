import type { ConfirmInput, SuperpositionUiAdapters } from "../types";

function shouldLogUiAdapterWarnings(): boolean {
  const runtime = globalThis as typeof globalThis & {
    process?: {
      env?: {
        NODE_ENV?: string;
      };
    };
  };

  return runtime.process?.env?.NODE_ENV !== "production";
}

export async function confirmAction(
  ui: SuperpositionUiAdapters | undefined,
  input: ConfirmInput,
  fallback?: (input: ConfirmInput) => Promise<boolean> | boolean,
): Promise<boolean> {
  if (ui?.confirm) {
    try {
      return await ui.confirm(input);
    } catch (error) {
      if (shouldLogUiAdapterWarnings()) {
        console.error("[Superposition] confirm adapter threw:", error);
      }

      return false;
    }
  }

  if (fallback) {
    return await fallback(input);
  }

  if (typeof window !== "undefined" && typeof window.confirm === "function") {
    if (shouldLogUiAdapterWarnings()) {
      console.warn(
        "[Superposition] No `confirm` UI adapter provided; falling back to window.confirm(). Provide a `ui.confirm` adapter for a better UX.",
      );
    }

    const message = input.description
      ? `${input.title}\n\n${input.description}`
      : input.title;

    return window.confirm(message);
  }

  return false;
}
