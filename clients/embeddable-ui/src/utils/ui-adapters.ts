import type { SuperpositionUiAdapters } from "../types";

export async function requestConfirmation(
  ui: SuperpositionUiAdapters | undefined,
  input: {
    title: string;
    description?: string;
    confirmLabel?: string;
    cancelLabel?: string;
  },
): Promise<boolean> {
  if (ui?.confirm) {
    return ui.confirm(input);
  }

  const message = input.description
    ? `${input.title}\n\n${input.description}`
    : input.title;
  return globalThis.confirm(message);
}
