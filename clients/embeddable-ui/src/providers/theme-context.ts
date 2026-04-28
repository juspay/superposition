import { createContext } from "react";
import type { SuperpositionThemeMode, SuperpositionThemeTokens } from "../types";

export interface SuperpositionThemeValue {
  mode: SuperpositionThemeMode;
  resolvedMode: Exclude<SuperpositionThemeMode, "system">;
  tokens?: SuperpositionThemeTokens;
}

export const ThemeContext = createContext<SuperpositionThemeValue | null>(null);
