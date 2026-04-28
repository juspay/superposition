import { createContext } from "react";
import type { SuperpositionThemeMode, SuperpositionThemeTokens } from "../types";

export interface SuperpositionThemeValue {
  mode: SuperpositionThemeMode;
  tokens?: SuperpositionThemeTokens;
}

export const ThemeContext = createContext<SuperpositionThemeValue | null>(null);
