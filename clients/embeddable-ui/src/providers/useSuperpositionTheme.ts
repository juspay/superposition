import { useContext } from "react";
import { ThemeContext, type SuperpositionThemeValue } from "./theme-context";

export function useSuperpositionTheme(): SuperpositionThemeValue {
  const ctx = useContext(ThemeContext);
  if (!ctx) {
    throw new Error("useSuperpositionTheme must be used within a <ThemeProvider>");
  }
  return ctx;
}
