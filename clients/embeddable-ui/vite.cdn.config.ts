import react from "@vitejs/plugin-react";
import { resolve } from "path";
import { defineConfig } from "vite";

export default defineConfig({
  plugins: [react()],
  build: {
    emptyOutDir: false,
    cssCodeSplit: false,
    lib: {
      entry: resolve(__dirname, "src/browser.tsx"),
      name: "SuperpositionEmbeddableUI",
      formats: ["iife"],
      fileName: () => "superposition-embeddable-ui.global.js",
    },
  },
});
