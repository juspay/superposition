import react from "@vitejs/plugin-react";
import { resolve } from "path";
import { defineConfig } from "vite";

const featureGlobals = {
  admin: {
    entry: "src/browser/admin.global.tsx",
    name: "SuperpositionAdminUI",
    fileName: "superposition-admin.global.external.js",
  },
  "config-manager": {
    entry: "src/browser/config-manager.global.tsx",
    name: "SuperpositionConfigManagerUI",
    fileName: "superposition-config-manager.global.external.js",
  },
  "override-manager": {
    entry: "src/browser/override-manager.global.tsx",
    name: "SuperpositionOverrideManagerUI",
    fileName: "superposition-override-manager.global.external.js",
  },
  "dimension-manager": {
    entry: "src/browser/dimension-manager.global.tsx",
    name: "SuperpositionDimensionManagerUI",
    fileName: "superposition-dimension-manager.global.external.js",
  },
} as const;

const selectedFeature = process.env.SUPERPOSITION_CDN_FEATURE as
  | keyof typeof featureGlobals
  | undefined;

if (!selectedFeature || !featureGlobals[selectedFeature]) {
  throw new Error(
    "SUPERPOSITION_CDN_FEATURE must be one of: admin, config-manager, override-manager, dimension-manager",
  );
}

const feature = featureGlobals[selectedFeature];

export default defineConfig({
  plugins: [react()],
  build: {
    emptyOutDir: false,
    cssCodeSplit: false,
    lib: {
      entry: resolve(__dirname, feature.entry),
      name: feature.name,
      formats: ["iife"],
      fileName: () => feature.fileName,
    },
    rollupOptions: {
      external: ["react", "react-dom", "react-dom/client"],
      output: {
        globals: {
          react: "React",
          "react-dom": "ReactDOM",
          "react-dom/client": "ReactDOM",
        },
      },
    },
  },
});