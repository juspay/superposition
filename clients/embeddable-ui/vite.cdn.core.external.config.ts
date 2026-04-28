import react from "@vitejs/plugin-react";
import { resolve } from "path";
import { defineConfig } from "vite";

export default defineConfig({
  // Only the plain-browser IIFE build needs this explicit replacement.
  // Package and module builds stay on the normal library pipeline.
  define: {
    "process.env.NODE_ENV": JSON.stringify("production"),
  },
  plugins: [react()],
  build: {
    emptyOutDir: false,
    cssCodeSplit: false,
    lib: {
      entry: resolve(__dirname, "src/browser-core.global.tsx"),
      name: "SuperpositionBrowserCore",
      formats: ["iife"],
      fileName: () => "superposition-browser-core.global.external.js",
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
