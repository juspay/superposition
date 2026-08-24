import react from "@vitejs/plugin-react";
import { svelte } from "@sveltejs/vite-plugin-svelte";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { defineConfig } from "vite";

const demoRoot = fileURLToPath(new URL(".", import.meta.url));

export default defineConfig({
  root: demoRoot,
  plugins: [svelte(), react()],
  resolve: {
    alias: {
      "superposition-embeddable-ui/browser": resolve(
        demoRoot,
        "../src/browser.tsx",
      ),
    },
  },
  server: {
    proxy: {
      "/api": {
        target: "http://localhost:8081",
        changeOrigin: true,
        rewrite: (path) => path.replace(/^\/api/, ""),
      },
    },
  },
});
