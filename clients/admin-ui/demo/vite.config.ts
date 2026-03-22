import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";

// Demo-only vite config — proxies /api to the local Superposition server
// so the browser never makes cross-origin requests.
export default defineConfig({
  plugins: [react()],
  server: {
    proxy: {
      "/api": {
        target: "http://localhost:8080",
        changeOrigin: true,
        rewrite: (path) => path.replace(/^\/api/, ""),
      },
    },
  },
});
