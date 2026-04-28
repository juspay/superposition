import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const rootDir = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const viteCli = resolve(rootDir, "node_modules", "vite", "bin", "vite.js");
const features = ["admin", "config-manager", "override-manager", "dimension-manager"];

for (const feature of features) {
  const result = spawnSync(
    process.execPath,
    [viteCli, "build", "--config", "vite.cdn.feature.config.ts"],
    {
      cwd: rootDir,
      stdio: "inherit",
      env: {
        ...process.env,
        SUPERPOSITION_CDN_FEATURE: feature,
      },
    },
  );

  if (result.status !== 0) {
    process.exit(result.status ?? 1);
  }
}
