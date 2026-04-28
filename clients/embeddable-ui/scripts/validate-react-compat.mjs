import { mkdirSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const rootDir = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const reactVersion = process.env.REACT_VERSION ?? "18.3.1";
const reactDomVersion = process.env.REACT_DOM_VERSION ?? reactVersion;
const reactTypesVersion = process.env.REACT_TYPES_VERSION ?? "18.3.12";
const reactDomTypesVersion = process.env.REACT_DOM_TYPES_VERSION ?? "18.3.1";
const tmpDir = process.env.TMPDIR ?? process.env.TEMP ?? "/tmp";
const cacheDir = join(tmpDir, "superposition-embeddable-ui-npm-cache");
const npmExecutable = process.platform === "win32" ? "npm.cmd" : "npm";

mkdirSync(cacheDir, { recursive: true });

console.log(
  `Validating with react@${reactVersion}, react-dom@${reactDomVersion}, @types/react@${reactTypesVersion}, @types/react-dom@${reactDomTypesVersion}`,
);

const runNpm = (args) => {
  const result = spawnSync(npmExecutable, args, {
    cwd: rootDir,
    stdio: "inherit",
    env: process.env,
  });

  if (result.status !== 0) {
    process.exit(result.status ?? 1);
  }
};

runNpm([
  "install",
  "--no-save",
  "--no-package-lock",
  "--cache",
  cacheDir,
  `react@${reactVersion}`,
  `react-dom@${reactDomVersion}`,
  `@types/react@${reactTypesVersion}`,
  `@types/react-dom@${reactDomTypesVersion}`,
]);

runNpm(["run", "typecheck"]);
runNpm(["run", "test"]);
runNpm(["run", "build"]);
runNpm(["pack", "--dry-run", "--cache", cacheDir]);
