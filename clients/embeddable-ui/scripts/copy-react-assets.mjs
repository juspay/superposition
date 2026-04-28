import { cpSync, mkdirSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const rootDir = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const vendorDir = resolve(rootDir, "dist", "vendor");

mkdirSync(vendorDir, { recursive: true });

cpSync(
  resolve(rootDir, "node_modules", "react", "umd", "react.production.min.js"),
  resolve(vendorDir, "react.production.min.js"),
);

cpSync(
  resolve(rootDir, "node_modules", "react-dom", "umd", "react-dom.production.min.js"),
  resolve(vendorDir, "react-dom.production.min.js"),
);
