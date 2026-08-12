/**
 * FileDataSource honours if_modified_since via the file's last-modified time: an unchanged file
 * returns NotModified (the 304 equivalent) rather than re-reading/re-parsing, and an edited file
 * returns fresh data. Mirrors Python's test_file_if_modified.py.
 *
 * Needs the native binding (parsing a changed file goes through the FFI parser), but no live server.
 *
 * Run standalone (from clients/javascript/provider-sdk-tests): node unit/test-file-if-modified.mjs
 */

import assert from "node:assert";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { FileDataSource } from "superposition-provider";

const TOML = `
[default-configs]
currency = { value = "Rupee", schema = { type = "string" } }
[dimensions]
city = { position = 1, schema = { type = "string" }, type = "REGULAR" }
[[overrides]]
_context_ = { city = "Boston" }
currency = "Dollar"
`;

function withTempFile(name, contents) {
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), "sp-file-test-"));
    const file = path.join(dir, name);
    fs.writeFileSync(file, contents);
    return { dir, file };
}

async function test_unchanged_file_is_not_modified() {
    const { file } = withTempFile("config.toml", TOML);
    const source = new FileDataSource(file);

    const first = await source.fetchConfig();
    const fetchedAt = first.getData().fetchedAt;

    // Nothing changed since the read, so a conditional fetch is NotModified.
    const response = await source.fetchConfig(fetchedAt);
    assert.strictEqual(response.isNotModified(), true, "unchanged file should be NotModified");
    assert.strictEqual(response.getData(), null);

    await source.close();
}

async function test_modified_file_returns_fresh_data() {
    const { file } = withTempFile("config.toml", TOML);
    const source = new FileDataSource(file);

    const first = await source.fetchConfig();
    const fetchedAt = first.getData().fetchedAt;

    // Rewrite with a strictly later mtime; the next conditional fetch must re-read.
    fs.writeFileSync(file, TOML.replace('"Rupee"', '"Yen"'));
    const later = new Date(fetchedAt.getTime() + 2000);
    fs.utimesSync(file, later, later);

    const response = await source.fetchConfig(fetchedAt);
    assert.strictEqual(response.isNotModified(), false, "an edited file must return fresh data");
    const configJson = JSON.stringify(response.getData().data.default_configs);
    assert.ok(configJson.includes("Yen"), `expected fresh 'Yen' value, got ${configJson}`);

    await source.close();
}

const CASES = [test_unchanged_file_is_not_modified, test_modified_file_returns_fresh_data];

for (const c of CASES) {
    try {
        await c();
        console.log(`ok  ${c.name}`);
    } catch (e) {
        console.error(`FAIL ${c.name}:`, e && e.stack ? e.stack : e);
        process.exitCode = 1;
    }
}
if (!process.exitCode) {
    console.log(`\n${CASES.length} passed`);
}
