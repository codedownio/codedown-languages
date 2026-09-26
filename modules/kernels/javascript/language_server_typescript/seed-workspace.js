#!/usr/bin/env node
"use strict";

// A stdio proxy in front of typescript-language-server.
//
// tsserver resolves modules and @types the way tsc does: from the edited file's directory
// upwards. Notebook cells are written to the user's notebook directory, which knows nothing
// about the environment's node_modules, so without help every import is `any` -- hovering
// `d3.scaleLinear` gives "any" and completion falls back to 4 text matches instead of d3's 577.
//
// The one thing that fixes it is a jsconfig.json (or tsconfig.json, for the TypeScript kernel)
// next to the file. So this proxy watches for the `initialize` request and drops the
// environment's config into each workspace root that doesn't already have one. Everything else
// is passed through byte for byte.

const { spawn } = require("child_process");
const fs = require("fs");
const path = require("path");

const configPath = process.env.CODEDOWN_WORKSPACE_CONFIG;
const configName = process.env.CODEDOWN_WORKSPACE_CONFIG_NAME || "jsconfig.json";
const serverArgs = process.argv.slice(2);

const child = spawn(serverArgs[0], serverArgs.slice(1), {
  stdio: ["pipe", "pipe", "inherit"],
});
child.stdout.pipe(process.stdout);
child.on("exit", (code) => process.exit(code === null ? 1 : code));

function uriToPath(uri) {
  if (typeof uri !== "string" || !uri.startsWith("file://")) return null;
  try {
    return decodeURIComponent(uri.slice("file://".length));
  } catch (e) {
    return null;
  }
}

function seed(dir) {
  if (!configPath || !dir) return;
  try {
    // Never overwrite a configuration that is already there -- the user's own, or the one the
    // other kernel's server seeded.
    const target = path.join(dir, configName);
    if (fs.existsSync(target)) return;
    fs.copyFileSync(configPath, target);
    fs.chmodSync(target, 0o644);
  } catch (e) {
    // A read-only or missing workspace is not fatal; the server still works, just untyped.
  }
}

function handle(message) {
  let parsed;
  try {
    parsed = JSON.parse(message);
  } catch (e) {
    return;
  }
  if (parsed.method !== "initialize" || !parsed.params) return;

  const roots = [];
  for (const folder of parsed.params.workspaceFolders || []) roots.push(uriToPath(folder.uri));
  roots.push(uriToPath(parsed.params.rootUri));
  if (parsed.params.rootPath) roots.push(parsed.params.rootPath);
  for (const root of roots) if (root) seed(root);
}

// Split the client's stream into LSP messages so `initialize` is handled before it reaches the
// server, then forward each message unchanged.
let buffer = Buffer.alloc(0);

process.stdin.on("data", (chunk) => {
  buffer = Buffer.concat([buffer, chunk]);

  for (;;) {
    const headerEnd = buffer.indexOf("\r\n\r\n");
    if (headerEnd === -1) break;

    const headers = buffer.slice(0, headerEnd).toString("ascii");
    const match = /content-length:\s*(\d+)/i.exec(headers);
    if (!match) {
      // Not something we understand; hand the rest over and stop parsing.
      child.stdin.write(buffer);
      buffer = Buffer.alloc(0);
      process.stdin.pipe(child.stdin);
      return;
    }

    const length = parseInt(match[1], 10);
    const start = headerEnd + 4;
    if (buffer.length < start + length) break;

    const body = buffer.slice(start, start + length);
    handle(body.toString("utf8"));
    child.stdin.write(buffer.slice(0, start + length));
    buffer = buffer.slice(start + length);
  }
});

process.stdin.on("end", () => child.stdin.end());
