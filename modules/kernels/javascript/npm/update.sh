#!/usr/bin/env bash
# Regenerate package-lock.json and descriptions.json from package.json.
#
# Run this after editing the dependency list in package.json. Needs network; npm only resolves
# here (--package-lock-only), it doesn't install or build anything.

set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")"

echo "Resolving package-lock.json..."
rm -f package-lock.json
npm install --package-lock-only --no-audit --no-fund

# Install scripts are not run in the build (see npmRebuildFlags in ./default.nix), and most of
# them in the wild are funding or telemetry banners, which is fine. What a lockfile genuinely
# cannot express is a package that compiles a native addon at install time. Classify them here
# rather than discovering it at kernel runtime.
#
# Prebuilt-binary packages (@napi-rs/canvas, esbuild, ...) are fine and don't appear here: they
# ship one package per platform and pick with os/cpu fields, which default.nix filters on.
echo "Classifying install scripts..."
node -e '
const lock = require("./package-lock.json");

// zeromq is built by ./zeromq.nix; fsevents is a macOS-only optional dep nothing depends on.
const handled = new Set(["zeromq", "fsevents"]);
const buildsNative = /node-gyp|node-pre-gyp|prebuild|cmake|build-from-source|(^|\s)make(\s|$)/;

(async () => {
  const names = [...new Set(Object.entries(lock.packages)
    .filter(([, p]) => p.hasInstallScript)
    .map(([path]) => path.split("node_modules/").pop()))];

  const native = [], benign = [];
  for (const name of names) {
    if (handled.has(name)) continue;
    const version = Object.entries(lock.packages)
      .find(([path]) => path.endsWith("node_modules/" + name))[1].version;
    const res = await fetch("https://registry.npmjs.org/" + name.replace("/", "%2f") + "/" + version);
    const scripts = res.ok ? ((await res.json()).scripts || {}) : {};
    const text = ["preinstall", "install", "postinstall"].map(k => scripts[k] || "").join(" ");
    (buildsNative.test(text) ? native : benign).push(name + ": " + text.trim());
  }

  for (const b of benign) console.log("  ok (not run): " + b);
  if (native.length) {
    console.error("");
    console.error("These compile a native addon at install time, which a lockfile cannot do.");
    console.error("Either drop them, or give them a Nix derivation the way zeromq.nix does:");
    for (const n of native) console.error("  " + n);
    process.exit(1);
  }
})();
'

echo "Fetching package descriptions..."
node -e '
const fs = require("fs");
const pkg = require("./package.json");
const lock = require("./package-lock.json");

const names = Object.keys(pkg.dependencies).filter(n => n !== "tslab");

(async () => {
  const out = {};
  for (const name of names) {
    const version = lock.packages["node_modules/" + name].version;
    const url = "https://registry.npmjs.org/" + name.replace("/", "%2f") + "/" + version;
    const res = await fetch(url);
    if (!res.ok) {
      console.error("  " + name + ": " + res.status + ", skipping");
      continue;
    }
    const meta = await res.json();
    out[name] = {};
    if (meta.description) out[name].description = meta.description;
    if (meta.homepage) out[name].homepage = meta.homepage;
    if (typeof meta.license === "string") out[name].license = meta.license;
  }
  fs.writeFileSync("descriptions.json", JSON.stringify(out, null, 2) + "\n");
  console.log("Wrote descriptions for " + Object.keys(out).length + " packages.");
})();
'
