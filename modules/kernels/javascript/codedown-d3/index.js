"use strict";

// Helpers for drawing D3 charts from a notebook cell.
//
// `render` runs the drawing code in the *browser*, so the chart keeps its interactivity
// (hover, zoom, transitions). The cell's callback is serialized into the HTML output along with
// the d3 bundle and the data, because output HTML is displayed in a sandboxed frame that can't
// reach back into the kernel or fetch a script from a CDN.
//
// `renderStatic` runs the same kind of callback against jsdom here in the kernel and emits a
// plain SVG. It loses interactivity but works in any frontend and survives export.

const fs = require("fs");
const path = require("path");

let counter = 0;
const nextId = () => "codedown-d3-" + process.pid + "-" + ++counter;

// d3's "exports" map only exposes the ESM entry point and a "umd" condition, so the browser
// bundle can't be require.resolve'd by path. Walk up from the entry point to the package root
// instead.
function d3BundleSource() {
  let dir = path.dirname(require.resolve("d3"));
  while (!fs.existsSync(path.join(dir, "package.json"))) {
    const parent = path.dirname(dir);
    if (parent === dir) throw new Error("could not locate the d3 package root");
    dir = parent;
  }
  return fs.readFileSync(path.join(dir, "dist", "d3.min.js"), "utf8");
}

function display(mime, content) {
  // tslab is always present next to us in the environment's node_modules.
  require("tslab").display[mime](content);
}

function renderToHtml(fn, data, options) {
  const opts = options || {};
  const width = opts.width || 640;
  const height = opts.height || 400;
  const id = nextId();

  // The bundle is inlined per output: each output may be displayed in its own frame, so there's
  // no shared window to cache it on. The guard still helps when several outputs share one.
  return [
    '<div id="' + id + '" style="width:' + width + "px;height:" + height + 'px"></div>',
    "<script>",
    "(function() {",
    "  if (typeof window.d3 === 'undefined') {",
    d3BundleSource(),
    "  }",
    "  var el = document.getElementById(" + JSON.stringify(id) + ");",
    "  var data = " + JSON.stringify(data === undefined ? null : data) + ";",
    "  var draw = " + fn.toString() + ";",
    "  draw(window.d3, el, data, { width: " + width + ", height: " + height + " });",
    "})();",
    "</script>",
  ].join("\n");
}

function render(fn, data, options) {
  display("html", renderToHtml(fn, data, options));
}

function renderStaticToSvg(fn, data, options) {
  const opts = options || {};
  const width = opts.width || 640;
  const height = opts.height || 400;

  const { JSDOM } = require("jsdom");
  const d3 = require("d3");

  const dom = new JSDOM("<body></body>");
  const el = dom.window.document.body;
  fn(d3, el, data === undefined ? null : data, { width: width, height: height });

  const svg = el.querySelector("svg");
  if (svg && !svg.getAttribute("xmlns")) {
    svg.setAttribute("xmlns", "http://www.w3.org/2000/svg");
  }
  return el.innerHTML;
}

function renderStatic(fn, data, options) {
  display("html", renderStaticToSvg(fn, data, options));
}

module.exports = {
  render: render,
  renderToHtml: renderToHtml,
  renderStatic: renderStatic,
  renderStaticToSvg: renderStaticToSvg,
};
