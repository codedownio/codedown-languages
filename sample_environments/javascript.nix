{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "javascript";

  kernels.javascript.enable = true;
  kernels.javascript.packages = [
    "d3"
    "jsdom"
    "@types/d3"
    "vega-lite"
    "arquero"
    "simple-statistics"
    "@napi-rs/canvas"
    "chart.js"
  ];
}
