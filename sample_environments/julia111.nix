{ codedown
, ...
}:

codedown.makeEnvironment {
  kernels.julia.enable = true;
  kernels.julia.juliaPackage = "julia_111";
  kernels.julia.packages = ["JSON3" "Plots"];

  kernels.julia.lsp.LanguageServer.enable = false;
}
