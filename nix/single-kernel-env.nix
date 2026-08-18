# One minimal environment per kernel, each with just that kernel enabled at its default
# settings.
#
# Used by scripts/probe-lsp-capabilities to attribute language servers to kernels
# unambiguously: whatever shows up under lib/codedown/language-servers in one of these
# environments belongs to that kernel. (In a combined environment like `mega` you can't
# always tell, since e.g. the R and R-ark kernels would both claim the same `attrs`.)

{ lib
, makeEnvironment
, kernelNames
}:

let
  # Settings a kernel needs on top of its defaults just to build here. Keep this as small
  # as possible: anything in it means the probe isn't measuring the default configuration.
  overrides = {
    # julia-modules can't resolve a package closure under the default Julia (1.12); the
    # `mega` environment pins the same version for the same reason.
    julia.juliaPackage = "julia_110";
  };

in

lib.genAttrs kernelNames (name: makeEnvironment {
  name = "probe-${name}";
  kernels.${name} = { enable = true; } // (overrides.${name} or {});
})
