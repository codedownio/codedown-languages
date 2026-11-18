# One minimal environment per kernel, each with just that kernel enabled at its default
# settings.
#
# Used by scripts/probe-lsp-capabilities to attribute language servers to kernels
# unambiguously: whatever shows up under lib/codedown/language-servers in one of these
# environments belongs to that kernel. (In a combined environment like `mega` you can't
# always tell which kernel a server came from.)

{ lib
, makeEnvironment
, kernelNames
}:

let
  # Settings a kernel needs on top of its defaults just to build here. Keep this as small
  # as possible: anything in it means the probe isn't measuring the default configuration.
  overrides = { };

in

lib.genAttrs kernelNames (name: makeEnvironment {
  name = "probe-${name}";
  kernels.${name} = { enable = true; } // (overrides.${name} or {});
})
