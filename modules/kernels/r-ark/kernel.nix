{ callPackage

, ark
, R
, rLibsSite

, attrs
, extensions
, version
}:

let
  common = callPackage ../common.nix {};

in

common.makeJupyterKernel {
  r-ark = {
    displayName = "R (Ark)";
    argv = [
      "${ark}/bin/ark"
      "--connection_file"
      "{connection_file}"
      "--session-mode"
      "notebook"
    ];
    language = "r";

    logo32 = null;
    logo64 = ./r-logo-64x64.png;

    env = {
      # Ark embeds R via libR, which it locates through `R_HOME`. Point it at
      # the R we built against, and expose the selected R packages via
      # `R_LIBS_SITE` (this is all `rWrapper` does — set the library search
      # path — so we replicate it directly for ark's embedded session).
      R_HOME = "${R}/lib/R";
      R_LIBS_SITE = rLibsSite;

      # Quiet ark's default INFO-to-stderr logging (a full message struct per
      # cell), and silence the per-cell "UI comm is absent during dispatch"
      # warning that fires because a plain Jupyter frontend never opens the
      # Positron-only `positron.ui` comm. EnvFilter applies the most specific
      # directive, so the rest of ark stays at `warn`.
      RUST_LOG = "ark=warn,ark::console::console_comm=error";
    };

    metadata = {
      codedown = {
        inherit attrs extensions;

        language_version = version;

        priority = 1;
      };
    };
  };
}
