{ callPackage
, lib
, symlinkJoin

, lean4

, settings
, settingsSchema
}:

with { inherit (settings.interface) attrs extensions; };

let
  common = callPackage ../common.nix {};

  kernelName = "lean4";

  displayName = "Lean " + lean4.version;

  repl = callPackage ./repl.nix { inherit lean4; };

  lean4-jupyter = callPackage ./lean4-jupyter.nix { inherit repl; };

  languageServers =
    []
    ++ lib.optionals settings.lsp.lean.enable [(callPackage ./language-server-lean/default.nix {
      inherit attrs extensions kernelName lean4;
    })]
  ;

  # Lean's package manager is Lake, resolving through Reservoir, and what anyone actually
  # wants is Mathlib -- ~5000 files, hours to compile, normally distributed as prebuilt
  # .olean tarballs from Mathlib's CI rather than built from source. Fitting that into Nix is
  # its own project, and it isn't just a package list: a notebook that `import Mathlib` needs
  # those oleans on LEAN_PATH inside the repl process and inside the language server, so it
  # means rebuilding both environments. Ship nothing for now and say so.
  packageOptions = {};
  packageSearch = common.searcher packageOptions;

  kernel = callPackage ./kernel.nix {
    inherit attrs displayName extensions lean4 lean4-jupyter repl;
  };

in

symlinkJoin {
  name = "lean4-${lean4.version}";

  paths = [
    kernel

    lean4
    repl
  ]
  ++ languageServers
  ;

  passthru = {
    meta = lean4.meta // {
      baseName = "lean4";
      inherit displayName settingsSchema;
      inherit (lean4) version;
      icon = ./lean4-logo-64x64.png;
      iconMonochrome = ./lean4-monochrome.svg;
      hasPackages = packageOptions != {};
    };
    inherit packageOptions packageSearch;
    versions = {
      lean4 = lean4.version;
      repl = (import ./repl-version.nix).rev;
    };
    inherit settings settingsSchema;
    inherit (kernel) repls;
    modes = {
      inherit attrs extensions;
      # What lean4_jupyter itself reports in language_info.codemirror_mode, so passthru has
      # to agree with it. There's no Lean mode in stock CodeMirror -- the upstream kernel
      # ships a JupyterLab extension to register one, which isn't usable from here.
      code_mirror_mode = "lean4";
    };
    languageServerNames = map (x: x.languageServerName) languageServers;
    # Lean has no official formatter and `lean --server` advertises no formatting provider,
    # so this stays empty and the Formatting column stays blank.
    formatters = lib.concatMap (x: x.formatters or []) languageServers;
  };
}
