# No proxy, unlike Go and Haskell: a concatenation of cells is already a valid .lean file,
# so the server's lines are the notebook's. Hence notebook_suffix = ".lean".
{ lib
, callPackage
, makeWrapper
, runCommand
, tzdata

, lean4

, attrs
, chosenPackages
, extensions
, kernelName
}:

let
  common = callPackage ../../common.nix {};

  languageServerName = "lean";

  # LEAN_PATH gets the oleans; LEAN_SRC_PATH is what makes jump-to-definition into a
  # package resolve rather than come back empty.
  leanPath = lib.concatStringsSep ":" (map (p: "${p}/.lake/build/lib/lean") chosenPackages);
  leanSrcPath = lib.concatStringsSep ":" (map toString chosenPackages);

  # Needs a writable HOME. The server reads the local timezone at startup and exits if
  # there's no /etc/localtime, unless TZ and TZDIR both point at a real zone.
  leanWrapped = runCommand "lean-language-server" {
    nativeBuildInputs = [ makeWrapper ];
  } ''
    mkdir -p $out/bin
    makeWrapper ${lean4}/bin/lean $out/bin/lean-language-server \
      --add-flags --server \
      --suffix PATH : ${lean4}/bin \
      --set-default TZ UTC \
      --set-default TZDIR ${tzdata}/share/zoneinfo \
      ${lib.optionalString (chosenPackages != []) ''
        --prefix LEAN_PATH : ${leanPath} \
        --prefix LEAN_SRC_PATH : ${leanSrcPath} \
      ''} --run 'export HOME=''${HOME:-$(mktemp -d)}'
  '';

  passthru = {
    inherit languageServerName;
    # Lean has no official formatter and the server advertises no formatting provider.
    formatters = [];
  };

in

common.writeTextDirWithMetaAndPassthru lean4.meta passthru "lib/codedown/language-servers/lean-${kernelName}-lean.yaml" (lib.generators.toYAML {} [{
  name = languageServerName;
  version = lean4.version;
  display_name = "Lean";
  description = "The Lean 4 language server";
  icon = ../lean4-logo-64x64.png;
  icon_monochrome = ../lean4-monochrome.svg;
  inherit attrs extensions;
  notebook_suffix = ".lean";
  kernel_name = kernelName;
  type = "stream";
  primary = true;
  args = [ "${leanWrapped}/bin/lean-language-server" ];
  env = {};
  language_id = "lean4";
}])
