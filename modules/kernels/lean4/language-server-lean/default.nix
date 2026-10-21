# `lean --server` is Lean's own language server, the same one VS Code drives.
#
# There's no notebook-language-server proxy here, unlike Go and Haskell. A .lean file is
# just a sequence of commands, so a concatenation of cells is already a valid Lean file and
# the server's line numbers line up with the notebook's -- hence notebook_suffix = ".lean".
# That's the first thing to check if diagnostics ever land on the wrong line.
{ lib
, callPackage
, makeWrapper
, runCommand

, lean4

, attrs
, chosenPackages
, extensions
, kernelName
}:

let
  common = callPackage ../../common.nix {};

  languageServerName = "lean";

  # LEAN_PATH finds the .oleans, which is all elaboration needs. LEAN_SRC_PATH is separate
  # and finds the .lean a module came from: without it textDocument/definition on a name
  # from a package answers with an empty result rather than pointing into the library.
  leanPath = lib.concatStringsSep ":" (map (p: "${p}/.lake/build/lib/lean") chosenPackages);
  leanSrcPath = lib.concatStringsSep ":" (map toString chosenPackages);

  # The server execs `lean` and `lake` for elaboration and writes an elaboration cache under
  # HOME, which it refuses to start without. It runs fine with no Lake project in sight --
  # verified against a bare .lean file in a scratch directory.
  #
  # On this Lean it does need /etc/localtime to exist. Right after `initialize` the watchdog
  # builds a default LSP log filename from Std.Time.ZonedDateTime.now -- it does this even
  # with logging off -- and 4.29.1's Std/Time/Zoned/Database/TZdb.lean hardcodes
  # /etc/localtime with no fallback and no TZ override. Without it the server exits with
  # "Watchdog error: no such file or directory" before elaborating anything.
  #
  # leanprover/lean4#12869 added a TZ/TZDIR override, shipped in 4.35.0, so after the next
  # nixpkgs bump this becomes a wrapper concern rather than a sandbox one: `--set TZ UTC`
  # plus a TZDIR pointing into tzdata's zoneinfo is enough here, verified against 4.35.0 in
  # a sandbox with no /etc. The unconditional timestamp in ofLspLogConfig is still there
  # though, so a Lean with no local timezone at all still loses its language server.
  leanWrapped = runCommand "lean-language-server" {
    nativeBuildInputs = [ makeWrapper ];
  } ''
    mkdir -p $out/bin
    makeWrapper ${lean4}/bin/lean $out/bin/lean-language-server \
      --add-flags --server \
      --suffix PATH : ${lean4}/bin \
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
