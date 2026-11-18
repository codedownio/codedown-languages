# Fails if two kernels, at their default settings, claim the same attr: a code fence like
# ```{r} must name exactly one kernel.

{ lib
, runCommand

, kernels
}:

let
  claims = lib.concatLists (lib.mapAttrsToList (name: kernel: map (attr: { inherit attr name; }) kernel.modes.attrs) kernels);

  clashes = lib.filterAttrs (_: cs: lib.length cs > 1) (lib.groupBy (c: c.attr) claims);

  report = lib.concatStringsSep "\n" (lib.mapAttrsToList (attr: cs: "  ${attr}: ${lib.concatMapStringsSep ", " (c: c.name) cs}") clashes);

in

runCommand "check-kernel-attrs" {} (
  if clashes == {}
  then "touch $out"
  else ''
    echo "These attrs are claimed by more than one kernel:"
    echo ${lib.escapeShellArg report}
    exit 1
  ''
)
