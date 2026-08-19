{ lib
, runCommand
, writeText
, writeTextDir
, writeShellScriptBin
, callPackage

, jupyter-console
}:

with lib;

rec {
  # Based on the version in Nixpkgs, but with different output path
  makeJupyterKernel = definitions: with lib;
    let
      dir = "lib/codedown/kernels";
    in
      runCommand "jupyter-kernels" { inherit dir; } ''
        mkdir -p $dir

        ${concatStringsSep "\n" (mapAttrsToList (kernelName: kernel:
          let
            allowedKernelKeys = ["argv" "display_name" "language" "codemirror_mode" "interrupt_mode" "env" "metadata" "logo32" "logo64"];
            config = builtins.toJSON (
              (filterAttrs (n: v: (any (x: x == n) allowedKernelKeys)) kernel)
              // {display_name = if (kernel.displayName != "") then kernel.displayName else kernelName;}
              // (optionalAttrs (kernel ? interruptMode) { interrupt_mode = kernel.interruptMode; })
            );
            logo32 =
              if (kernel.logo32 != null)
              then "ln -s ${kernel.logo32} '${dir}/${kernelName}/logo-32x32.png';"
              else "";
            logo64 =
              if (kernel.logo64 != null)
              then "ln -s ${kernel.logo64} '${dir}/${kernelName}/logo-64x64.png';"
              else "";
          in ''
            mkdir -p '${dir}/${kernelName}';
            echo ${escapeShellArg config} > '${dir}/${kernelName}/kernel.json';

            ${logo32}
            ${logo64}
          '') definitions)}
        mkdir $out
        cp -r lib $out
    '';

  # `passthru.repls` is the rich description of a kernel's REPLs (icons, argv). What the
  # runtime actually reads is the list in kernel.json under metadata.codedown.repls, where
  # a REPL is just an attr, a label, and a single executable to run in a PTY -- so anything
  # that needs arguments gets a wrapper script here.
  replsToMetadata = kernelName: repls: mapAttrsToList (name: repl: {
    inherit (repl) attr display_name;
    proc =
      if length repl.args == 1
      then head repl.args
      else
        let wrapper = writeShellScriptBin "codedown-repl-${kernelName}-${name}"
                        ''exec ${escapeShellArgs repl.args} "$@"'';
        in "${wrapper}/bin/codedown-repl-${kernelName}-${name}";
  }) repls;

  # A REPL for languages with no interactive interpreter of their own: run the kernel
  # itself under jupyter-console. It gets its own bare kernelspec -- no codedown metadata --
  # so it doesn't have to refer back to the kernel derivation being built, which would be
  # a cycle.
  jupyterConsoleRepl = { displayName, language, argv, env ? {}
                       , icon ? null, iconMonochrome ? null }:
    let
      # withSingleKernel writes its own kernelspec and wraps jupyter-console with a
      # JUPYTER_PATH pointing at it, so the console doesn't depend on the surrounding
      # codedown environment being installed.
      console = jupyter-console.withSingleKernel {
        inherit displayName language argv env;
        logo32 = null;
        logo64 = null;
      };
    in
      {
        display_name = "Jupyter Console";
        attr = "console";
        args = ["${console}/bin/jupyter-console"];
      }
      // optionalAttrs (icon != null) { inherit icon; }
      // optionalAttrs (iconMonochrome != null) { inherit iconMonochrome; };

  writeTextDirWithMeta = meta: path: text: (writeTextDir path text).overrideAttrs (old: {
    inherit meta;
  });

  writeTextDirWithMetaAndPassthru = meta: passthru: path: text: (writeTextDir path text).overrideAttrs (old: {
    inherit meta passthru;
  });

  writeShellScriptBinWithAttrs = attrs: path: text: (writeShellScriptBin path text).overrideAttrs (old: attrs);

  searcher = packages: (callPackage ../tools/sqlite-indexer { inherit packages; }).searcher;
  searcher' = args: (callPackage ../tools/sqlite-indexer args).searcher;

  lexicographyVersionNumber = lexicographyVersionNumber' 5 3;
  lexicographyVersionNumber' = maxComponents: componentLength: s:
    let
      parts = splitString "." s;
      componentsToAdd = maxComponents - length parts;
      withMaxComponents = map padLeftZeros (parts ++ (replicate componentsToAdd "0"));

      replicate = remaining: x: if remaining <= 0 then [] else [x] ++ (replicate (remaining - 1) x);

      replicateStr = remaining: x: if remaining <= 0 then "" else x + (replicateStr (remaining - 1) x);
      padLeftZeros = s: (replicateStr (componentLength - stringLength s) "0") + s;
    in
      concatStrings withMaxComponents;

  packageName = p: if lib.isString p then p else p.name;
}
