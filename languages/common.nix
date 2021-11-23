{ lib
, runCommand
, writeTextDir
, callPackage
, makeWrapper
}:

rec {
  makeJupyterKernel = makeJupyterKernelInner false;

  # Based on the version in Nixpkgs, but with different output path
  makeJupyterKernelInner = scrambleKernelJson: definitions: with lib;
    let dir = "lib/codedown/kernels"; in runCommand "jupyter-kernels" { inherit dir; } ''
      mkdir -p $dir

      ${concatStringsSep "\n" (mapAttrsToList (kernelName: kernel:
        let
          allowedKernelKeys = ["argv" "display_name" "language" "interrupt_mode" "env" "metadata" "logo32" "logo64"];
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
          echo '${config}' > '${dir}/${kernelName}/kernel.json';

          if [[ -n "${if scrambleKernelJson then "t" else ""}" ]]; then
            sed -i 's|/nix/store/[^-]*|/nixpath|g' '${dir}/${kernelName}/kernel.json'
          fi

          ${logo32}
          ${logo64}
        '') definitions)}
      mkdir $out
      cp -r lib $out
    '';

  writeTextDirWithMeta = meta: path: text: (writeTextDir path text).overrideAttrs (old: {
    inherit meta;
  });

  searcher = packages: (callPackage ../tools/fuse-indexer { inherit packages; }).searcher;

  hasAttrSafe =  x: set: lib.hasAttr x set && (let
    evaluated = builtins.tryEval (lib.getAttr x set);
  in
    if evaluated.success then true else false);

  wrapShell = executableName: baseDerivation: runCommand "codedown-shell" { inherit baseDerivation executableName;
                                                                            buildInputs = [makeWrapper]; } ''
    mkdir -p $out/lib/codedown
    makeWrapper "$baseDerivation/bin/$executableName" $out/lib/codedown/shell
  '';

  wrapShells = availableShells: shells: runCommand "codedown-shells" { shells = [(map (x: lib.getAttr x availableShells) shells)]; } ''
    mkdir -p $out/lib/codedown/shells

    COUNTER=1
    for shell in $shells; do
      ln -s $shell/lib/codedown/shell $out/lib/codedown/shells/shell$COUNTER
      let COUNTER++
    done

    if [[ -f "$out/lib/codedown/shells/shell1" ]]; then
      ln -s $out/lib/codedown/shells/shell1 $out/lib/codedown/shell
    fi
  '';
}
