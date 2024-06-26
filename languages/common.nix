{ lib
, runCommand
, writeTextDir
, writeShellScriptBin
, callPackage
}:

with lib;

rec {
  # Based on the version in Nixpkgs, but with different output path
  makeJupyterKernel = definitions: with lib;
    let dir = "lib/codedown/kernels"; in runCommand "jupyter-kernels" { inherit dir; } ''
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
          echo '${config}' > '${dir}/${kernelName}/kernel.json';

          ${logo32}
          ${logo64}
        '') definitions)}
      mkdir $out
      cp -r lib $out
    '';

  writeTextDirWithMeta = meta: path: text: (writeTextDir path text).overrideAttrs (old: {
    inherit meta;
  });

  writeTextDirWithMetaAndPassthru = meta: passthru: path: text: (writeTextDir path text).overrideAttrs (old: {
    inherit meta passthru;
  });

  writeShellScriptBinWithMeta = meta: path: text: (writeTextDir path text).overrideAttrs (old: {
    inherit meta;
  });

  writeShellScriptBinWithAttrs = attrs: path: text: (writeShellScriptBin path text).overrideAttrs (old: attrs);

  searcher = packages: (callPackage ../tools/sqlite-indexer { inherit packages; }).searcher;
  searcher' = args: (callPackage ../tools/sqlite-indexer args).searcher;
  searcherIcons' = args: (callPackage ../tools/sqlite-indexer args).allIcons;

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

  hasAttrSafe =  x: set: hasAttr x set && (let
    evaluated = builtins.tryEval (getAttr x set);
  in
    if evaluated.success then true else false);

  safeEval = safeEval' "";
  safeEval' = default: e: let
    evaluated = builtins.tryEval e;
  in
    if evaluated.success then evaluated.value else default;

  isTrue = settings: name: hasAttr name settings && getAttr name settings == true;

  focusSettings = prefix: settings: with lib; let
    filtered = lib.filterAttrs (n: _: hasPrefix prefix n) settings;
  in
    listToAttrs (mapAttrsToList (n: v: {
      name = removePrefix prefix n;
      value = v;
    }) filtered);

  makeDefaultSettings = settingsSchema: listToAttrs (map (item: {
    name = item.target;
    value = item.defaultValue;
  }) (filter (hasAttr "target") settingsSchema));

  packageName = p: if lib.isString p then p else p.name;
}
