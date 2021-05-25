{ lib
, runCommand
}:

{
  # Based on the version in Nixpkgs, but with different output path
  makeJupyterKernel = definitions: with lib;
    let dir = "lib/codedown/kernels"; in runCommand "jupyter-kernels" { inherit dir; } ''
      mkdir -p $dir

      ${concatStringsSep "\n" (mapAttrsToList (kernelName: unfilteredKernel:
        let
          allowedKernelKeys = ["argv" "displayName" "language" "interruptMode" "env" "metadata" "logo32" "logo64"];
          kernel = filterAttrs (n: v: (any (x: x == n) allowedKernelKeys)) unfilteredKernel;
          config = builtins.toJSON (
            kernel
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
}
