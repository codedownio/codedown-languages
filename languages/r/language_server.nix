{ pkgs
, stdenv
, lib
, callPackage
, fetchFromGitHub

, rWrapper
, rPackages
, basePackages
, R
, kernelName
}:

let
  common = callPackage ../common.nix {};

  # languageserver_dependencies = with pkgs; [callr jsonlite lintr R6 repr stringr styler desc collections];
  languageserver_dependencies = [];

  languageServerDeps = with rPackages; [callr collections desc fs jsonlite lintr R6 repr roxygen2 stringi styler xml2 xmlparsedata];

  buildR = rWrapper.override { packages = languageServerDeps; };

  version = "0.3.16";

  languageserver = stdenv.mkDerivation {
    name = "r-custom-languageserver";
    inherit version;

    inherit languageserver_dependencies;

    src = fetchFromGitHub {
      owner = "REditorSupport";
      repo = "languageserver";
      rev = "beed0f00563390b3e39a9bb47fc25bf852ec3734";
      sha256 = "0sifrmkv6hn15nppjg4wjvs3l8an3lsa4rdp3xi3f4w006kdgx5w";
    };

    configurePhase = ''
      runHook preConfigure
      export R_LIBS_SITE="$R_LIBS_SITE''${R_LIBS_SITE:+:}$out/library"
      runHook postConfigure
    '';

    buildInputs = [buildR];

    buildPhase = ''
      runHook preBuild
      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall
      mkdir -p $out/library
      R CMD INSTALL $installFlags --configure-args="$configureFlags" -l $out/library .
      runHook postInstall
    '';

    postFixup = ''
      if test -e $out/nix-support/propagated-build-inputs; then
      ln -s $out/nix-support/propagated-build-inputs $out/nix-support/propagated-user-env-packages
      fi
    '';

    meta = {
      description = "An implementation of the Language Server Protocol for R";
      homepage = "https://github.com/REditorSupport/languageserver";
    };
  };

  rWithPackagesAndLanguageServer = rWrapper.override {
    packages = [languageserver] ++ languageServerDeps ++ basePackages;
  };

in

common.writeTextDirWithMeta languageserver.meta "lib/codedown/language-servers/r-${kernelName}-languageserver.yaml" (lib.generators.toYAML {} [{
  name = "languageserver";
  inherit version;
  display_name = "";
  description = "An implementation of the Language Server Protocol for R";
  icon = null;
  extensions = ["r"];
  notebook_suffix = ".r";
  attrs = ["r" "R"];
  type = "stream";
  primary = true;
  args = ["${rWithPackagesAndLanguageServer}/bin/R" "--slave" "-e" "languageserver::run()"];
}])
