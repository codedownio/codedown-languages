{ lib
, callPackage
, symlinkJoin

, jdk
, jdt-language-server

, settings
, settingsSchema
}:

with { inherit (settings) packages; };
with { inherit (settings.interface) attrs extensions; };

with lib;

let
  common = callPackage ../common.nix {};

  maven = callPackage ./maven {};

  jjava = callPackage ./jjava.nix {};
  jjavaVersion = import ./jjava-version.nix;

  kernelName = "java";
  displayName = "Java";

  # null when nothing is selected, so JJAVA_CLASSPATH isn't an empty glob.
  classpath = if packages == [] then null else maven.mkClasspath { inherit packages; };

  inherit (maven) packageOptions;
  packageSearch = common.searcher' {
    packageMustBeDerivation = false;
    packages = packageOptions;
  };

  # JShell is the same engine the kernel uses, so the REPL and notebook behave alike.
  repls = {
    jshell = {
      display_name = "JShell " + jdk.version;
      attr = "jshell";
      args = ["${jdk}/bin/jshell"]
             ++ optionals (classpath != null) ["--class-path" "${classpath}/share/java/*"];
      icon = ./java-logo-64x64.png;
      iconMonochrome = ./java-monochrome.svg;
    };
  };

  languageServers =
    []
    ++ optionals settings.lsp.jdtls.enable [(callPackage ./language-server-jdtls {
      inherit attrs kernelName classpath;
      settings = settings.lsp.jdtls;
    })]
  ;

in

symlinkJoin {
  name = kernelName;

  paths = [
    (callPackage ./kernel.nix {
      inherit jjava classpath attrs extensions repls;
      inherit kernelName displayName;
      inherit (settings.misc) loadExtensions;
      version = jdk.version;
    })

    jdk
  ]
  ++ languageServers
  ;

  passthru = {
    meta = jdk.meta // {
      baseName = kernelName;
      inherit displayName;
      version = jdk.version;
      icon = ./java-logo-64x64.png;
      iconMonochrome = ./java-monochrome.svg;
      inherit settingsSchema;
      hasPackages = packageOptions != {};
    };
    inherit packageOptions packageSearch;
    versions = {
      jdk = jdk.version;
      jjava = jjavaVersion;
      jdt-language-server = jdt-language-server.version;
    };
    inherit settingsSchema settings;
    inherit repls;
    inherit classpath jjava;
    modes = {
      inherit attrs extensions;
      code_mirror_mode = "clike";
      code_mirror_mime_type = "text/x-java";
    };
    languageServerNames = map (x: x.languageServerName) languageServers;
    formatters = lib.concatMap (x: x.formatters or []) languageServers;
  };
}
