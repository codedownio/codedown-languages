# JDT LS needs every document to be a valid compilation unit, which a cell isn't. With
# notebookProxy on, java-notebook-language-server projects cells into one; with it off JDT LS
# runs directly and answers for .java files only.
#
# notebook_suffix stays "" either way: without the proxy it stops JDT LS being handed a cell
# it can't parse, and with it the proxy names its own shadow document.
{ lib
, callPackage
, coreutils
, jdk
, runCommand
, makeWrapper
, writeShellScriptBin

, jdt-language-server

, attrs
, kernelName
, classpath
, settings
}:

let
  common = callPackage ../../common.nix {};

  languageServerName = "jdtls";

  jnlsVersion = import ./jnls-version.nix;
  jnls = callPackage ./jnls.nix {};
  java-parser = callPackage ./java-parser.nix { inherit jdk; };

  # Eclipse won't build its invisible project if the workspace is inside the project folder,
  # and jdtls defaults -data to $HOME/.cache -- so a HOME inside the project breaks every
  # request. Keying on the full path also avoids upstream's basename collision.
  wrapped = writeShellScriptBin "jdtls" ''
    export PATH=${lib.makeBinPath [coreutils]}:$PATH

    data="''${XDG_CACHE_HOME:-''${HOME:-/tmp}/.cache}/codedown/jdtls/$(printf %s "$PWD" | sha1sum | cut -d' ' -f1)"
    case "$data" in
      "$PWD"/*) data=$(mktemp -d) ;;
    esac

    exec ${jdt-language-server}/bin/jdtls -data "$data" "$@"
  '';

  jnls-wrapped = runCommand "java-notebook-language-server-${jnlsVersion}-wrapped" {
    nativeBuildInputs = [makeWrapper];
  } ''
    mkdir -p $out/bin
    makeWrapper ${jnls}/bin/java-notebook-language-server $out/bin/java-notebook-language-server \
      --prefix PATH : ${java-parser}/bin
  '';

  passthru = {
    inherit languageServerName;
    # Only claimed without the proxy: it declines notebook formatting until SnippetSifter
    # implements unproject.
    formatters = lib.optionals (!settings.notebookProxy) ["eclipse-jdt"];
  };

  args =
    if settings.notebookProxy
    then ["${jnls-wrapped}/bin/java-notebook-language-server"
          "--wrapped-server" "${wrapped}/bin/jdtls"]
         ++ lib.optionals settings.debug ["--log-level" "debug"]
    else ["${wrapped}/bin/jdtls"]
         ++ lib.optionals settings.debug ["--jvm-arg=-Dlog.level=ALL"]
  ;

in

common.writeTextDirWithMetaAndPassthru jdt-language-server.meta passthru
  "lib/codedown/language-servers/${kernelName}-jdtls.yaml"
  (lib.generators.toYAML {} [{
    name = languageServerName;
    version = jdt-language-server.version;
    display_name = "Eclipse JDT language server";
    description = jdt-language-server.meta.description;
    icon = ../java-logo-64x64.png;
    icon_monochrome = ../java-monochrome.svg;
    extensions = ["java"];
    notebook_suffix = "";
    kernel_name = kernelName;
    inherit attrs args;
    type = "stream";
    primary = true;
    env = {};
    # Passed through untouched by the proxy, so this reaches JDT LS either way.
    initialization_options = {
      settings.java = {
        # An invisible project's classpath is just the JDK, so name the environment's jars.
        project.referencedLibraries =
          lib.optionals (classpath != null) ["${classpath}/share/java/*.jar"];
        # Nothing here is a Maven or Gradle project; the importers would just hit the network.
        import.maven.enabled = false;
        import.gradle.enabled = false;
        configuration.updateBuildConfiguration = "disabled";
        # workspace/symbol answers with types alone by default, and everything a cell
        # declares is a method on the projection's synthetic class.
        symbols.includeSourceMethodDeclarations = true;
      };
    };
    language_id = "java";
  }])
