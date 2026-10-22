{ lib
, callPackage

, jdk
, jjava

, attrs
, extensions
, version
, repls ? {}

, classpath
, loadExtensions

, kernelName
, displayName
}:

with lib;

let
  common = callPackage ../common.nix {};

in

common.makeJupyterKernel {
  ${kernelName} = {
    inherit displayName;
    language = "java";
    # Skipping jjava-launcher.jar, whose only job is to re-exec `java` off PATH -- which a
    # Jupyter-started kernel doesn't control. The --add-opens is JShell's; without it the
    # kernel won't start.
    argv = [
      "${jdk}/bin/java"
      "--add-opens" "jdk.jshell/jdk.jshell=ALL-UNNAMED"
      "-jar" "${jjava}/jjava.jar"
      "{connection_file}"
    ];
    logo32 = ./java-logo-32x32.png;
    logo64 = ./java-logo-64x64.png;

    interruptMode = "message";

    # cwd is the user's notebook directory, so jars come in through the environment.
    # JJAVA_CLASSPATH is glob-expanded, so one directory covers the selection.
    env =
      optionalAttrs (classpath != null) { JJAVA_CLASSPATH = "${classpath}/share/java/*"; }
      # JJava scans every jar for kernel extensions at startup; skippable if that's slow.
      // optionalAttrs (!loadExtensions) { JJAVA_LOAD_EXTENSIONS = "false"; };

    metadata = {
      codedown = {
        inherit attrs extensions;
        language_version = version;

        # JJava exposes no way to list JShell's snippets, so there's nothing to inspect.
        variable_inspector = null;

        repls = common.replsToMetadata kernelName repls;

        priority = 1;
      };
    };
  };
}
