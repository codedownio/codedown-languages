{ config, options, lib, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

let
  pkgs = config.pkgs;

in

{
  options = {
    kernels.java = {
      enable = mkOption {
        title = "Enable Java kernel";
        type = types.bool;
        default = false;
        visible = false;
      };

      packages = mkOption {
        title = "List of packages";
        type = types.listOf types.str;
        default = [];
        visible = false;
      };

      jdkPackage = mkOption {
        title = "JDK version";
        description = "JShell's handling of snippets improves with newer releases, so the default is whatever Nixpkgs calls the current JDK.";
        type = types.enum ["jdk" "jdk17" "jdk21" "jdk23" "jdk24" "jdk25"];
        default = "jdk";
      };

      interface.attrs = mkOption {
        title = boilerplate.attrsTitle;
        description = boilerplate.attrsDescription;
        type = types.listOf types.str;
        default = ["java"];
      };
      interface.extensions = mkOption {
        title = boilerplate.extensionsTitle;
        description = boilerplate.extensionsDescription;
        type = types.listOf types.str;
        default = ["java"];
      };

      lsp.jdtls.enable = mkOption {
        title = "Enable the Eclipse JDT language server";
        description = "JDT LS compiles whole Java files, and a cell of loose statements is not one, so this answers for .java files in the environment rather than for notebook cells.";
        type = types.bool;
        default = true;
      };
      lsp.jdtls.notebookProxy = mkOption {
        title = "Answer for notebook cells as well as files";
        description = "Runs the language server behind java-notebook-language-server, which projects cells into a document the compiler accepts and maps positions back. Without it the server answers for .java files only, because a cell of loose statements is not a compilation unit.";
        type = types.bool;
        default = true;
      };
      lsp.jdtls.debug = mkOption {
        title = "Eclipse JDT language server: debug output";
        type = types.bool;
        default = false;
      };

      misc.loadExtensions = mkOption {
        title = "Load kernel extensions from the classpath";
        description = "JJava scans every jar on the classpath for kernel extensions at startup, and again whenever the classpath grows. Turn this off to skip the scan if startup is slow with a large package selection.";
        type = types.bool;
        default = true;
      };
    };
  };

  config = mkIf config.kernels.java.enable {
    builtKernels.java = pkgs.callPackage ./. {
      jdk = getAttr config.kernels.java.jdkPackage pkgs;

      settings = config.kernels.java;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.java;
    };
  };
}
