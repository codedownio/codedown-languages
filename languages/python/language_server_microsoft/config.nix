{ stdenv
, coreutils
, python
, python-language-server
, callPackage
, lib
, kernelName
}:

with lib;

let
  common = callPackage ../../common.nix {};

  initialization_options = {
    interpreter = {
      properties = {
        Version = python.python.version;
        InterpreterPath = "${python}/bin/python";
      };
    };
    searchPaths = [];
    typeStubSearchPaths = [];
    excludeFiles = [];
    includeFiles = ["**/*.py"];
    cacheFolderPath = "FILLED_IN";
  };

  configuration_settings = cacheFolderPath: {
    python = {
      linting = {
        enabled = true;
      };
      analysis = {
        enabled = true;
        cachingLevel = "System";
        cacheFolderPath = cacheFolderPath;
      };
    };
  };

  generate_cache = ./generate_cache.py;

  cache = stdenv.mkDerivation {
    name = "microsoft-python-language-server-cache";

    dontUnpack = true;

    buildInputs = [coreutils python];

    buildPhase = ''
      mkdir -p $out/cache

      echo '${generators.toJSON {} initialization_options}' > initialization_options.json
      echo '${generators.toJSON {} (configuration_settings "FILLED_IN")}' > configuration_settings.json

      python ${generate_cache} ${python-language-server}/bin/python-language-server $out/cache ./initialization_options.json ./configuration_settings.json
    '';

    dontInstall = true;

    dontFixup = true;
  };

in

common.writeTextDirWithMeta python-language-server.meta "lib/codedown/python-microsoft-language-servers.yaml"
  (lib.generators.toYAML {} [{
    name = "python-language-server";
    display_name = "Python Language Server";
    description = python-language-server.meta.description;
    extensions = ["py"];
    notebook_suffix = ".py";
    kernel_name = kernelName;
    attrs = ["python"];
    type = "stream";
    args = ["${python-language-server}/bin/python-language-server"];
    # configuration_settings = configuration_settings "${cache}/cache";
    initialization_options = overrideExisting initialization_options {
      # cacheFolderPath = "${cache}/cache";
    };
  }])
