{ stdenv
, pkgs
, python
}:

with pkgs.lib;

let
  initialization_options = {
    interpreter = {
      properties = {
        Version = python.version;
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

    buildInputs = [pkgs.coreutils python];

    buildPhase = ''
      mkdir -p $out/cache

      echo '${generators.toJSON {} initialization_options}' > initialization_options.json
      echo '${generators.toJSON {} (configuration_settings "FILLED_IN")}' > configuration_settings.json

      python ${generate_cache} ${pkgs.python-language-server}/bin/python-language-server $out/cache ./initialization_options.json ./configuration_settings.json
    '';

    dontInstall = true;

    dontFixup = true;
  };

in

{
  config = {
    name = "python";
    extensions = ["py"];
    attrs = ["python"];
    type = "stream";
    args = ["${pkgs.python-language-server}/bin/python-language-server"];
    configuration_settings = configuration_settings "${cache}/cache";
    initialization_options = overrideExisting initialization_options { cacheFolderPath = "${cache}/cache"; };
  };
}
