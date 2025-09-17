{ lib
, callPackage
, symlinkJoin
, cling
, clang
, xeus-cling

, settings
, settingsSchema
}:

with { inherit (settings) packages flavor; };
with { inherit (settings.interface) extensions; };

with lib;

let
  attrs = [flavor] ++ settings.interface.attrs;

  common = callPackage ../common.nix {};

  displayName = "C++";

  icons = {
    # cpp98 = ./cpp11-logo-64x64.png; # TODO
    "c++11" = ./cpp11-logo-64x64.png;
    "c++14" = ./cpp14-logo-64x64.png;
    "c++17" = ./cpp17-logo-64x64.png;
    "c++20" = ./cpp2a-logo-64x64.png; # TODO
    "c++23" = ./cpp2a-logo-64x64.png; # TODO
  };

  packageOptions = {};
  packageSearch = common.searcher packageOptions;

in

symlinkJoin {
  name = "cpp";
  paths = [
    (callPackage ./kernel_xeus.nix {
      inherit attrs displayName extensions;
      std = flavor;
      kernelName = "cpp";
    })
    cling
  ];

  passthru = {
    meta = clang.meta // {
      baseName = "cpp";
      inherit displayName;
      version = clang.version;
      icon = getAttr flavor icons;
      iconMonochrome = ./cplusplus.svg;
      inherit settingsSchema;
      hasPackages = packageOptions != {};
    };
    inherit packageOptions packageSearch;
    versions = {
      clang = clang.version;
      cling = cling.unwrapped.version;
      xeus-cling = xeus-cling.version;
      std = flavor;
    };
    inherit settings settingsSchema;
    repls = {
      cling = {
        display_name = "Cling " + cling.unwrapped.version;
        attr = "cling";
        args = ["${cling}/bin/cling"];
        icon = icons.${flavor};
        iconMonochrome = ./cplusplus.svg;
      };
    };
    modes = {
      inherit attrs extensions;
      code_mirror_mode = "clike";
      code_mirror_mime_type = "text/x-c++src";
    };
    languageServerNames = [];
  };
}
