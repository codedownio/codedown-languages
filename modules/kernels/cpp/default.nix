{ lib
, callPackage
, writeTextDir
, symlinkJoin
, cling
, clang
, llvmPackages_13
, xeus-cling

, settings
, settingsSchema
}:

with { inherit (settings) packages extensions flavor; };

with lib;

let
  attrs = [flavor] ++ settings.attrs;

  common = callPackage ../common.nix {};

  displayName = "C++";

  # Fix for https://github.com/NixOS/nixpkgs/issues/306782
  clingToUse = cling.override {
    llvmPackages_13 = llvmPackages_13.override { enableSharedLibraries = false; };
  };
  xeusClingToUse = xeus-cling.override {
    cling = clingToUse;
  };

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
      cling = clingToUse;
      xeus-cling = xeusClingToUse;
      inherit attrs displayName extensions;
      std = flavor;
      kernelName = "cpp";
    })
    cling
  ]
  ;

  passthru = {
    meta = clang.meta // {
      baseName = "cpp";
      inherit displayName;
      version = clang.version;
      icon = getAttr flavor icons;
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
    args = {
      inherit attrs extensions settings packages;
    };
    repls = {
      cling = {
        display_name = "Cling " + clingToUse.unwrapped.version;
        attr = "cling";
        args = ["${clingToUse}/bin/cling"];
        icon = icons.${flavor};
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
