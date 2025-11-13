{ lib
, callPackage
, symlinkJoin
, cling
, clang
, xeus-cling
, llvmPackages

, settings
, settingsSchema
}:

with { inherit (settings) packages flavor; };
with { inherit (settings.interface) extensions; };

with lib;

let
  attrs = [flavor] ++ settings.interface.attrs;

  kernelName = "cpp";

  common = callPackage ../common.nix {};

  languageServers = lib.optionals settings.lsp.clangd.enable
    [(callPackage ./language_server_clangd { inherit kernelName llvmPackages; })];

  displaySuffix = {
    "c++17" = " 17";
    "c++20" = " 20";
    "c++23" = " 23";
    "c++2c" = " 26";

    "gnu++17" = " 17";
    "gnu++20" = " 20";
    "gnu++23" = " 23";
    "gnu++2c" = " 26";
  };

  displayName = "C++" + (displaySuffix.${flavor} or "");

  iconsPng = {
    "c++17" = ./cpp17-logo-64x64.png;
    "c++20" = ./cpp20-logo-64x64.png;
    "c++23" = ./cpp23-logo-64x64.png;
    "c++2c" = ./cpp2c-logo-64x64.png;

    "gnu++17" = ./cpp17-logo-64x64.png;
    "gnu++20" = ./cpp20-logo-64x64.png;
    "gnu++23" = ./cpp23-logo-64x64.png;
    "gnu++2c" = ./cpp2c-logo-64x64.png;
  };

  iconsSvg = {
    "c++17" = ./cpp17.svg;
    "c++20" = ./cpp20.svg;
    "c++23" = ./cpp23.svg;
    "c++2c" = ./cpp2c.svg;

    "gnu++17" = ./cpp17.svg;
    "gnu++20" = ./cpp20.svg;
    "gnu++23" = ./cpp23.svg;
    "gnu++2c" = ./cpp2c.svg;
  };

  iconsSvgMonochrome = {
    "c++17" = ./cpp17-monochrome.svg;
    "c++20" = ./cpp20-monochrome.svg;
    "c++23" = ./cpp23-monochrome.svg;
    "c++2c" = ./cpp2c-monochrome.svg;

    "gnu++17" = ./cpp17-monochrome.svg;
    "gnu++20" = ./cpp20-monochrome.svg;
    "gnu++23" = ./cpp23-monochrome.svg;
    "gnu++2c" = ./cpp2c-monochrome.svg;
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
      inherit kernelName;
    })
    cling
  ]
  ++ languageServers
  ;

  passthru = {
    meta = clang.meta // {
      baseName = "cpp";
      inherit displayName;
      version = clang.version;
      icon = getAttr flavor iconsSvg;
      iconMonochrome = getAttr flavor iconsSvgMonochrome;
      inherit settingsSchema;
      hasPackages = packageOptions != {};
    };
    inherit packageOptions packageSearch;
    versions = {
      clang = clang.version;
      cling = cling.unwrapped.version;
      xeus-cling = xeus-cling.version;
      clangd = llvmPackages.clang-tools.version;
      std = flavor;
    };
    inherit settings settingsSchema;
    repls = {
      cling = {
        display_name = "Cling " + cling.unwrapped.version;
        attr = "cling";
        args = ["${cling}/bin/cling"];
        icon = iconsSvg.${flavor};
        iconMonochrome = iconsSvgMonochrome.${flavor};
      };
    };
    modes = {
      inherit attrs extensions;
      code_mirror_mode = "clike";
      code_mirror_mime_type = "text/x-c++src";
    };
    languageServerNames = map (x: x.languageServerName) languageServers;
  };
}
