{ lib }:

rec {
  chooseInterestingMeta = lib.filterAttrs (n: v:
    n == "description"
    || n == "homepage"
    || n == "changelog"
    || (n == "available" && !v)
    || (n == "broken" && v)
    || (n == "unfree" && v)
    || (n == "unsupported" && v)
    || (n == "insecure" && v)
      # || (n == "license.spdxId") # TODO
  );

  mkChannelUiMetadata = name: channel: channel // {
    name = name;
  };

  mkKernelUiMetadata = kernel: {
    # Dry
    channel = kernel.channel;
    name = kernel.name;
    packages = kernel.args.packages;
    settings = if kernel ? "settings" then kernel.settings else {};

    # Hydrated
    display_name = if kernel.meta ? "displayName" then kernel.meta.displayName else null;
    icon = if kernel.meta ? "icon" then kernel.meta.icon else null;
    modes = kernel.modes;
    settings_schema = if kernel ? "settingsSchema" then kernel.settingsSchema else {};

    # TODO?
    languageServerNames = [];
  };

  mkOtherPackageUiMetadata = package: {
    channel = package.channel;
    attr = package.attr;
    meta = if package.contents ? "meta" then chooseInterestingMeta package.contents.meta else {};
  };
}
