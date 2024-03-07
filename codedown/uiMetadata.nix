{ lib }:

rec {
  chooseInterestingMeta = contents: (lib.optionalAttrs (contents ? "version") {
    version = contents.version;
  }) // (lib.optionalAttrs (contents ? "meta") (
    lib.filterAttrs (n: v:
      n == "description"
      || n == "displayName"
      || n == "icon"

      || n == "homepage"
      || n == "downloadPage"
      || n == "changelog"

      || (n == "available" && !v)
      || (n == "broken" && v)
      || (n == "unfree" && v)
      || (n == "unsupported" && v)
      || (n == "insecure" && v)
      || (n == "lessCommon" && v)

      || (n == "maintainers")
    )
      contents.meta
  )) // (lib.optionalAttrs (lib.hasAttrByPath ["meta" "license" "spdxId"] contents) {
    spdxId = contents.meta.license.spdxId;
  }) // (lib.optionalAttrs (contents ? "settingsSchema") {
    inherit (contents) settingsSchema;
  }) // (lib.optionalAttrs (contents ? "modes") {
    inherit (contents) modes;
  });

  mkChannelUiMetadata = name: channel: channel // {
    name = name;
  };

  packageName = p: if lib.isString p then p else p.name;

  mkKernelPackageMetadata = kernel: p: {
    name = packageName p;
    meta = if lib.hasAttrByPath ["packageOptions" (packageName p)] kernel then chooseInterestingMeta (kernel.packageOptions.${p}) else {};
  };

  mkKernelUiMetadata = kernel: {
    # Dry
    channel = kernel.channel;
    name = kernel.name;
    settings = if kernel ? "settings" then kernel.settings else {};

    # Different for hydrated
    packages = map (p: mkKernelPackageMetadata kernel p) kernel.args.packages;

    # Hydrated
    display_name = if kernel.meta ? "displayName" then kernel.meta.displayName else null;
    icon = if kernel.meta ? "icon" then kernel.meta.icon else null;
    modes = kernel.modes;
    settings_schema = if kernel ? "settingsSchema" then kernel.settingsSchema else {};
    meta = chooseInterestingMeta kernel;

    # TODO?
    languageServerNames = [];
  };

  mkOtherPackageUiMetadata = package: {
    channel = package.channel;
    attr = package.attr;
    meta = if package.contents ? "meta" then chooseInterestingMeta package.contents else {};
  };
}
