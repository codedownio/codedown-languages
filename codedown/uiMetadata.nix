{ lib }:

rec {
  chooseInterestingMeta = contents: (lib.optionalAttrs (contents ? "version") {
    version = contents.version;
  }) // (lib.optionalAttrs (contents ? "meta") (
    lib.filterAttrs (n: v:
      n == "description"
      || n == "icon"

      || n == "homepage"
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
    spdx_id = contents.meta.license.spdxId;
  }) // (lib.optionalAttrs (lib.hasAttrByPath ["meta" "downloadPage"] contents) {
    download_page = contents.meta.downloadPage;
  }) // (lib.optionalAttrs (lib.hasAttrByPath ["meta" "displayName"] contents) {
    display_name = contents.meta.displayName;
  }) // (lib.optionalAttrs (contents ? "settingsSchema") {
    settings_schema = contents.settingsSchema;
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
    meta = chooseInterestingMeta kernel;
  };

  mkOtherPackageUiMetadata = package: {
    channel = package.channel;
    attr = package.attr;
    meta = if package.contents ? "meta" then chooseInterestingMeta package.contents else {};
  };
}
