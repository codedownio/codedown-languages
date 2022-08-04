{ lib
, writeTextDir
}:

with lib;

{
  channels
, shells
, exporters ? []
, repls ? {}
, kernels ? []
, otherPackages ? []
, ...
}: writeTextDir "lib/codedown/spec.yaml" (lib.generators.toYAML {} {
  channels = lib.mapAttrsToList (name: value: value // { inherit name; }) channels;

  shells = map (x: {
    channel = x.channel;
    attr = x.attr;
    name = x.contents.name;
    meta = x.contents.meta;
  }) shells;

  exporters = map (x: {
    channel = x.channel;
    attr = x.attr;
    name = x.contents.name;
    meta = x.contents.meta;
  }) exporters;

  kernels = map (x: {
    channel = x.channel;
    language = x.language;
    display_name = attrByPath ["meta" "displayName"] null x;
    icon = attrByPath ["meta" "icon"] null x.passthru;
    meta = attrByPath ["meta"] null x.passthru;
    packages = map (name: {
      inherit name;
      meta = attrByPath [name "meta"] null x.passthru.packageOptions;
    }) x.passthru.args.packages;
    language_servers = map (name: {
      inherit name;
      meta = attrByPath [name "meta"] null x.passthru.languageServerOptions;
    }) x.passthru.args.languageServers;
    settings_schema = attrByPath ["passthru" "settingsSchema"] null x;
    settings = attrByPath ["passthru" "settings"] null x;
  }) kernels;

  other_packages = map (x: {
    channel = x.channel;
    attr = x.attr;
    name = x.contents.name;
    meta = x.contents.meta;
  }) otherPackages;
})
