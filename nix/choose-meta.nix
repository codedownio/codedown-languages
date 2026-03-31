{ lib }:

contents:

(lib.optionalAttrs (contents ? "version") {
  version = contents.version;
}) // (lib.optionalAttrs (contents ? "meta") (
  lib.filterAttrs (n: v:
    n == "name"
    || n == "description"
    || n == "icon"
    || n == "category"

    || n == "homepage"
    || n == "changelog"

    || (n == "available" && !v)
    || (n == "broken" && v)
    || (n == "unfree" && v)
    || (n == "unsupported" && v)
    || (n == "insecure" && v)

    || (n == "maintainers")
  )
    contents.meta
)) // (lib.optionalAttrs (lib.hasAttrByPath ["meta" "license" "spdxId"] contents) {
  spdx_id = contents.meta.license.spdxId;
}) // (lib.optionalAttrs (lib.hasAttrByPath ["meta" "downloadPage"] contents) {
  download_page = contents.meta.downloadPage;
}) // (lib.optionalAttrs (lib.hasAttrByPath ["meta" "displayName"] contents) {
  display_name = contents.meta.displayName;
}) // (lib.optionalAttrs (lib.hasAttrByPath ["meta" "hasPackages"] contents) {
  has_packages = contents.meta.hasPackages;
}) // (lib.optionalAttrs (lib.hasAttrByPath ["meta" "lessCommon"] contents) {
  less_common = contents.meta.lessCommon;
}) // (lib.optionalAttrs (lib.hasAttrByPath ["meta" "mainProgram"] contents) {
  main_program = contents.meta.mainProgram;
}) // (lib.optionalAttrs (lib.hasAttrByPath ["meta" "iconMonochrome"] contents) {
  icon_monochrome = contents.meta.iconMonochrome;
}) // (lib.optionalAttrs (contents ? "settingsSchema") {
  settings_schema = contents.settingsSchema;
}) // (lib.optionalAttrs (contents ? "modes") {
  inherit (contents) modes;
}) // (lib.optionalAttrs (contents ? "languageServerNames") {
  language_server_names = contents.languageServerNames;
}) // (lib.optionalAttrs (lib.hasAttrByPath ["meta" "exporterInfos"] contents) {
  exporter_infos = let
    # Whitelist fields to avoid leaking store paths (e.g. args, pandoc) into the search index
    keepFields = info: lib.filterAttrs (n: _:
      n == "name" || n == "display_name" || n == "group" || n == "extension"
      || n == "icon" || n == "icon_monochrome" || n == "input_extensions"
    ) info;
  in map keepFields contents.meta.exporterInfos;
})
