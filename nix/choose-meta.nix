{ lib }:

contents:

let
  # Test if the derivation has a single output
  hasSimpleOutputs =
    lib.hasAttr "outputName" contents
    && lib.hasAttr "outputs" contents
    && [contents.outputName] == contents.outputs;

  includeOutputsOption = !hasSimpleOutputs && ((contents.outputs or null) != null);

in

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
}) // (lib.optionalAttrs ((contents ? "settingsSchema") || includeOutputsOption) {
  settings_schema =
    (contents.settingsSchema or {})
    // lib.optionalAttrs includeOutputsOption {
      outputs = {
        title = "Outputs";
        description = "Package outputs to include.";
        type = "list";
        listType = {
          type = "enum";
          values = (contents.outputs or []);
        };
        defaultValue = if lib.hasAttr "outputName" contents then [contents.outputName] else [];
      };
    };
}) // (lib.optionalAttrs (contents ? "modes") {
  inherit (contents) modes;
}) // (lib.optionalAttrs (contents ? "languageServerNames") {
  language_server_names = contents.languageServerNames;
})
