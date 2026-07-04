# Fix the nodehun build on Darwin under newer Apple clang.
#
# nodehun 3.0.2 vendors node-addon-api 1.6.3, whose napi.h has
#   static const napi_typedarray_type unknown_array_type =
#     static_cast<napi_typedarray_type>(-1);
# Newer clang (Apple SDK 14+) rejects this in-class initializer because -1 is
# outside the enum's declared range under stricter constexpr enum-range
# checking. Upstream node-addon-api dropped this sentinel entirely and defaults
# to napi_int8_array; we do the same. The value is only used as a "no-match"
# return from a primitive->typed-array-type lookup that nodehun never invokes
# for the unmapped types.
#
# `npmConfigHook` runs as a postPatch hook and itself does both `npm ci` (which
# materialises node_modules) and `npm rebuild` (which compiles the native addon
# via node-gyp) in one go. We tell npm rebuild to skip scripts so node-gyp
# doesn't run yet, patch the now-present header, then drive the native build
# ourselves in preBuild.
nodehun: nodehun.overrideAttrs (oldAttrs: {
  npmRebuildFlags = (oldAttrs.npmRebuildFlags or []) ++ [ "--ignore-scripts" ];

  preBuild = (oldAttrs.preBuild or "") + ''
    if [ -f node_modules/node-addon-api/napi.h ]; then
      substituteInPlace node_modules/node-addon-api/napi.h \
        --replace-fail 'static_cast<napi_typedarray_type>(-1)' 'napi_int8_array'
    fi
    npm rebuild --offline --foreground-scripts
  '';
})
