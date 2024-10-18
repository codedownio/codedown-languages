{ lib
, symlinkJoin

, chooseMeta

, name ? "nixpkgs-environment"
}:

self: super: {
  makeEnvironment = fakeModule:
    symlinkJoin {
      inherit name;
      paths = let
        getOutputs = pkg:
          let
            outputs = (fakeModule.${pkg}.outputs or null);
          in
            if outputs != null
            then (map (x: self.${pkg}.${x}) outputs)
            else [self.${pkg}];
      in
        self.lib.concatMap getOutputs (builtins.attrNames fakeModule);

      passthru = {
        ui_metadata = {
          packages = self.lib.mapAttrs (n: v: {
            name = n;
            meta = chooseMeta (self.${n} or {});
            packages = [];
            settings =
              {}
              // lib.optionalAttrs (lib.hasAttr "outputs" v) {
                inherit (v) outputs;
              };
          }) fakeModule;
        };
      };
    };
}
