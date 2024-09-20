{ poetry2nix }:

poetry2nix.defaultPoetryOverrides.extend (self: super: {
  lsprotocol = super.lsprotocol.overridePythonAttrs (old: {
    buildInputs = (old.buildInputs or [ ]) ++ [ super.flit ];
  });

  attrs = super.attrs.overridePythonAttrs (old: {
    buildInputs = (old.buildInputs or [ ]) ++ [ super.hatchling ];
  });
})
