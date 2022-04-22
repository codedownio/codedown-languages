{ channels
, importedChannels
, overlays
, importedOverlays

, kernels ? []
, otherPackages ? []
}:


importedChannels.nixpkgs.codedown.mkCodeDownEnvironment {
  inherit channels importedChannels overlays;

  inherit kernels otherPackages;
}
