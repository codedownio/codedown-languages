self: super: {
  # evcxr = super.evcxr.overrideAttrs (oldAttrs: {
  #   src = oldAttrs.src.override {
  #     rev = "f7e28a10d759c862b9ccb685b07fa3444444e18b";
  #     sha256 = "sha256-SXyJcvsFMM/CSZS7v+xRxP/Ar4/d14Uk1N4GkmMPNUw=";
  #   };
  #   cargoHash = "sha256-qjenAQKWxt0hiJkAe4brTSkFSNm8F7WNXXF3OAeOVUU=";
  # });

  evcxr = self.callPackage ./evcxr.nix {
    inherit (self.darwin.apple_sdk.frameworks) CoreServices Security;
  };
}
