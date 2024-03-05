
let
  bootstrapNixpkgs = import <nixpkgs> {};
  fetchgit = bootstrapNixpkgs.fetchgit;
  fetchFromGitHub = bootstrapNixpkgs.fetchFromGitHub;
  lib = bootstrapNixpkgs.lib;

  overlays = {
    codedown = {
      tag = "path";
      path = ./.;
    };
  };

  channelsAll = with bootstrapNixpkgs; callPackage ./channels.nix { inherit overlays; };
  channels = channelsAll.channels;
  importedChannels = channelsAll.importedChannels;
  importedOverlays = channelsAll.importedOverlays;

in

with bootstrapNixpkgs;

{
  otherPackages = importedChannels.nixpkgs.codedown.nixpkgsSearcher;

  shells = importedChannels.nixpkgs.codedown.shellsSearcher;

  languages = importedChannels.nixpkgs.codedown.languagesSearcher;
  languagesSqlite = importedChannels.nixpkgs.codedown.languagesSearcherSqlite;

  exporters = importedChannels.nixpkgs.codedown.exportersSearcher;
  exportersSqlite = importedChannels.nixpkgs.codedown.exportersSearcherSqlite;
}
