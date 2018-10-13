{ pkgs-func ? import (import ./fetch-nixpkgs.nix), compiler ? "ghc843" }:

let
  overlays = [];
  pkgs = pkgs-func {
    inherit overlays;
  };
in with pkgs.lib; with pkgs.haskell.lib;
let
  stdhsPackages = pkgs.haskell.packages."${compiler}".extend (self: super: {
#    pipes-text = doJailbreak super.pipes-text;
#    streamly = self.callHackage "streamly" "0.4.1" {};
    base-noprelude = self.callHackage "base-noprelude" "4.11.1.0" {};
  });
  haskellPackages = stdhsPackages.extend (packageSourceOverrides {
    labels = ../labels;
    prelude = ../prelude;
  });
in rec {
  inherit (haskellPackages) labels prelude;
  shell = haskellPackages.shellFor {
    packages = p: with p; [ labels prelude ];
    withHoogle = true;
    buildInputs = with haskellPackages; [
      cabal2nix cabal-install ghcid stylish-haskell hpack hlint
    ];
  };
}
