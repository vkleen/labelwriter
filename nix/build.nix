{ pkgs-func ? import (import ./fetch-nixpkgs.nix), compiler ? "ghc863" }:

let
  overlays = [];
  pkgs = pkgs-func {
    inherit overlays;
  };
  composed = builtins.foldl' (a: acc: b: a (acc b)) (a: a);
in with pkgs.lib; with pkgs.haskell.lib;
let
  enable = x: drv: enableCabalFlag drv x;
  disable = x: drv: disableCabalFlag drv x;
  stdhsPackages = pkgs.haskell.packages."${compiler}".extend (self: super: {
    base-noprelude = self.callHackage "base-noprelude" "4.12.0.0" {};
    diagrams-contrib = doJailbreak super.diagrams-contrib;
  });
  haskellPackages = stdhsPackages.extend (self: super: {
    gloss            = self.callCabal2nixWithOptions "gloss" ../nih/gloss/gloss "-fGLUT" {};
    gloss-algorithms = self.callCabal2nix "gloss-algorithms" ../nih/gloss/gloss-algorithms {};
    gloss-examples   = self.callCabal2nix "gloss-examples" ../nih/gloss/gloss-examples {};
    gloss-raster     = self.callCabal2nix "gloss-raster" ../nih/gloss/gloss-raster {};
    gloss-rendering  = self.callCabal2nix "gloss-rendering" ../nih/gloss/gloss-rendering {};
    rapid            = self.callCabal2nix "rapid" ../nih/rapid {};
    prelude          = self.callCabal2nix "prelude" ../prelude {};
    labelwriter      = composed [
                         (enable "devel")
                         (drv: addBuildDepend drv pkgs.llvm_6)
                       ] (self.callCabal2nixWithOptions "labelwriter" ../labelwriter "-fdevel" {});
  });
in rec {
  inherit (haskellPackages) labels prelude;
  shell = haskellPackages.shellFor {
    packages = p: with p; [ labelwriter prelude ];
    withHoogle = true;
    buildInputs = with haskellPackages; [
      cabal2nix cabal-install ghcid stylish-haskell hpack hlint
    ];
  };
}
