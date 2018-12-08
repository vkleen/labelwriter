{ pkgs-func ? import (import ./fetch-nixpkgs.nix), compiler ? "ghc844" }:

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
    base-noprelude = self.callHackage "base-noprelude" "4.11.1.0" {};
    # gloss wants GLFW <2 :-(
    # gloss = composed [ (enable "GLFW")
    #                    (disable "GLUT")
    #                    (drv: addBuildDepend drv [self.GLFW-b])
    #                  ]
    #                  super.gloss;
  });
  haskellPackages = stdhsPackages.extend (self: super: {
    gloss            = self.callCabal2nix "gloss" ../nih/gloss/gloss {};
    gloss-algorithms = self.callCabal2nix "gloss-algorithms" ../nih/gloss/gloss-algorithms {};
    gloss-examples   = self.callCabal2nix "gloss-examples" ../nih/gloss/gloss-examples {};
    gloss-raster     = self.callCabal2nix "gloss-raster" ../nih/gloss/gloss-raster {};
    gloss-rendering  = self.callCabal2nix "gloss-rendering" ../nih/gloss/gloss-rendering {};
    prelude          = self.callCabal2nix "prelude" ../prelude {};
    labels           = addBuildDepend (self.callCabal2nix "labels" ../labels {}) pkgs.llvm;
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
