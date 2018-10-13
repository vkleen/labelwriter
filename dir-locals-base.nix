{ pkgs ? import (import ./nix/fetch-nixpkgs.nix) {}, ... }:

let env = import ./shell.nix;
    bufferBuilders = import ./nix/buffer.nix {
                       inherit (pkgs) lib writeText;
                       inherit (pkgs.emacsPackagesNg) inherit-local;
                     };
in target: bufferBuilders.withPackages (   env.buildInputs
                                        ++ env.nativeBuildInputs
                                        ++ env.propagatedBuildInputs) ''
          (setq-local dante-repl-command-line
                      `("nix-shell" "--run" "cabal new-repl --builddir=dist/dante ${target}"
                        ,(concat (projectile-project-root) "/shell.nix")))
          (setq-local dante-target "${target}")
        ''
