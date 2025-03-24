{ pkgs, lib, config, inputs, ... }:

{
  cachix.enable = false;

  packages = with pkgs; [ 
    python312
    python312Packages.fints
    python312Packages.mt-940

    haskellPackages.fourmolu
    ];

  languages.haskell = {
    enable = true;
    package = pkgs.haskell.compiler.ghc910;
  };
}
