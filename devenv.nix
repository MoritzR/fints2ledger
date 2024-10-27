{ pkgs, lib, config, inputs, ... }:

{
  cachix.enable = false;

  packages = with pkgs; [ 
    git
    python312
    python312Packages.fints
    python312Packages.mt-940
    ];

  languages.haskell.enable = true;
}
