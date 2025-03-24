{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };
  outputs = inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "aarch64-darwin" "x86_64-darwin" "aarch64-linux" "x86_64-linux" ];
      imports = [ ];

      perSystem = { pkgs, ... }: 
        let
          fints2ledger = pkgs.haskellPackages.developPackage {
        # packages.default = pkgs.haskell.packages.ghc910.developPackage {
          root = ./.;
          source-overrides = {
            text-format-heavy = pkgs.fetchFromGitHub {
              owner = "MoritzR";
              repo = "text-format-heavy";
              rev = "d7de69f3e2e931ed56a67bbbd2608f778c714302";
              hash = "sha256-OTty1uHIakaKHGfphNBiSzmIn/XDcllDzeaznuKqd5M=";
            };
          };
        };
        in {
          packages.default = fints2ledger; 
        };
    };
}
