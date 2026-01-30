{
  description = "A Nix flake for fs-presentatin";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        hpkgs = pkgs.haskell.packages.ghc912;
        py = pkgs.python313Packages;
      in
      {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            gfortran15
            hpkgs.ghc
            py.numpy
            py.scipy
            py.pandas
            py.matplotlib
          ];
        };
      }
    );
}
