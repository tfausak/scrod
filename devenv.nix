{ pkgs, ... }:

{
  languages.haskell.enable = true;
  languages.haskell.package = pkgs.haskell.compiler.ghc910;

  packages = [
    pkgs.haskellPackages.cabal-gild
    pkgs.ghcid
    pkgs.hlint
    pkgs.ormolu
  ];
}
