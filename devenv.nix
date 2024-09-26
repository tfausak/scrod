{ pkgs, ... }:

{
  languages.haskell.enable = true;
  languages.haskell.package = pkgs.haskell.compiler.ghc910;

  packages = [
    pkgs.haskellPackages.cabal-gild
    pkgs.hlint
    pkgs.ormolu
  ];
}
