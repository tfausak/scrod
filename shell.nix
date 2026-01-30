# https://nix.dev/tutorials/first-steps/declarative-shell.html
let
  pkgs = import ./nixpkgs.nix;
in
pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.cabal-install
    pkgs.curl
    pkgs.git
    pkgs.haskell-language-server
    pkgs.haskellPackages.cabal-gild
    pkgs.haskellPackages.weeder
    pkgs.hlint
    pkgs.jq
    pkgs.nixfmt-rfc-style
    pkgs.ormolu
    pkgs.pkg-config
    pkgs.vim
    pkgs.zlib
    pkgs.zstd
  ];
}
