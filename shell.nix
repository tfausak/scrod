# https://nix.dev/tutorials/first-steps/declarative-shell.html
let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/fa83fd837f3098e3e678e6cf017b2b36102c7211.tar.gz";
    sha256 = "sha256-e7VO/kGLgRMbWtpBqdWl0uFg8Y2XWFMdz0uUJvlML8o=";
  };
  pkgs = import nixpkgs {};
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
  ];
}
