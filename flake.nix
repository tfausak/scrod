{
  inputs = {
    ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
    hooky.url = "github:tfausak/hooky-nix";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs =
    { ghc-wasm-meta, hooky, nixpkgs, ... }:
    let
      forAllSystems = nixpkgs.lib.genAttrs [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
    in
    {
      devShells = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = pkgs.mkShell {
            nativeBuildInputs = [
              ghc-wasm-meta.packages.${system}.default
              hooky.packages.${system}.default
              pkgs.cabal-install
              pkgs.fzf
              pkgs.gh
              pkgs.haskell.compiler.native-bignum.ghc9141
              pkgs.haskellPackages.cabal-gild
              pkgs.hlint
              pkgs.jq
              pkgs.nixfmt
              pkgs.ormolu
              pkgs.ripgrep
              pkgs.wabt
            ];
          };
        }
      );
    };
}
