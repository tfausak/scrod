{
  inputs = {
    ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs =
    { ghc-wasm-meta, nixpkgs, ... }:
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
          hooky =
            let
              version = "1.0.2";
              assets = {
                aarch64-darwin = {
                  suffix = "darwin-arm64";
                  hash = "sha256-v1VeSGibVHVpO3SyaKKS0+AvZ8wuSIVu5cOhOrsMIT4=";
                };
                aarch64-linux = {
                  suffix = "linux-arm64";
                  hash = "sha256-R51KwBTSgL0x+DBodXApjhuYOQuPOcZwnlEqoNDfJlM=";
                };
                x86_64-darwin = {
                  suffix = "darwin-x86_64";
                  hash = "sha256-DAaWhze5gkSDvJN3oAojct3gIYmtRDzb7ejadxrT5MY=";
                };
                x86_64-linux = {
                  suffix = "linux-x86_64";
                  hash = "sha256-RVPI287fjjqV8lVfgRfnbJsF3JNOKWj8RmbmsCSE8JY=";
                };
              };
              asset = assets.${system};
            in
            pkgs.stdenv.mkDerivation {
              pname = "hooky";
              inherit version;
              src = pkgs.fetchurl {
                url = "https://github.com/brandonchinn178/hooky/releases/download/v${version}/hooky-${version}-${asset.suffix}";
                inherit (asset) hash;
              };
              dontUnpack = true;
              nativeBuildInputs = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
                pkgs.autoPatchelfHook
              ];
              buildInputs = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
                pkgs.gmp
              ];
              installPhase = ''
                install -Dm755 $src $out/bin/hooky
              '';
            };
        in
        {
          default = pkgs.mkShell {
            nativeBuildInputs = [
              ghc-wasm-meta.packages.${system}.default
              hooky
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
