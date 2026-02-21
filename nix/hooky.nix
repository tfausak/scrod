{ pkgs }:
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
  asset = assets.${pkgs.system};
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
}
