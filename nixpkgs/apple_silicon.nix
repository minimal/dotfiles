# use x86_64-darwin for packages broken on apple silicon
{nixpkgs}: (
  final: prev: let
    isM1 = prev.stdenv.hostPlatform.system == "aarch64-darwin";
    pkgs_x86_64 = import nixpkgs {localSystem = "x86_64-darwin";};
    pkgs =
      if isM1
      then pkgs_x86_64
      else prev;
  in {
    emacsMacport = pkgs.emacsMacport;
    babashka = pkgs.babashka;
    clj-kondo = pkgs.clj-kondo;
    kafkacat = pkgs.kafkacat;
  }
)
