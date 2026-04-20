{
  config,
  lib,
  pkgs,
  ...
}: let
  sharedLib = import ../lib.nix {inherit lib;};
in {
  programs.git = {
    settings.user.email = sharedLib.reverseString "moc.liamg@ttivedcmrehpotsirhc";
    signing = {
      key = "3A042C6B67C88936D05AD968288F081B1A54FB2A";
      format = "openpgp";
    };
  };
}
