{
  config,
  lib,
  pkgs,
  ...
}: let
  sharedLib = import ../lib.nix {inherit lib;};
in {
  programs.git = {
    settings.user.email = sharedLib.reverseString "moc.elcricgnidnuf@ttivedcm.sirhc";
    signing = {
      key = "836FC19C63AEC8BE";
      signByDefault = true;
      format = "openpgp";
    };
  };
}
