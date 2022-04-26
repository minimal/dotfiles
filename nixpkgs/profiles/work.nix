{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.git = {
    userEmail = "chris.mcdevitt@fundingcircle.com";
    signing = {
      key = "836FC19C63AEC8BE";
      signByDefault = true;
    };
  };
}
