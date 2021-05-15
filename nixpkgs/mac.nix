{ config, pkgs, ... }:
# to enable:
# ln -s mac.nix home.nix
{
  imports = [ ./_home.nix ];
  home.packages = with pkgs; [
    terminal-notifier
    emacsMacport
  ];

  home.sessionVariables = {
    EDITOR = "emacsclient";
    VISUAL = "emacsclient";
  };

  home.file.".lein".source = ../.lein;
}
