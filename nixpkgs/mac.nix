{ config, pkgs, ... }:
let
  HOME = config.home.homeDirectory;
in
{
  imports = [
    ./_home.nix
    ./git.nix
    ./zsh.nix
  ];

  home.packages = with pkgs; [
    terminal-notifier
    tk
    # babashka # broken
    babashka-bin
    pinentry_mac
  ];

  home.sessionVariables = {
    EDITOR = "emacsclient --no-wait";
    VISUAL = "emacsclient --no-wait";
    PROJECT_HOME = "${HOME}/code";
    CONFLUENT_HOME = "${HOME}/Downloads/confluent-6.2.1";
    EMACS = "/opt/homebrew/opt/emacs-mac/Emacs.app/Contents/MacOS/Emacs";
  };

  home.file.".amethyst".source = config.lib.file.mkOutOfStoreSymlink ../config/amethyst;
  # home.file.".lein".source = ../.lein;

  programs = {

    zsh = {
      shellAliases = {
        mdfindname = "mdfind -name";
        emacs = "/opt/homebrew/opt/emacs-mac/Emacs.app/Contents/MacOS/Emacs";
        emacsclient = "/opt/homebrew/bin/emacsclient";
      };
    };
  };
}
