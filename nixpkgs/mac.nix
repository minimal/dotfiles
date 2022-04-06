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
  ];

  home.sessionVariables = {
    EDITOR = "emacsclient --no-wait";
    VISUAL = "emacsclient --no-wait";
    PROJECT_HOME = "${HOME}/code";
    CONFLUENT_HOME = "${HOME}/Downloads/confluent-6.2.1";
    EMACS = "/Applications/Emacs.app/Contents/MacOS/Emacs";
  };

  home.file.".amethyst".source = config.lib.file.mkOutOfStoreSymlink ../config/amethyst;
  # home.file.".lein".source = ../.lein;

  programs = {

    zsh = {
      shellAliases = {
        mdfindname = "mdfind -name";
        emacs = "/Applications/Emacs.app/Contents/MacOS/Emacs";
        emacsclient = "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient";
        e = "emacsclient --no-wait"; # override for broken zprezto

      };
    };
  };
}
