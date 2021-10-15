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
    CONFLUENT_HOME = "${HOME}/Downloads/confluent-6.1.1";
  };

  home.file.".amethyst".source = config.lib.file.mkOutOfStoreSymlink ../config/amethyst;
  # home.file.".lein".source = ../.lein;

  programs = {

    zsh = {
      shellAliases = {
        mdfindname = "mdfind -name";
      };
    };

    htop = {
      enable = true;
      settings.tree_view = true;
    };

    jq.enable = true;

  };
}
