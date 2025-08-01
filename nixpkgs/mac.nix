{
  config,
  pkgs,
  ...
}: let
  HOME = config.home.homeDirectory;
in {
  imports = [
    ./_home.nix
    ./git.nix
    ./zsh.nix
  ];

  home.packages = with pkgs; [
    terminal-notifier
    tk
    babashka
    pinentry_mac
    granted
  ];

  home.sessionVariables = {
    EDITOR = "emacsclient --no-wait";
    VISUAL = "emacsclient --no-wait";
    PROJECT_HOME = "${HOME}/code";
    EMACS = "/opt/homebrew/opt/emacs-mac/Emacs.app/Contents/MacOS/Emacs";
    GRANTED_ALIAS_CONFIGURED = "true";
    DOD_HOME = "${HOME}/code/fc/originations-toolbox/robert.johnson/decision-output-downloader";
  };

  home.file.".amethyst".source = config.lib.file.mkOutOfStoreSymlink ../config/amethyst;
  # home.file.".lein".source = ../.lein;

  programs = {
    zsh = {
      shellAliases = {
        mdfindname = "mdfind -name";
        emacs = "/opt/homebrew/opt/emacs-mac/Emacs.app/Contents/MacOS/Emacs";
        emacsclient = "/opt/homebrew/bin/emacsclient";
        assume = "source /opt/homebrew/bin/assume";
      };
    };
  };
}
