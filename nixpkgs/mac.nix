{ config, pkgs, ... }:
# to enable:
# ln -s mac.nix home.nix
let
  vars = import ./vars.nix;
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
    emacsMacport
    tk
    babashka
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
    git.userEmail = "chris.mcdevitt@fundingcircle.com";

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

    fzf = {
      enable = true;
      enableZshIntegration = true;
      defaultOptions = [
        "--preview 'fzfprev {}'"
        "--height 40%"
        "--layout=reverse"
        "--border"
        "--inline-info"
        "--color 'fg:#${vars.colors.White}'" # Text
        "--color 'bg:#${vars.colors.Black}'" # Background
        "--color 'preview-fg:#${vars.colors.White}'" # Preview window text
        "--color 'preview-bg:#${vars.colors.Black}'" # Preview window background
        "--color 'hl:#${vars.colors.Yellow}'" # Highlighted substrings
        "--color 'fg+:#${vars.colors.Blue}'" # Text (current line)
        "--color 'bg+:#${vars.colors.Grey}'" # Background (current line)
        "--color 'gutter:#${vars.colors.Grey}'" # Gutter on the left (defaults to bg+)
        "--color 'hl+:#${vars.colors.Magenta}'" # Highlighted substrings (current line)
        "--color 'info:#${vars.colors.Magenta}'" # Info line (match counters)
        "--color 'border:#${vars.colors.Blue}'" # Border around the window (--border and --preview)
        "--color 'prompt:#${vars.colors.White}'" # Prompt
        "--color 'pointer:#${vars.colors.Magenta}'" # Pointer to the current line
        "--color 'marker:#${vars.colors.Magenta}'" # Multi-select marker
        "--color 'spinner:#${vars.colors.Magenta}'" # Streaming input indicator
        "--color 'header:#${vars.colors.White}'" # Header
      ];
    };

    # autojump clone in rust
    pazi = {
      enable = true;
      enableZshIntegration = true;
    };
  };
}
