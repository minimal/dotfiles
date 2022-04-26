{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = false;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  # home.username = builtins.getEnv "USER";
  # home.homeDirectory = builtins.getEnv "HOME";

  home.packages = with pkgs; [
    # pkgs is the set of all packages in the default home.nix implementation
    home-manager
    nix

    # cmdline / system programs
    aspell
    aspellDicts.en
    cmake
    coreutils
    curl
    delta
    difftastic
    exa # ls alt
    fd
    findutils
    fpp
    fzf
    gawk
    getopt
    gnumake
    gnused
    gnutar
    gnutls
    hello
    indent
    jq
    just
    lsd # ls alt
    nox
    pet
    ripgrep
    silver-searcher
    tealdeer # fast tldr
    tree
    # unrar # slow compile, nonfree
    wget
    xz

    direnv
    nix-prefetch-git
    nixpkgs-fmt
    alejandra # nix formatter
    pgcli
    poppler_utils # pdftools
    prettyping
    tmux
    zsh

    # programming langs
    openjdk
    clojure
    leiningen
    # boot
    clj-kondo # broken waiting for fix to merge
    neil
    clojure-lsp
    # joker
    rlwrap
    shellcheck
    cloc
    scc # fast loc counter
    python3
    pipenv
    wakatime
    highlight
    bat # fast syntax highlight
    httpie
    sourceHighlight
    cargo

    awscli2
    kubectl
    kafkacat
    neovim
    youtube-dl
    gnupg
    #pinentry
    #sshrc
    prettyping
    fontconfig
    htop
    btop
    # dhall
    # dhall-json
    # dhall-text
    lastpass-cli

    postgresql
  ];

  programs = {
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
    htop = {
      enable = true;
      settings.tree_view = true;
    };
    jq.enable = true;
  };

  # Raw configuration files
  home.file.".tmux.conf".source = ../config/tmux.conf;
  home.file.".config/git/gitignore".source = ../config/git/gitignore;
  home.file."bin/fzfprev".source = ../bin/fzfprev;
  home.file."bin/qfind".source = ../bin/fzfprev;
  home.file."bin/cmd_exists".source = ../bin/cmd_exists;
  home.file.".config/nix/nix.conf".source = config.lib.file.mkOutOfStoreSymlink ../config/nix/nix.conf;

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  # home.stateVersion = "21.05";
}
