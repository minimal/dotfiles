{
  config,
  pkgs,
  ...
}: {
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = false;
  manual.manpages.enable = false; # https://github.com/nix-community/home-manager/issues/3344
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
    eza # exa fork
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
    pet
    ripgrep
    silver-searcher
    tealdeer # fast tldr
    tree
    # unrar # slow compile, nonfree
    wget
    xz

    # devenv
    direnv
    nix-prefetch-git
    nixpkgs-fmt
    alejandra # nix formatter
    shfmt
    pgcli
    poppler_utils # pdftools
    prettyping
    tmux
    zsh

    # programming langs
    openjdk
    clojure
    leiningen
    clj-kondo
    neil
    clojure-lsp
    rlwrap
    shellcheck
    cloc
    scc # fast loc counter
    python3
    pipenv
    uv
    wakatime
    highlight
    bat # fast syntax highlight
    httpie
    sourceHighlight
    cargo
    nodejs
    pre-commit
    # haskell-language-server # started trying to compile ormolu and failing

    awscli2

    kubectl
    kubectx
    kubelogin-oidc
    krew

    kcat
    neovim
    yt-dlp
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
  home.file."bin/elgato".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/code/dotfiles/bin/elgato";
  home.file.".config/nix/nix.conf".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/code/dotfiles/config/nix/nix.conf";
  home.file.".config/user.justfile".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/code/dotfiles/config/user.justfile";

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
