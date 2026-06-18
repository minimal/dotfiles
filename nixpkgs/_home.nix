{
  config,
  pkgs,
  ...
}: {
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = false;
  manual.manpages.enable = false; # https://github.com/nix-community/home-manager/issues/3344
  # Universal base: lightweight tools every host gets. Heavier/optional
  # stacks (langs, cloud, work) live in ./modules/* and are opted into
  # per-host via baseModules in flake.nix.
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
    ast-grep
    tealdeer # fast tldr
    tree
    # unrar # slow compile, nonfree
    wget
    xz
    ouch # easy compressions
    yt-dlp

    # devenv (universal)
    direnv
    nix-prefetch-git
    nixpkgs-fmt
    alejandra # nix formatter
    nixd
    shfmt
    poppler-utils # pdftools
    prettyping
    tmux
    zsh
    bash
    neovim
    gnupg
    #pinentry
    #sshrc
    fontconfig
    htop
    btop

    # viewers
    highlight
    bat # fast syntax highlight
    sourceHighlight
    glow # markdown viewer
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
  home.file."bin/csv2md".source = ../bin/csv2md.bb;
  home.file."bin/elgato".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/code/dotfiles/bin/elgato";
  home.file.".config/nix/nix.conf".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/code/dotfiles/config/nix/nix.conf";
  home.file.".config/user.justfile".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/code/dotfiles/config/user.justfile";
}
