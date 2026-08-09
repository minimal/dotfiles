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
    ncdu

    # devenv (universal)
    direnv
    nix-prefetch-git
    nixpkgs-fmt
    alejandra # nix formatter
    nixd
    shfmt
    poppler-utils # pdftools
    prettyping
    restic # encrypted, deduplicated, incremental backups
    tmux
    sesh
    zsh
    bash
    gnupg
    #sshrc
    fontconfig
    htop
    btop
    starship
    gum
    fastfetch

    # viewers
    highlight
    bat # fast syntax highlight
    sourceHighlight
    glow # markdown viewer
    tuicr
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
    neovim = {
      enable = true;
      sideloadInitLua = true;
      withPython3 = true;
      withRuby = false;
      plugins = [pkgs.vimPlugins.oil-nvim pkgs.vimPlugins.nvim-web-devicons pkgs.vimPlugins.mini-nvim];
    };
  };

  # Raw configuration files
  home.file.".tmux.conf".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/code/dotfiles/config/tmux.conf";
  home.file.".config/nvim/init.lua".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/code/dotfiles/config/nvim/init.lua";
  home.file.".config/nvim/vimrc.vim".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/code/dotfiles/config/nvim/vimrc.vim";
  home.file.".config/shell/paths.sh".source = ../config/shell/paths.sh;
  home.file.".config/git/gitignore".source = ../config/git/gitignore;
  home.file."bin/fzfprev".source = ../bin/fzfprev;
  home.file."bin/qfind".source = ../bin/qfind;
  home.file."bin/cmd_exists".source = ../bin/cmd_exists;
  home.file."bin/csv2md".source = ../bin/csv2md.bb;
  home.file."bin/elgato".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/code/dotfiles/bin/elgato";
  home.file."bin/zf".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/code/dotfiles/bin/zf";
  home.file.".config/nix/nix.conf".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/code/dotfiles/config/nix/nix.conf";
  home.file.".config/user.justfile".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/code/dotfiles/config/user.justfile";
  home.file.".config/restic/include.conf".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/code/dotfiles/config/restic/include.conf";
  home.file.".config/restic/exclude.conf".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/code/dotfiles/config/restic/exclude.conf";
  home.file."bin/restic-backup.sh".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/code/dotfiles/bin/restic-backup.sh";
}
