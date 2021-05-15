{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = builtins.getEnv "USER"; 
  home.homeDirectory = builtins.getEnv "HOME"; 

  home.packages = with pkgs; [
    # pkgs is the set of all packages in the default home.nix implementation

    # cmdline / system programs
    aspell
    aspellDicts.en
    cmake
    coreutils
    curl
    exa # ls alt
    fd
    findutils
    fpp
    fzf
    gawk
    getopt
    git-crypt
    gnused
    gnutar
    gnutls
    hello
    indent
    jq
    lsd # ls alt
    nox
    ripgrep
    silver-searcher
    tree
    unrar # slow compile, nonfree
    wget
    xz

    ansible
    direnv
    nix-prefetch-git
    nixpkgs-fmt
    pgcli
    poppler_utils # pdftools
    prettyping
    tmux
    zsh

    # programming langs
    clojure
    leiningen
    # boot
    clj-kondo # if fails run `sudo chmod 1777 /tmp`
    # joker
    rlwrap
    shellcheck
    cloc
    scc # fast loc counter
    python3
    pipenv
    wakatime
    highlight
    httpie

    kafkacat
    neovim
    youtube-dl
    gnupg
    #pinentry
    #sshrc
    prettyping
    fontconfig
    htop
    # dhall
    # dhall-json
    # dhall-text
    # lastpass-cli

    postgresql
  ];

  # Raw configuration files
  home.file.".tmux.conf".source = ../.tmux.conf;
  home.file.".gitignore".source = ../gitignore;
  # home.file.".gitconfig".source = ../.gitconfig;

  # Git config using Home Manager modules
  programs.git = {
    enable = true;
    userName = "Chris McDevitt";
    userEmail = "chris.mcdevitt@fundingcircle.com";

    signing = {
      key = "3A042C6B67C88936D05AD968288F081B1A54FB2A";
      signByDefault = false;
    };

    aliases = {
      lol               = "log --oneline --graph --decorate";
      lola              = "log --graph --decorate --oneline --all";
      ci                = "commit";
      st                = "status -sb";
      co                = "checkout";
      oneline           = "log --pretty=oneline";
      br                = "branch";
      la                = "log --pretty=\"format:%ad %h (%an): %s\" --date=short";
      pg                = "push origin HEAD:refs/for/master";
      nm                = "log --no-merges";
      dw                = "diff --word-diff";
      d                 = "diff --color-words --abbrev";
      dc                = "diff --cached";
      m                 = "merge";
      mnff              = "merge --no-ff";
      pop               = "stash pop";
      tags              = "!git for-each-ref --sort=-taggerdate --format '%(refname:short) %(taggerdate:relative)' refs/tags";
      tagsonly          = "!git for-each-ref --sort=-taggerdate --format '%(refname:short)' refs/tags";
      pick              = "!sh -c 'git log -S$1 -p' -  # show log diffs that add/remove arg string";
      graphviz          = "!f() { echo 'digraph git {' ; git log --pretty='format:  %h -> { %p }' \"$@\" | sed 's/[0-9a-f][0-9a-f]*/\"&\"/g' ; echo '}'; }; f";
      root              = "!pwd";
      pu                = "push --follow-tags";
      permission-reset  = "!git diff -p -R | grep -E \"^(diff|(old|new) mode)\" | git apply";
      sync              = "!git stash && git pull --rebase && git stash pop";
    };
    extraConfig = {
      color = {
        status      = "auto";
        diff        = "auto";
        branch      = "auto";
        interactive = "auto";
        ui          = "auto";
        sh          = "auto";
      };
      core = {
        editor          = "emacsclient  -nw";
        excludesfile    = "~/.gitignore";
        attributesfile  = "~/.gitattributes";
      };

      help.autocorrect = 10;
      rerere.enabled = true;
      branch.autosetuprebase = "always";
      push.default = "simple";
      pull.default = "only";

      commit = {
        template = "~/code/dotfiles/git_commit_msg.txt";
        gpgsign = false;
      };

      github = {
        user = "minimal";
      };

      url."git@github.com:".pushInsteadOf = "https://github.com/";
      diff."clojure".xfuncname = "(^\\(.*|\\s*\\(defn.*)";
    };
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.05";
}
