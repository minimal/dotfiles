{ config, pkgs, ... }:
# to enable:
# ln -s mac.nix home.nix
let
  vars = import ./vars.nix;
  HOME = builtins.getEnv "HOME";
in
{
  imports = [
    ./_home.nix
    ./git.nix
  ];
  home.packages = with pkgs; [
    terminal-notifier
    emacsMacport
    tk
    babashka-bin
  ];

  programs.git.userEmail = "chris.mcdevitt@fundingcircle.com";

  home.sessionVariables = {
    EDITOR = "emacsclient --no-wait";
    VISUAL = "emacsclient --no-wait";
    PROJECT_HOME="${HOME}/code";
    CONFLUENT_HOME = "${HOME}/Downloads/confluent-6.1.1";
  };

  home.file.".lein".source = ../.lein;

  programs.zsh = {
    enable = true;
    autocd = true;
    dotDir = ".config/zsh";
    enableAutosuggestions = true;
    enableCompletion = true;
    history.path = "${HOME}/.zhistory";
    shellAliases = {
      sl = "exa";
      l = "exa";
      ll = "exa -l";
      la = "exa -la";
      ip = "ip --color=auto";
      gita = "git archive --format=zip `git reflog | grep 'HEAD@{0}' | cut -d \" \" -f1 | sed 's/[.]*//g'` > archive.zip";
      # gka = "gitk --all&";
      rm-git-turds = "rm **/(*.orig|*(LOCAL|BASE|REMOTE|BACKUP)*)";
    };
    prezto = {
      enable = true;
      pmodules = [
        "archive"
        "autosuggestions"
        "environment"
        "terminal"
        "editor"
        "history"
        "directory"
        "spectrum"
        # "fasd"
        "spectrum"
        "utility"
        "ssh"
        "completion"
        "git"
        "osx"
        "syntax-highlighting"
        "history-substring-search"
        "node"
        # "homebrew"
        # "haskell"
        "ruby"
        "prompt"
      ];
      prompt = {
        theme = "powerlevel10k";
        pwdLength = "long";
      };

      extraConfig = ''
        test -e ${HOME}/.iterm2_shell_integration.zsh && source ${HOME}/.iterm2_shell_integration.zsh
        eval "$(direnv hook zsh)"
      '';
    };


    profileExtra = ''
      . ${HOME}/.nix-profile/etc/profile.d/nix.sh

      if [[ -s ${HOME}/.secrets ]]; then
        source "${HOME}/.secrets"
      fi

      path=(${HOME}/bin
            ${HOME}/.local/bin
            ${HOME}/Downloads/confluent-6.1.1/bin/
            $path)
    '';
    # + builtins.readFile ../Makefile;
    initExtra = ''
      # To customize prompt, run `p10k configure` or edit ~/.config/zsh/.p10k.zsh.
      [[ ! -f ~/.config/zsh/.p10k.zsh ]] || source ~/.config/zsh/.p10k.zsh

      function grep-port {
          lsof -n -i4TCP:$1 | grep LISTEN
      }

      function cdgroot () { cd `git root`; } # relies on a git alias `root = !pwd`

      # awesome!! e.g. $ git <up-arrow> => $ git log
      bindkey "^[[A" history-beginning-search-backward
      bindkey "^[[B" history-beginning-search-forward
      # some keyboards use this:
      bindkey "^[OA" history-beginning-search-backward
      bindkey "^[OB" history-beginning-search-forward
    '';
  };

  programs.htop = {
    enable = true;
    treeView = true;
  };

  programs.jq.enable = true;

  programs.fzf = {
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
  programs.pazi = {
    enable = true;
    enableZshIntegration = true;
  };

}
