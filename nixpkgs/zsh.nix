{ config, pkgs, ... }:
let
  HOME = config.home.homeDirectory;
in
{
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
      switch = "cd ${HOME}/code/dotfiles && make hm-switch";
      nsearch = "nix search nixpkgs";
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
        # "ruby"
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

    zplug = {
      enable = true;
      plugins = [
        { name = "jeffreytse/zsh-vi-mode"; }
      ];
    };

    profileExtra = ''

      if [[ -s ${HOME}/.secrets ]]; then
        source "${HOME}/.secrets"
      fi

      path=(${HOME}/bin
            ${HOME}/.local/bin
            ${HOME}/.cargo/bin
            ${HOME}/Downloads/confluent-6.2.1/bin/
            /opt/homebrew/bin
            /opt/homebrew/sbin
            $path)
    '';
    # /usr/local/bin gets added at the front after the above so
    # overrides some of nix bins etc. How to fix?

    initExtraBeforeCompInit = ''
      fpath=(~/code/dotfiles/nixpkgs/zfunc $fpath)
    '';

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

      # Fix binds clobbered by zsh-vim-mode
      fzfkb_path=${pkgs.fzf}/share/fzf/key-bindings.zsh
      zvm_after_init_commands+=('[ -f $fzfkb_path ] && source $fzfkb_path'
                                'bindkey "^[[A" history-beginning-search-backward'
                                'bindkey "^[[B" history-beginning-search-forward')
    '';
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
    defaultOptions = [
      "--preview 'fzfprev {}'"
      # "--height 40%"
      "--layout=reverse"
      "--border"
      "--inline-info"
    ];
  };

  # autojump clone in rust
  programs.pazi = {
    enable = true;
    enableZshIntegration = true;
  };
}
