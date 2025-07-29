{
  config,
  pkgs,
  lib,
  ...
}: let
  HOME = config.home.homeDirectory;
  justfile = config.home.file.".config/user.justfile".source;
in {
  programs.zsh = {
    enable = true;
    autocd = true;
    dotDir = "${HOME}/.config/zsh";
    autosuggestion.enable = true;
    enableCompletion = true;
    history.path = "${HOME}/.zhistory";
    shellAliases = {
      sl = "eza";
      l = "eza";
      ll = "eza -l";
      la = "eza -la";
      ip = "ip --color=auto";
      gita = "git archive --format=zip `git reflog | grep 'HEAD@{0}' | cut -d \" \" -f1 | sed 's/[.]*//g'` > archive.zip";
      # gka = "gitk --all&";
      rm-git-turds = "rm **/(*.orig|*(LOCAL|BASE|REMOTE|BACKUP)*)";
      switch = "cd ${HOME}/code/dotfiles && make hm-switch";
      nsearch = "nix search nixpkgs";
      rgclj = "rg --type clojure";
      j = "just --justfile ${justfile} --working-directory .";
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
        {name = "jeffreytse/zsh-vi-mode";}
      ];
    };

    profileExtra = ''

      if [[ -s ${HOME}/.secrets ]]; then
        source "${HOME}/.secrets"
      fi

      path=(${HOME}/bin
            ${HOME}/.local/bin
            ${HOME}/.cargo/bin
            ${HOME}/.emacs.d/bin
            ${HOME}/.krew/bin
            ${HOME}/.babashka/bbin/bin
            /opt/homebrew/bin
            /opt/homebrew/sbin
            $path)
    '';
    # /usr/local/bin gets added at the front after the above so
    # overrides some of nix bins etc. How to fix?
    initContent = lib.mkMerge [
      (lib.mkOrder 550 ''
        fpath=(~/code/dotfiles/nixpkgs/zfunc $fpath)
      '')

      (lib.mkOrder 600 ''

        # To customize prompt, run `p10k configure` or edit ~/.config/zsh/.p10k.zsh.
        [[ ! -f ~/.config/zsh/.p10k.zsh ]] || source ~/.config/zsh/.p10k.zsh

        function grep-port {
            lsof -n -i4TCP:$1 | grep LISTEN && nc -z localhost $1
        }

        function cdgroot () { cd `git root`; } # relies on a git alias `root = !pwd`

        # awesome!! e.g. $ git <up-arrow> => $ git log
        bindkey "^[[A" history-beginning-search-backward
        bindkey "^[[B" history-beginning-search-forward
        # some keyboards use this:
        bindkey "^[OA" history-beginning-search-backward
        bindkey "^[OB" history-beginning-search-forward

        function pet-select() {
          BUFFER=$(pet search --query "$LBUFFER")
          CURSOR=$#BUFFER
          zle redisplay
        }
        zle -N pet-select
        stty -ixon

        ZVM_VI_INSERT_ESCAPE_BINDKEY=jk
        # Fix binds clobbered by zsh-vim-mode
        fzfkb_path=${pkgs.fzf}/share/fzf/key-bindings.zsh
        zvm_after_init_commands+=('[ -f $fzfkb_path ] && source $fzfkb_path'
                                  'bindkey "^[[A" history-beginning-search-backward'
                                  'bindkey "^[[B" history-beginning-search-forward'
                                  'bindkey "^O" pet-select')
      '')

      (lib.mkOrder 700 ''
            refresh-just-aliases() {
              for recipe in `just --justfile ${justfile} --summary`; do
                alias $recipe="just --justfile ${justfile} --working-directory . $recipe"
              done
            }
            refresh-just-aliases

        # Initialize the completion system
        autoload -U compinit && compinit
      '')

      (lib.mkOrder 800 ''
        nixify() {
          if [ ! -e ./.envrc ]; then
            echo "use nix" > .envrc
            direnv allow
          fi
          if [[ ! -e shell.nix ]] && [[ ! -e default.nix ]]; then
            cat > default.nix <<'EOF'
        with import <nixpkgs> {};
        mkShell {
          nativeBuildInputs = [
            bashInteractive
          ];
        }
        EOF
            ${config.home.sessionVariables.EDITOR} default.nix
          fi
        }
        flakify() {
          if [ ! -e flake.nix ]; then
            nix flake new -t github:nix-community/nix-direnv .
          elif [ ! -e .envrc ]; then
            echo "use flake" > .envrc
            direnv allow
          fi
          ${config.home.sessionVariables.EDITOR} flake.nix
        }

      '')
    ];
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
    defaultOptions = [
      "--preview 'bat -p -f {}'"
      "--height 50%"
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
