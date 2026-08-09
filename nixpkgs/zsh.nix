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
      en = "emacsclient -n"; # open in emacs gui
      nv = "nvim";
      gls = "${HOME}/.nix-profile/bin/ls";
      g = "git";
      sl = "eza";
      l = "eza";
      ll = "eza -l";
      la = "eza -la";
      ltr = "eza -l --sort time --reverse";
      ip = "ip --color=auto";
      gita = "git archive --format=zip `git reflog | grep 'HEAD@{0}' | cut -d \" \" -f1 | sed 's/[.]*//g'` > archive.zip";
      # gka = "gitk --all&";
      rm-git-turds = "rm **/(*.orig|*(LOCAL|BASE|REMOTE|BACKUP)*)";
      switch = "cd ${HOME}/code/dotfiles && rm -f ${HOME}/.config/zsh/.zcompdump*(N) && make hm-switch";
      emacsd = "emacs --daemon";
      nsearch = "nix search nixpkgs";
      nsearchx = "(){ nix search nixpkgs \"^$1$\";}";
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
        zstyle ':prezto:module:git:alias' skip 'yes'
      '';
    };

    zplug = {
      enable = true;
      plugins = [
        {name = "jeffreytse/zsh-vi-mode";}
      ];
    };

    profileExtra = ''

      if [[ -s ${HOME}/.secrets/secrets ]]; then
        source "${HOME}/.secrets/secrets"
      fi

      source ${HOME}/.config/shell/paths.sh
      export BASH_ENV="${HOME}/.config/shell/paths.sh"
    '';
    # /usr/local/bin gets added at the front after the above so
    # overrides some of nix bins etc. How to fix?
    initContent = lib.mkMerge [
      (lib.mkOrder 550 ''
        fpath=(~/code/dotfiles/nixpkgs/zfunc $fpath)

        # Required for gpg-agent to launch pinentry-mac for commit signing.
        export GPG_TTY=$(tty)
      '')

      (lib.mkOrder 600 ''

        # To customize prompt, run `p10k configure` or edit ~/.config/zsh/.p10k.zsh.
        [[ ! -f ~/.config/zsh/.p10k.zsh ]] || source ~/.config/zsh/.p10k.zsh

        # Auto-start Emacs daemon (WSL2), then connect with client
        function ec() {
            if ! emacsclient -e '(+ 1 1)' 2>/dev/null >/dev/null; then
                # Clean up stale server socket from a previous crashed daemon
                rm -f "${TMPDIR:-/tmp}/emacs$(id -u)/server" "${HOME}/.emacs.d/server/server" 2>/dev/null
                # PGTK Emacs on WSL crashes when the Wayland/X display disconnects.
                # Start daemon without display to keep it alive across terminal sessions.
                env -u WAYLAND_DISPLAY -u DISPLAY emacs --daemon
            fi
            emacsclient -nw "$@"
        }

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

      function sesh-sessions() {
  {
    exec </dev/tty
    exec <&1
    local session
    session=$(sesh list -t -c | fzf --height 40% --reverse --border-label ' sesh ' --border --prompt '⚡  ')
    zle reset-prompt > /dev/null 2>&1 || true
    [[ -z "$session" ]] && return
    sesh connect $session
  }
      }

zle     -N             sesh-sessions
bindkey -M emacs '\es' sesh-sessions
bindkey -M vicmd '\es' sesh-sessions
bindkey -M viins '\es' sesh-sessions
      '')

      (lib.mkOrder 700 ''
            refresh-just-aliases() {
              for recipe in `just --justfile ${justfile} --summary`; do
                alias $recipe="just --justfile ${justfile} --working-directory . $recipe"
              done
            }
            refresh-just-aliases

        # worktrunk (wt) shell integration: defines the `wt` cd/exec wrapper
        # and a lazy completer. Sourcing it here defines the functions, but its
        # internal `compdef` registration is wiped when home-manager runs
        # prezto's compinit *after* all initContent. So we (re)register the
        # completion on the first prompt via a one-shot precmd, by which point
        # prezto's compinit has run and `compdef` is live.
        source ${HOME}/code/dotfiles/nixpkgs/wt-integration.zsh
        _wt_register_completion() {
          (( $+functions[compdef] )) && compdef _wt_lazy_complete wt
          add-zsh-hook -d precmd _wt_register_completion
        }
        autoload -Uz add-zsh-hook && add-zsh-hook precmd _wt_register_completion
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

  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
  };
}
