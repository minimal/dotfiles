let
  HOME = builtins.getEnv "HOME";
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
      switch = "home-manager switch";
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
    # /usr/local/bin gets added at the front after the above so
    # overrides some of nix bins etc. How to fix?
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
}
