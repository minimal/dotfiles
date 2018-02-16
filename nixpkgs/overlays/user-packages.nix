self: super:
{
  userPackages = super.userPackages or {} // {
    # add more packages here...
    nix-rebuild = super.writeScriptBin "nix-rebuild"
    ''
      #!${super.stdenv.shell}
      exec nix-env -f '<nixpkgs>' -r -iA userPackages
    '';
    nix-user-packages = super.writeScriptBin "nix-user-packages"
    ''
      #!${super.stdenv.shell}
      exec nix-env -f '<nixpkgs>' -iA userPackages
    '';

    myPackages = with super; super.buildEnv {
      name = "my-packages";
      paths = [
        aspell
        nox
        silver-searcher
        ## gitk not working:
        # tk
        # tcl
        # gitAndTools.gitFull
        git-crypt
        jq
        coreutils
        findutils
        gawk
        fd
        fzf
        fpp
        tree
        wget
        unrar # slow compile, nonfree
        leiningen
        rlwrap
        shellcheck
        cloc
        terminal-notifier
        python3
        direnv
        pgcli
        tmux
        zsh
        emacs25Macport
        youtube-dl
        gnupg
        pinentry
        joker
        sshrc
        # exa # fails compile
      ];
      pathsToLink = [ "/share" "/bin" "/Applications"];
      extraOutputsToInstall = [ "man" "doc" ];
    };
    yarn = super.yarn;
    poppler = super.poppler;
  };
}
