let pkgs = import <nixpkgs> { };
in
{
  allowUnfree = true;
  packageOverrides = pkgs: with pkgs; {
    myPackages = pkgs.buildEnv {
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
        # exa # fails compile
      ];
      pathsToLink = [ "/share" "/bin" "/Applications"];
      extraOutputsToInstall = [ "man" "doc" ];
    };
  };
}
