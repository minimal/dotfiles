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
        gnutar
        gnused
        indent
        getopt
        gnutls
        cmake
        curl
        nox
        silver-searcher
        ripgrep
        xz
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
        clojure
        leiningen
        boot
        rlwrap
        shellcheck
        cloc
        loc
        terminal-notifier
        python3
        pipenv
        python36Packages.virtualenv
        python36Packages.virtualenvwrapper
        direnv
        nix-repl
        pgcli
        tmux
        zsh
        emacs25Macport
        neovim
        youtube-dl
        gnupg
        pinentry
        joker
        sshrc
        prettyping
        fontconfig
        htop
        wakatime
        dhall
        lastpass-cli
        highlight
        httpie
        # exa # fails compile
      ];
      pathsToLink = [ "/share" "/bin" "/Applications"];
      extraOutputsToInstall = [ "man" "doc" ];
    };
    yarn = super.yarn;
    poppler-utils = super.poppler_utils;
    kafkacat = super.kafkacat;
    postgresql96 = super.postgresql96;
    # nixUnstable= super.nixUnstable;
    aspell = super.aspell;
    aspell-en = super.aspellDicts.en;
    ansible = super.ansible;
    pandoc = super.pandoc;
  };
}
