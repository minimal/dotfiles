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
        # ripgrep # moved to home-manager
        ## gitk not working:
        # tk
        # tcl
        # gitAndTools.gitFull
        # jq
        #clojure
        # python36Packages.virtualenv 10may
        # python36Packages.virtualenvwrapper 10may
      ];
      pathsToLink = [ "/share" "/bin" "/Applications"];
      extraOutputsToInstall = [ "man" "doc" ];
    };
    # yarn = super.yarn;
    # poppler-utils = super.poppler_utils; # pdf stuff
    # postgresql96 = super.postgresql96;
    # nixUnstable= super.nixUnstable;
    # aspell = super.aspell;
    # aspell-en = super.aspellDicts.en;
    # ansible = super.ansible;
    #pandoc = super.pandoc;
    #asciinema = super.asciinema;
    # joker = super.joker;
    # gitandtools = super.gitAndTools.gitFull;

    # myemacspackages =  super: epkgs: with epkgs;
    # [
    #   visual-regexp
    #   visual-regexp-steroids
    # ];

    # myinstallepkgs =  super.emacsPackagesNgGen [  epkgs.visual-regexp];
  };
}
