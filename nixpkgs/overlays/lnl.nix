self: super:

{
  userPackages = super.userPackages or {} // {
    hello = super.hello;
    # add more packages here...

    nix-rebuild = super.writeScriptBin "nix-rebuild"
      ''
        #!${super.stdenv.shell}
        exec nix-env -f '<nixpkgs>' -r -iA userPackages
      '';
  };
}
