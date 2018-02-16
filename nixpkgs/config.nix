let pkgs = import <nixpkgs> {};
in
{
  allowUnfree = true;
  packageOverrides = pkgs: with pkgs; {
  };
}
