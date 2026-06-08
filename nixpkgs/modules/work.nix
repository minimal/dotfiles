{pkgs, ...}: {
  # Work-only packages. A host importing this must also allow the relevant
  # unfree packages via the `unfree` arg to mkHomeConfig in flake.nix.
  home.packages = with pkgs; [
    terraform # unfree
    kcat
  ];
}
