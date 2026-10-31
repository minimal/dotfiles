{pkgs, ...}: {
  # JavaScript / Node toolchain.
  home.packages = with pkgs; [
    # default `nodejs` is nodejs_24, stuck at 24.21.0 in nixpkgs
    nodejs_26
    node-gyp
    bun
    pnpm
  ];
}
