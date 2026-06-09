{pkgs, ...}: {
  # JavaScript / Node toolchain.
  home.packages = with pkgs; [
    nodejs
    node-gyp
    bun
    pnpm
  ];
}
