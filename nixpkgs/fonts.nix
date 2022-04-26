{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    # cardo # not found
    # cormorant # not found
    # hack-nerd-font # not found
    # jetbrains-mono-nerd-font # not found
    # vollk rn # error
    (pkgs.callPackage ./pkgs/pragmatapro.nix {})
    (pkgs.callPackage ./pkgs/equity.nix {})
    # alegreya
    # alegreya-sans
    # anonymousPro
    # fira
    # fira-code
    # fira-mono
    # hack-font
    # hasklig
    # inconsolata
    # jetbrains-mono
    # lato
    # monoid
    # open-sans
    # source-code-pro
    # source-sans-pro
    # source-serif-pro
  ];
}
