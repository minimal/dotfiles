{ config, lib, pkgs, ... }:

let

  emacsSyncScript = pkgs.writeScriptBin "doom-sync-git" ''
    #!${pkgs.runtimeShell}
    export PATH=$PATH:${lib.makeBinPath [ pkgs.git pkgs.sqlite pkgs.unzip ]}
    if [ ! -d $HOME/.emacs.d/.git ]; then
      mkdir -p $HOME/.emacs.d
      git -C $HOME/.emacs.d init
    fi
    if [ $(git -C $HOME/.emacs.d rev-parse HEAD) != ${pkgs.doomEmacsRevision} ]; then
      git -C $HOME/.emacs.d fetch https://github.com/hlissner/doom-emacs.git || true
      git -C $HOME/.emacs.d checkout ${pkgs.doomEmacsRevision} || true
      $HOME/.emacs.d/bin/doom sync || true
      YES=1 FORCE=1 $HOME/.emacs.d/bin/doom sync -u &
    fi
  '';
in

{

  home.packages = with pkgs; [
    # emacsMacport
    emacsGcc
    emacsSyncScript
    ripgrep
  ];
}