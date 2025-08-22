set shell := ["zsh", "-uc"]
flake := ".#" +`hostname | tr -d .`

default:
    echo {{ flake }}

nix-packages-tree:
    nix-store -q --tree /nix/var/nix/profiles/per-user/${USER}/profile

hm-switch:
    nix develop -c home-manager switch --flake {{ flake }}

hm-config-setup:
    mkdir -p ~/.config/nix
    ln -sfn $(PWD)/config/nix/nix.conf ~/.config/nix/nix.conf

hm-bootstrap: hm-config-setup hm-switch

hm-firstrun: hm-config-setup
    nix-shell -p nixUnstable --command "nix --experimental-features 'nix-command flakes' develop -c home-manager switch --flake $(flake)"

hm-packages:
    nix develop -c home-manager packages

nix-gc-30d:
    nix-collect-garbage --delete-older-than 30d

nix-repair-store:
    nix-store --verify --check-contents --repair

nix-install-unstable:
    nix-env -f '<nixpkgs>' -iA nixUnstable

nix-reg-pin-latest-nixpkgs:
    nix registry remove nixpkgs
    nix registry pin flake:nixpkgs

@_nix-flake-lock:
    nix flake lock

nixpkgs-update-flake: _nix-flake-lock
    nix flake update nixpkgs home-manager

nix-update-safe:
    ./bin/nixpkgs-update-safe

nix-update-safe-nixpkgs-only:
    ./bin/nixpkgs-update-safe --nixpkgs-only

nix-check-format:
    alejandra --check **/*nix

doom-update-flake: _nix-flake-lock
    nix flake update doom-emacs

doom-update-sync: doom-update-flake hm-switch
    doom-sync-git

doom-trash-packages:
    trash ~/.emacs.d/.local/straight/

git-submodules:
    git submodule update --init

brew-bundle:
    brew bundle

nix-latest-git-version:
    curl -s https://raw.githubusercontent.com/NixOS/nixpkgs/master/pkgs/applications/version-management/git/default.nix | grep '^  version ='

save-space: nix-gc-30d
    doom gc
