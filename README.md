dotfiles
========
[![CI](https://github.com/minimal/dotfiles/actions/workflows/build.yml/badge.svg)](https://github.com/minimal/dotfiles/actions/workflows/build.yml)

    git clone --recurse-submodules git@github.com:minimal/dotfiles.git

Nix home-manager config, dotfiles and emacs.

Emacs config uses Doom [here](doom.d/)

Most packages and dotfiles installed using Nix [flake](flake.nix) with home manager [Nix config](nixpkgs/)

`make hm-switch`

Rest installed with brew
