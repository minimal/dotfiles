flake := '.\#'`hostname | tr -d .`

nix-packages-tree:
	nix-store -q --tree /nix/var/nix/profiles/per-user/${USER}/profile

hm-switch:
	nix develop -c home-manager switch --flake $(flake)

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

doom-update-flake:
	nix flake lock --update-input doom-emacs

doom-update-sync: doom-update-flake hm-switch
	doom-sync-git
