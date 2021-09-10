prefix := '.\#'
flake := $(prefix)`hostname | tr -d .`

nix-packages-tree:
	nix-store -q --tree /nix/var/nix/profiles/per-user/${USER}/profile

hm-switch:
	nix --experimental-features 'nix-command flakes' develop -c home-manager switch --flake $(flake)

hm-config-setup:
	ln -sfn $(PWD)/config/nix/nix.conf ~/.config/nix/nix.conf

hm-bootstrap: hm-config-setup hm-switch

hm-packages:
	nix develop -c home-manager packages

nix-gc-30d:
	nix-collect-garbage --delete-older-than 30d

nix-repair-store:
	nix-store --verify --check-contents --repair
