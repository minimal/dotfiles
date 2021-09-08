
ansible:
	ansible-playbook -i .ansible/hosts .ansible/playbook.yml

nix-packages-tree:
	nix-store -q --tree /nix/var/nix/profiles/per-user/cmcdevitt/profile

hm-switch:
	home-manager switch

hm-packages:
	home-manager packages

nix-gc-30d:
	nix-collect-garbage --delete-older-than 30d

nix-repair-store:
	nix-store --verify --check-contents --repair
