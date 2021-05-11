
ansible:
	ansible-playbook -i .ansible/hosts .ansible/playbook.yml

nix-packages:
	home-manager switch
	# nix-env -f '<nixpkgs>' -iA userPackages

nix-packages-tree:
	nix-store -q --tree /nix/var/nix/profiles/per-user/cmcdevitt/profile

hm-switch:
	home-manager switch

hm-packages:
	home-manager packages
