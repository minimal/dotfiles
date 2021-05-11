
ansible:
	ansible-playbook -i .ansible/hosts .ansible/playbook.yml

nix-packages:
	nix-env -f '<nixpkgs>' -iA userPackages

nix-packages-tree:
	nix-store -q --tree /nix/var/nix/profiles/per-user/cmcdevitt/profile
