
ansible:
	ansible-playbook -i .ansible/hosts .ansible/playbook.yml

nix-packages:
	nix-env -f '<nixpkgs>' -iA userPackages
