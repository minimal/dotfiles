{
  description = "A basic flake with a shell";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    doom-emacs.url = "github:hlissner/doom-emacs/develop";
    doom-emacs.flake = false;
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, nixpkgs, flake-utils, doom-emacs, emacs-overlay, home-manager }:

    flake-utils.lib.eachDefaultSystem (system:
      let
        system-map = {
          "x86_64-linux" = "linux";
          "x86_64-darwin" = "mac";
          "aarch64-darwin" = "mac";
        };
        hm-filename = system-map."${system}";
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            emacs-overlay.overlay
            (final: prev: { doomEmacsRevision = doom-emacs.rev; })
          ];
        };
      in
      {
        devShell = pkgs.mkShell {
          nativeBuildInputs = [ pkgs.bashInteractive ];
          buildInputs = with pkgs; [ (import home-manager { inherit pkgs; }).home-manager ];
          shellHook = ''
            export NIX_PATH="nixpkgs=${nixpkgs}:home-manager=${home-manager}"
            export HOME_MANAGER_CONFIG="./nixpkgs/${hm-filename}.nix"
          '';
        };
      });
}
