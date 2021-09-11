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
    let
      inherit (home-manager.lib) homeManagerConfiguration;
      isDarwin = system: (builtins.elem system nixpkgs.lib.platforms.darwin);
      homePrefix = system: if isDarwin system then "/Users" else "/home";
      overlays = [
        emacs-overlay.overlay
        (final: prev: { doomEmacsRevision = doom-emacs.rev; })
      ];
      mkHomeConfig =
        { username
        , system ? "x86_64-linux"
        , baseModules ? [ ]
        , extraModules ? [ ]
        }:
        homeManagerConfiguration rec {
          inherit system username;
          homeDirectory = "${homePrefix system}/${username}";
          # extraSpecialArgs = { inherit inputs lib; };
          configuration = {
            imports = baseModules ++ extraModules
              ++ [{ nixpkgs.overlays = overlays; }];
          };
        };
    in
    {
      checks = builtins.listToAttrs (
        (map
          (system: {
            name = system;
            value = {
              linux = self.homeConfigurations.DESKTOP-096IFDV.activationPackage;
            };
          })
          nixpkgs.lib.platforms.linux) ++
        (map
          (system: {
            name = system;
            value = {
              mac = self.homeConfigurations.MacBook-Prolocal.activationPackage;
            };
          })
          nixpkgs.lib.platforms.darwin)
      );

      homeConfigurations = {
        DESKTOP-096IFDV = mkHomeConfig {
          username = "chris";
          baseModules = [ ./nixpkgs/linux.nix ];
        };
        MacBook-Prolocal = mkHomeConfig {
          system = "x86_64-darwin";
          username = "cmcdevitt";
          baseModules = [ ./nixpkgs/mac.nix ];
        };
        Yuris-MacBook-Airlocal = mkHomeConfig {
          system = "aarch64-darwin";
          username = "chris";
          baseModules = [ ./nixpkgs/mac.nix ];
        };
      };
    }
    //
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
          '';
        };
      });
}
