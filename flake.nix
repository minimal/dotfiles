{
  description = "A basic flake with a shell";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    devshell.url = "github:numtide/devshell";

    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    doom-emacs.url = "github:hlissner/doom-emacs/develop";
    doom-emacs.flake = false;
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, nixpkgs, flake-utils, doom-emacs, emacs-overlay, home-manager, devshell }:
    let
      inherit (home-manager.lib) homeManagerConfiguration;
      isDarwin = system: (builtins.elem system nixpkgs.lib.platforms.darwin);
      homePrefix = system: if isDarwin system then "/Users" else "/home";
      mkOverlays = system: [
        emacs-overlay.overlay
        (final: prev: { doomEmacsRevision = doom-emacs.rev; })
        (final: prev: { home-manager = home-manager.packages.${system}.home-manager; })
        (import ./nixpkgs/overlays/bins.nix)
        (import ./nixpkgs/overlays/apple_silicon.nix { nixpkgs = nixpkgs; })
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
              ++ [{ nixpkgs.overlays = (mkOverlays system); }];
          };
        };
    in
    {
      checks = builtins.listToAttrs (
        (map
          (system: {
            name = system;
            value = {
              linux = self.homeConfigurations.chris-3900x.activationPackage;
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
          [ ] #nixpkgs.lib.platforms.darwin # disable until IFD bug fixed
        )
      );

      homeConfigurations = {
        chris-3900x = mkHomeConfig {
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
        pkgs = import nixpkgs {
          inherit system;
          overlays = (mkOverlays system) ++ [ devshell.overlay ];
        };
        nixBin = pkgs.writeShellScriptBin "nix" ''
          ${pkgs.nixFlakes}/bin/nix --option experimental-features "nix-command flakes" "$@"
        '';

        devShellOld = pkgs.mkShell {
          nativeBuildInputs = [ pkgs.bashInteractive ];
          packages = with pkgs; [ nixUnstable ];
          buildInputs = with pkgs; [ pkgs.home-manager ];
          shellHook = ''
            export NIX_PATH="nixpkgs=${nixpkgs}:home-manager=${home-manager}"
          '';
        };
      in
      {

        devShell = pkgs.devshell.mkShell {
          packages = with pkgs; [ pkgs.home-manager ];
        };
      });
}
