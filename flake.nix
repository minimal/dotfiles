{
  description = "A basic flake with a shell";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    devshell.url = "github:numtide/devshell";

    # Add the emacs-lsp-booster input
    emacs-lsp-booster = {
      url = "github:slotThe/emacs-lsp-booster-flake";
      # Ensure it uses the same nixpkgs as the rest of your flake
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    doom-emacs.url = "github:doomemacs/doomemacs/master";
    doom-emacs.flake = false;
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    doom-emacs,
    home-manager,
    emacs-lsp-booster, # Pass the new input to outputs
    devshell,
  }: let
    inherit (home-manager.lib) homeManagerConfiguration;
    isDarwin = system: (builtins.elem system nixpkgs.lib.platforms.darwin);
    M1Overlay = (
      final: prev: let
        pkgs_x86_64 = import nixpkgs {localSystem = "x86_64-darwin";};
      in {
        emacsMacport = pkgs_x86_64.emacsMacport;
      }
    );
    homePrefix = system:
      if isDarwin system
      then "/Users"
      else "/home";
    mkOverlays = system:
      [
        emacs-lsp-booster.overlays.default # Add the booster overlay
        (final: prev: {doomEmacsRevision = doom-emacs.rev;})
        (final: prev: {home-manager = home-manager.packages.${system}.home-manager;})
        (import ./nixpkgs/overlays/bins.nix)
      ]
      ++ (
        if system == "aarch64-darwin"
        then [M1Overlay]
        else []
      );
    mkHomeConfig = {
      username,
      system ? "x86_64-linux",
      baseModules ? [],
      extraModules ? [],
    }:
      homeManagerConfiguration rec {
        pkgs = nixpkgs.legacyPackages.${system};
        modules =
          [
            {
              home = {
                inherit username;
                homeDirectory = "${homePrefix system}/${username}";
                stateVersion = "22.05";
              };
            }
          ]
          ++ baseModules
          ++ extraModules
          ++ (
            if isDarwin system
            then [./nixpkgs/mac.nix]
            else [./nixpkgs/linux.nix]
          )
          ++ [{nixpkgs.overlays = mkOverlays system;}];
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
          ["x86_64-linux"])
        ++ (map
          (system: {
            name = system;
            value = {
              mac = self.homeConfigurations.Yuris-MacBook-Airlocal.activationPackage;
            };
          })
          nixpkgs.lib.platforms.darwin)
      );

      homeConfigurations = {
        chris-3900x = mkHomeConfig {
          username = "chris";
          baseModules = [
            ./nixpkgs/emacs.nix
            ./nixpkgs/profiles/personal.nix
          ];
        };
        FVFG608QQ05Qlocal = mkHomeConfig {
          # workm1
          system = "aarch64-darwin";
          username = "chris.mcdevitt";
          baseModules = [
            ./nixpkgs/emacs.nix
            ./nixpkgs/fonts.nix
            ./nixpkgs/profiles/work.nix
          ];
        };
        Yuris-MacBook-Airlocal = mkHomeConfig {
          system = "aarch64-darwin";
          username = "chris";
          baseModules = [
            ./nixpkgs/fonts.nix
            ./nixpkgs/profiles/personal.nix
          ];
        };
      };
    }
    // flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = (mkOverlays system); # Remove devshell.overlay here
      };
      nixBin = pkgs.writeShellScriptBin "nix" ''
        ${pkgs.nixFlakes}/bin/nix --option experimental-features "nix-command flakes" "$@"
      '';

      devShellOld = pkgs.mkShell {
        nativeBuildInputs = [pkgs.bashInteractive];
        packages = with pkgs; [nixUnstable];
        buildInputs = with pkgs; [pkgs.home-manager];
        shellHook = ''
          export NIX_PATH="nixpkgs=${nixpkgs}:home-manager=${home-manager}"
        '';
      };
    in {
      # Use devshell's own mkShell function, accessed directly from the input
      devShell = devshell.legacyPackages.${system}.mkShell {
        packages = [pkgs.home-manager]; # Add packages directly here
      };
    });
}
