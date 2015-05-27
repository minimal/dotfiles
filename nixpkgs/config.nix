{
  packageOverrides = super: let self = super.pkgs; in
  {
    haskellEnv = self.haskellngPackages.ghcWithPackages (p: with p; [
      cabal2nix
      cabal-install
      hlint
    ]);
  };
}
