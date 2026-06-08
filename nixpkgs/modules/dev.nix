{pkgs, ...}: {
  # General developer tooling, beyond the universal base in _home.nix.
  home.packages = with pkgs; [
    pre-commit
    shellcheck
    cloc
    scc # fast loc counter
    httpie
    pgcli
    postgresql
    wakatime-cli
    cargo
  ];
}
