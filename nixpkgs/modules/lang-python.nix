{pkgs, ...}: {
  # Python toolchain.
  home.packages = with pkgs; [
    python3
    pipenv
    uv
  ];
}
