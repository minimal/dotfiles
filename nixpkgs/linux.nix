{ config, pkgs, ... }:
# to enable:
# ln -s linux.nix home.nix
{
  imports = [
    ./_home.nix
    ./git.nix
    ./zsh.nix
  ];
  home.packages = with pkgs; [
    unzip
    trash-cli
  ];

  home.sessionVariables = {
    EDITOR = "nvim";
    VISUAL = "nvim";
  };

  programs = {
    git.userEmail = "christophermcdevitt@gmail.com";
    zsh = {
      shellAliases = {
        code = "/mnt/c/Users/Chris/AppData/Local/Programs/Microsoft\\ VS\\ Code/bin/code";
      };
    };

    fzf = {
      enable = true;
      enableZshIntegration = true;
      defaultOptions = [
        # "--height 40%"
        "--layout=reverse"
        "--border"
        "--inline-info"
      ];
    };
  };
}
