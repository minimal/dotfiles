{
  config,
  pkgs,
  ...
}:
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
    zsh = {
      shellAliases = {
        code = "/mnt/c/Users/Chris/AppData/Local/Programs/Microsoft\\ VS\\ Code/bin/code";
      };
      initExtra = ''
        if [[ $(pwd) != $HOME ]] then cd ~; fi
      '';
    };
  };
}
