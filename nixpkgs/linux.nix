{
  config,
  pkgs,
  ...
}:
# to enable:
# ln -s linux.nix home.nix
let
  HOME = config.home.homeDirectory;
in {
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
        # WSL clipboard bridge. Windows dirs are not on PATH here, so use
        # full paths. -NoProfile skips the PowerShell startup profile.
        pbcopy = "/mnt/c/Windows/System32/clip.exe";
        pbpaste = "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe -NoProfile -Command 'Get-Clipboard' | tr -d '\\r'";
        # Lightweight pi for local models: only extensions with zero/low tool tokens
        pi-local =
          "pi --no-extensions"
          + " -e npm:pi-sandbox"
          + " -e npm:pi-vim"
          + " -e npm:@juicesharp/rpiv-btw"
          + " -e npm:pi-context-usage"
          + " -e npm:@tmustier/pi-usage-extension"
          + " -e npm:pi-cache-graph"
          + " -e npm:pi-token-speed"
          + " -e npm:@juicesharp/rpiv-todo"
          + " -e npm:@sting8k/pi-vcc"
          + " -e ${HOME}/.pi/agent/extensions/pi-tool-classifier/index.ts"
          + " -e ${HOME}/.pi/agent/extensions/agent-sessions-pi-live/index.ts"
          + " -e ${HOME}/.pi/agent/extensions/context-monitor.ts";
      };
      initContent = ''
        . ${HOME}/.nix-profile/etc/profile.d/nix.sh
      '';
    };
  };
}
