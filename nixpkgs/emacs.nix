{
  config,
  lib,
  pkgs,
  ...
}:
# most lifted from:
# https://github.com/Mic92/dotfiles/blob/master/nixpkgs-config/modules/emacs/default.nix
let
  emacsSyncScript = pkgs.writeScriptBin "doom-sync-git" ''
    #!${pkgs.runtimeShell}
    export PATH=$PATH:${lib.makeBinPath [pkgs.git pkgs.sqlite pkgs.unzip]}
    if [ ! -d $HOME/.emacs.d/.git ]; then
      mkdir -p $HOME/.emacs.d
      git -C $HOME/.emacs.d init
    fi
    if [ $(git -C $HOME/.emacs.d rev-parse HEAD) != ${pkgs.doomEmacsRevision} ]; then
      git -C $HOME/.emacs.d fetch https://github.com/doomemacs/doomemacs.git || true
      git -C $HOME/.emacs.d checkout ${pkgs.doomEmacsRevision} || true
      YES=1 FORCE=1 $HOME/.emacs.d/bin/doom sync -u || true
    fi
  '';
  myEmacs =
    if pkgs.stdenv.isDarwin
    then pkgs.emacsGcc
    else pkgs.emacsPgtkGcc;

  treeSitterGrammars = pkgs.runCommandLocal "grammars" {} ''
    mkdir -p $out/bin
    ${
      lib.concatStringsSep "\n"
      (lib.mapAttrsToList (name: src: "ln -s ${src}/parser $out/bin/${name}.so") pkgs.tree-sitter.builtGrammars)
    };
  '';
  # list taken from here: https://github.com/emacs-tree-sitter/tree-sitter-langs/tree/e7b8db7c4006c04a4bc1fc6865ec31f223843192/repos
  # commented out are not yet packaged in nix
  langs = [
    "agda"
    "bash"
    "c"
    "c-sharp"
    "cpp"
    "css"
    "clojure" # not in emacs tree-sitter-langs yet :(
    /*
     "elm"
     */
    "fluent"
    "go"
    "haskell"
    /*
     "hcl"
     */
    "html"
    /*
     "janet-simple"
     */
    "java"
    "javascript"
    "jsdoc"
    "json"
    "nix"
    "ocaml"
    "python"
    "php"
    /*
     "pgn"
     */
    "ruby"
    "rust"
    "scala"
    # "swift"
    "typescript"
  ];
  grammars = lib.getAttrs (map (lang: "tree-sitter-${lang}") langs) pkgs.tree-sitter.builtGrammars;
in {
  home.file.".tree-sitter".source = pkgs.runCommand "grammars" {} ''
    mkdir -p $out/bin
    ${
      lib.concatStringsSep "\n"
      (lib.mapAttrsToList (name: src: "name=${name}; ln -s ${src}/parser $out/bin/\${name#tree-sitter-}.so") grammars)
    };
  '';
  home.packages = with pkgs;
    [
      # emacsMacport
      emacsSyncScript
      ripgrep
      fd
      findutils
    ]
    ++ (
      if pkgs.stdenv.isDarwin
      then []
      else [myEmacs]
    );
}
