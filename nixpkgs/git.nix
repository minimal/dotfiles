{ config, pkgs, ... }:
{
  # Git config using Home Manager modules
  programs.git = {
    package = pkgs.gitAndTools.gitFull;
    enable = true;
    userName = "Chris McDevitt";

    signing = {
      key = "3A042C6B67C88936D05AD968288F081B1A54FB2A";
      signByDefault = false;
    };

    aliases = {
      lol               = "log --oneline --graph --decorate";
      lola              = "log --graph --decorate --oneline --all";
      ci                = "commit";
      st                = "status -sb";
      co                = "checkout";
      oneline           = "log --pretty=oneline";
      br                = "branch";
      la                = "log --pretty=\"format:%ad %h (%an): %s\" --date=short";
      pg                = "push origin HEAD:refs/for/master";
      nm                = "log --no-merges";
      dw                = "diff --word-diff";
      d                 = "diff --color-words --abbrev";
      dc                = "diff --cached";
      m                 = "merge";
      mnff              = "merge --no-ff";
      pop               = "stash pop";
      tags              = "!git for-each-ref --sort=-taggerdate --format '%(refname:short) %(taggerdate:relative)' refs/tags";
      tagsonly          = "!git for-each-ref --sort=-taggerdate --format '%(refname:short)' refs/tags";
      pick              = "!sh -c 'git log -S$1 -p' -  # show log diffs that add/remove arg string";
      graphviz          = "!f() { echo 'digraph git {' ; git log --pretty='format:  %h -> { %p }' \"$@\" | sed 's/[0-9a-f][0-9a-f]*/\"&\"/g' ; echo '}'; }; f";
      root              = "!pwd";
      pu                = "push --follow-tags";
      permission-reset  = "!git diff -p -R | grep -E \"^(diff|(old|new) mode)\" | git apply";
      sync              = "!git stash && git pull --rebase && git stash pop";
    };
    extraConfig = {
      color = {
        status      = "auto";
        diff        = "auto";
        branch      = "auto";
        interactive = "auto";
        ui          = "auto";
        sh          = "auto";
      };
      core = {
        editor          = "emacsclient  -nw";
        excludesfile    = "~/.gitignore";
        attributesfile  = "~/.gitattributes";
      };

      help.autocorrect = 10;
      rerere.enabled = true;
      branch.autosetuprebase = "always";
      push.default = "simple";
      pull.default = "only";

      commit = {
        template = "~/code/dotfiles/git_commit_msg.txt";
        gpgsign = false;
      };

      github = {
        user = "minimal";
      };

      url."git@github.com:".pushInsteadOf = "https://github.com/";
      diff."clojure".xfuncname = "(^\\(.*|\\s*\\(defn.*)";
    };
  };
}
