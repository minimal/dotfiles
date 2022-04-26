{
  config,
  pkgs,
  ...
}: let
  git-export-stash =
    pkgs.writeShellScriptBin "git-export-stash"
    ''
      mkdir -p stashes
      n=$(git stash list | wc -l)
      for i in $(seq 0  $((n-1))); do git stash show -p stash@"{$i}" > stashes/"$i".diff; done
    '';
in {
  home.file.".config/git/git_commit_msg.txt".source = ../config/git/git_commit_msg.txt;
  # Git config using Home Manager modules

  home.packages = with pkgs; [
    git-crypt
    git-lfs
    git-extras
    git-export-stash
    git-absorb
  ];
  programs.git = {
    package = pkgs.gitAndTools.gitFull;
    enable = true;
    userName = "Chris McDevitt";

    aliases = {
      lol = "log --oneline --graph --decorate";
      lola = "log --graph --decorate --oneline --all";
      ci = "commit";
      st = "status -sb";
      co = "checkout";
      oneline = "log --pretty=oneline";
      br = "branch";
      la = "log --pretty=\"format:%ad %h (%an): %s\" --date=short";
      pg = "push origin HEAD:refs/for/master";
      nm = "log --no-merges";
      dw = "diff --word-diff";
      d = "diff";
      dc = "diff --cached";
      m = "merge";
      mnff = "merge --no-ff";
      pop = "stash pop";
      tags = "!git for-each-ref --sort=-taggerdate --format '%(refname:short) %(taggerdate:relative)' refs/tags";
      tagsonly = "!git for-each-ref --sort=-taggerdate --format '%(refname:short)' refs/tags";
      pick = "!sh -c 'git log -S$1 -p' -  # show log diffs that add/remove arg string";
      graphviz = "!f() { echo 'digraph git {' ; git log --pretty='format:  %h -> { %p }' \"$@\" | sed 's/[0-9a-f][0-9a-f]*/\"&\"/g' ; echo '}'; }; f";
      root = "!pwd";
      pu = "push --follow-tags";
      permission-reset = "!git diff -p -R | grep -E \"^(diff|(old|new) mode)\" | git apply";
      sync = "!git stash && git pull --rebase && git stash pop";
      dft = "difftool";
    };
    extraConfig = {
      color = {
        status = "auto";
        diff = "auto";
        branch = "auto";
        interactive = "auto";
        ui = "auto";
        sh = "auto";
      };
      core = {
        editor = "emacsclient  -nw";
        excludesfile = "~/.config/git/gitignore";
        attributesfile = "~/.config/git/gitattributes";
      };

      help.autocorrect = 10;
      rerere.enabled = true;
      branch.autosetuprebase = "always";
      push.default = "simple";
      pull.default = "only";
      init.defaultBranch = "main";

      commit = {
        template = "~/.config/git/git_commit_msg.txt";
      };

      github = {
        user = "minimal";
      };

      url."git@github.com:".pushInsteadOf = "https://github.com/";
      diff."clojure".xfuncname = "(^\\(.*|\\s*\\(defn.*)";

      diff.tool = "difftastic";
      difftool.prompt = false;
      difftool."difftastic".cmd = "difft \"$LOCAL\" \"$REMOTE\"";

      pager = {
        diff = "delta";
        log = "delta";
        reflog = "delta";
        show = "delta";
        difftool = true;
      };
      delta = {
        plus-style = "syntax #012800";
        minus-style = "syntax #340001";
        syntax-theme = "Monokai Extended";
        navigate = true;
      };
      interactive = {
        diffFilter = "delta --color-only";
      };
    };
  };

  programs.gh = {
    enable = true;
    settings = {
      aliases = {
        co = "pr checkout";
        pv = "pr view";
        prs = "pr list -A minimal";
      };
      editor = "code";
      git_protocol = "ssh";
    };
  };
}
