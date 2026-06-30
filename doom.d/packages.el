;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

(package! telephone-line)
(package! magit-delta)
(package! wakatime-mode)
(package! just-mode)
(package! blamer :recipe (:host github :repo "artawower/blamer.el"))
(package! tree-sitter)
(package! tree-sitter-langs)
(package! justl)
(package! feature-mode)
(package! lsp-haskell)
(package! clean-kill-ring
  :recipe (:type git
           :host github
           :repo "NicholasBHubbard/clean-kill-ring.el"))
(package! boxquote
  :recipe (:type git
           :host github
           :repo "davep/boxquote.el"))
;; (package! chatgpt
;;   :recipe (:type git :host github :repo "emacs-openai/chatgpt"))
(package! editorconfig)

;; (package! copilot
;;   :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el" "dist")))

(package! difftastic)
(package! pcre2el)
(package! epoch-view)

(package! copilot-chat
  :recipe (:host github :repo "chep/copilot-chat.el" :files ("*.el")))
(package! org-download)
(package! kaocha-runner)
(package! rainbow-delimiters)

;; de-download-mode for downloading decisions and reports
;; (package! de-download-mode
;;   :recipe (:local-repo "/Users/chris.mcdevitt/code/fc/dod-emacs-mode/robert.johnson/decision-output-downloader"
;;            :files ("de-download-mode.el")))

;; Optional: Enhanced JSON support
(package! json-reformat)
(package! json-snatcher)

;; Optional: Enhanced dired support
(package! dired-sidebar)
(package! all-the-icons-dired)


;; agent-shell
(package! shell-maker :pin "808bede99dc2c2d27c3dc69d5f363aade40e6f87")
(package! acp)
(package! agent-shell :pin "68b8c394a4838fb54f7dbfc70cee38e7310f03a3")

(package! pi-coding-agent)
(package! catppuccin-theme)
(package! ghostel)
