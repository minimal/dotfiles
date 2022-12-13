;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; (remove-hook 'org-load-hook #'+org-init-keybinds-h)

(when (featurep! spacemacs)
  (remove-hook 'org-load-hook #'+org-init-keybinds-h)
  (doom-init-ui-hook . spacemacs/home))

(map! :leader :desc "M-x" "SPC" #'execute-extended-command
      (:prefix-map ("g" . "git")
       (:when (featurep! :tools magit)
        :desc "Magit status"              "s"   #'magit-status)))

;; Lisp keybinds
(map! (:map smartparens-mode-map ;; Some default paredit binds
       :ni "C-)" #'sp-forward-slurp-sexp
       :ni "C-}" #'sp-forward-barf-sexp
       :ni "C-(" #'sp-backward-slurp-sexp
       :ni "C-{" #'sp-backward-barf-sexp
       :ni "C-k" #'sp-kill-hybrid-sexp)
      (:map cider-repl-mode-map
       :n "s-k" #'cider-repl-backward-input
       :n "s-j" #'cider-repl-forward-input))

(setq cider-print-fn "puget")

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

(use-package wakatime-mode
  :config (global-wakatime-mode))

(use-package just-mode)
(use-package! feature-mode)

(+global-word-wrap-mode +1)

(use-package blamer
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :family "san-serif"
                   :height 120
                   :italic t)))
  :config
  (global-blamer-mode 1))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Chris McDevitt"
      user-mail-address "")
;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-font (font-spec :family "PragmataPro Liga" :size 13 :weight 'normal))
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
;; Need this for emacsPgtkGcc terminal wsl for some reason

(map! [mouse-4] [wheel-up])
(map! [mouse-5] [wheel-down])

(defun native-running? ()
  (interactive)
  (if (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
      (message "Native compilation is available")
    (message "Native complation is *not* available")))

(use-package! tree-sitter
  :config
  (cl-pushnew (expand-file-name "~/.tree-sitter") tree-sitter-load-path)
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (add-to-list 'tree-sitter-major-mode-language-alist '(clojure-mode . clojure)))


(set-formatter! 'alejandra "alejandra --quiet" :modes '(nix-mode))

(setq! electric-pair-mode t)

(use-package! justl)

(use-package! clean-kill-ring
  :config (clean-kill-ring-mode 1))
