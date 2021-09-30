;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

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

(evil-define-key nil cider-repl-mode-map
  (kbd "C-k") 'cider-repl-backward-input
  (kbd "C-j") 'cider-repl-forward-input)

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

(use-package wakatime-mode
  :config (global-wakatime-mode))

(use-package just-mode)

(+global-word-wrap-mode +1)
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
