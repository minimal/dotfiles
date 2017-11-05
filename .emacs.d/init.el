
(setq visible-bell t)
;; (setq visible-bell nil)
;; (setq ring-bell-function
;;       (lambda () (invert-face 'mode-line)
;;         (run-with-timer 0.1 nil 'invert-face 'mode-line)))
;; not needed after emacs update:
;; http://stuff-things.net/2015/12/16/emacs-el-capitain-visible-bell-fixed/

(add-to-list 'load-path "~/.emacs.d/vendor/")

;; Keep emacs Custom-settings in separate file
;; (setq custom-file (expand-file-name "secret.el" user-emacs-directory))
;; (if (file-exists-p abbrev-file-name)
;;     (load custom-file))

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(
    starter-kit
    ;starter-kit-lisp
    ;starter-kit-eshell
    ;starter-kit-js
    ;starter-kit-bindings
    scpaste
    markdown-mode
    yaml-mode
    marmalade
    ;;  scpaste
    ;; python-pep8
    ;; python-pylint
    ;; pyflakes
    ;; ipython
    ;; coffee-mode
    gist
    jump-char
    ;flymake-coffee
    smooth-scrolling
    ;multi-web-mode
    ag
    auto-complete
    flycheck
    ;;yasnippet-bundle
    use-package
    yasnippet
    company
    ;flymake-cursor
    ;rainbow-mode)  ;; colours css colours
))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

(require 'yaml-mode)
(require 'smooth-scrolling)
(require 'ag)
(require 'grep)  ;; to fix helm-ag-r

(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-set-upstream-on-push t)

;;; yasnippet
;;; should be loaded before auto complete so that they can work together
(require 'yasnippet)
(yas/global-mode 1)
(require 'auto-complete)

;;; auto complete mod
;;; should be loaded after yasnippet so that they can work together
(require 'auto-complete-config)
(ac-config-default)
;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

;; emacs for python https://github.com/gabrielelanaro/emacs-for-python.git
;(add-to-list 'load-path "~/.emacs.d/emacs-for-python/")
;; (require 'epy-setup)
;; (require 'epy-python)
;; (require 'epy-completion)
;; (epy-setup-checker "~/bin/pycheckers %f")


(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))
(el-get 'sync)


(require `tramp)
(require 'jump-char)

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;(if (file-exists-p "~/.emacs.d/snippets")
; ;   (yas/load-directory "~/.emacs.d/snippets"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#3C3836" "#FB4934" "#B8BB26" "#FABD2F" "#83A598" "#D3869B" "#8EC07C" "#EBDBB2"])
 '(blink-cursor-mode nil)
 '(cider-auto-test-mode nil)
 '(cljr-favor-prefix-notation nil)
 '(column-number-mode t)
 '(confirm-kill-emacs (quote y-or-n-p))
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes
   (quote
    ("5b29f90eb304b440c908de31caf7d730db451b5909e8a84a2e7cd8d60f6d5c1f" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "26614652a4b3515b4bbbb9828d71e206cc249b67c9142c06239ed3418eff95e2" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(exec-path
   (quote
    ("/Users/christophermcdevitt/.local/bin" "/Users/christophermcdevitt/.opam/system/bin/" "/Users/christophermcdevitt/perl5/bin/" "/Users/christophermcdevitt/perl5/bin/" "/usr/local/bin/" "/usr/local/sbin/" "/usr/local/share/python/" "/usr/bin/" "/bin/" "/usr/sbin/" "/sbin/" "/Users/christophermcdevitt/bin/" "/usr/local/Cellar/emacs/HEAD/libexec/emacs/25.0.50/x86_64-apple-darwin14.5.0/")))
 '(fci-rule-color "#202325")
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(flycheck-ruby-rubocop-executable "rbenv exec rubocop")
 '(flyspell-default-dictionary "british")
 '(haskell-indentation-left-offset 4)
 '(hindent-indent-size 4)
 '(hindent-style "johan-tibell")
 '(hl-sexp-background-color "#1c1f26")
 '(magit-diff-refine-hunk (quote all))
 '(org-agenda-files
   (quote
    ("~/Dropbox/docs/org/learning.goals.org" "~/Dropbox/docs/org/trampoline.org" "~/Dropbox/docs/org/todo-august-2014.org")))
 '(org-capture-templates nil t)
 '(org-directory "~/Dropbox/docs/org")
 '(org-return-follows-link t)
 '(package-selected-packages
   (quote
    (elm-yasnippets gist http todochiku flycheck-rust rust-mode persistent-scratch py-isort flycheck-mypy company-jedi pyvenv swiper yaml-mode gh org-pomodoro seq "seq-25" "seq" concurrent dash helm-google spacemacs-theme flycheck-joker git-timemachine clj-refactor cljr-helm clojars clojure-cheatsheet edn soft-morning-theme clojure-mode-extra-font-locking company desktop+ interleave gmail-message-mode osx-plist flycheck-flow nix-mode wakatime-mode dockerfile-mode docker restclient company-edbi heap vlf tao-theme helm helm-clojuredocs web-mode babel magit org material-theme helm-idris projectile speed-type emojify unicode-emoticons clojure-mode typed-clojure-mode neotree mmm-mode csv-mode elm-mode smart-mode-line-powerline-theme beacon ascii grandshell-theme darktooth-theme soft-charcoal-theme inf-clojureb company-ghc helm-hoogle unicode-input async avy find-file-in-project git-commit helm-core names less-css-mode flycheck-clojure flycheck-elm rainbow-mode adoc-mode typo tuareg jade-mode rainbow-identifiers wgrep-helm wgrep-ag typopunct clj-refactor-mode rainbow-delimiters smart-mode-line flatland-theme wrap-region multi-web-mode jedi-direx github-browse-file aggressive-indent ace-isearch textmate color-identifiers-mode volatile-highlights ethan-wspace visual-regexp-steroids guide-key-tip region-bindings-mode ace-window ace-jump-zap ace-jump-mode drag-stuff helm-ag-r exec-path-from-shell use-package auto-complete ag smooth-scrolling jump-char marmalade markdown-mode starter-kit)))
 '(paradox-automatically-star nil)
 '(paradox-github-token t)
 '(persistent-scratch-backup-directory "~/tmp/scratch-backups")
 '(pos-tip-background-color "#36473A")
 '(pos-tip-foreground-color "#FFFFC8")
 '(safe-local-variable-values
   (quote
    ((projectile-project-root . ".")
     (cider-refresh-after-fn . "integrant.repl/resume")
     (cider-refresh-before-fn . "integrant.repl/suspend")
     (pyvenv-workon . invoice-dedupe-py3)
     (whitespace-line-column . 80)
     (lexical-binding . t))))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(typo-language "English")
 '(vc-annotate-background "#1f2124")
 '(vc-annotate-color-map
   (quote
    ((20 . "#ff0000")
     (40 . "#ff4a52")
     (60 . "#f6aa11")
     (80 . "#f1e94b")
     (100 . "#f5f080")
     (120 . "#f6f080")
     (140 . "#41a83e")
     (160 . "#40b83e")
     (180 . "#b6d877")
     (200 . "#b7d877")
     (220 . "#b8d977")
     (240 . "#b9d977")
     (260 . "#93e0e3")
     (280 . "#72aaca")
     (300 . "#8996a8")
     (320 . "#afc4db")
     (340 . "#cfe2f2")
     (360 . "#dc8cc3"))))
 '(vc-annotate-very-old-color "#dc8cc3")
 '(vc-follow-symlinks nil)
 '(wakatime-api-key "94cd0542-f3f9-417b-8b43-3219d91be865")
 '(wakatime-cli-path "/Users/christophermcdevitt/Envs/wakatime/bin/wakatime")
 '(wakatime-python-bin "/Users/christophermcdevitt/Envs/wakatime/bin/python"))

(server-start)
;;; edit server for editing from chrome
(when (require 'edit-server nil t)
  (setq edit-server-new-frame nil)
  (edit-server-start))

;; Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
(setq org-replace-disputed-keys t)

(windmove-default-keybindings)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "#808080" :slant italic))))
 '(markdown-header-face-1 ((t (:inherit font-lock-function-name-face :weight bold :height 1.3 :family "Concourse C2"))))
 '(markdown-header-face-2 ((t (:inherit font-lock-function-name-face :weight bold :height 1.3 :family "Concourse T2"))))
 '(markdown-header-face-3 ((t (:inherit font-lock-function-name-face :weight bold :height 1.2 :family "Concourse T2")))))
(put 'narrow-to-region 'disabled nil)
