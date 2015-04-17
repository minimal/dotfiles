(setq visible-bell t)
(add-to-list 'load-path "~/.emacs.d/vendor/")
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
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
    ;flymake-cursor
    ;rainbow-mode)  ;; colours css colours
))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


(require 'use-package)
(require 'yaml-mode)
(require 'smooth-scrolling)
(require 'ag)

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

(if (file-exists-p "~/.emacs.d/snippets")
    (yas/load-directory "~/.emacs.d/snippets"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ack-and-a-half-executable "/usr/local/bin/ack")
 '(coffee-command "/usr/local/bin/coffee")
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes (quote ("71efabb175ea1cf5c9768f10dad62bb2606f41d110152f4ace675325d28df8bd" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "3580fb8e37ee9e0bcb60762b81260290329a97f3ca19249569d404fce422342f" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "bf9d5728e674bde6a112979bd830cc90327850aaaf2e6f3cc4654f077146b406" "5e1d1564b6a2435a2054aa345e81c89539a72c4cad8536cfe02583e0b7d5e2fa" "d2622a2a2966905a5237b54f35996ca6fda2f79a9253d44793cfe31079e3c92b" "501caa208affa1145ccbb4b74b6cd66c3091e41c5bb66c677feda9def5eab19c" default)))
 '(electric-indent-mode nil)
 '(electric-layout-mode t)
 '(electric-pair-mode t)
 '(erc-dangerous-hosts nil)
 '(erc-pals (quote ("catherine" "tomk" "stuart" "emilbielksi" "jurg" "phil" "tomjoy" "robcowie")))
 '(erc-track-exclude-types (quote ("JOIN" "KICK" "NICK" "PART" "QUIT" "MODE" "333" "353")))
 '(fill-column 70)
 '(hl-line-mode nil t)
 '(ibuffer-saved-filter-groups nil)
 '(ibuffer-saved-filters (quote (("realfiles" ((filename . ".*\\w.*"))) ("gnus" ((or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode)))) ("programming" ((or (mode . emacs-lisp-mode) (mode . cperl-mode) (mode . c-mode) (mode . java-mode) (mode . idl-mode) (mode . lisp-mode)))))))
 '(jabber-chat-buffer-show-avatar nil)
 '(jedi:complete-on-dot (quote on))
 '(line-move-visual nil)
 '(show-paren-mode t)
 '(sql-mysql-program "/usr/local/bin/mysql")
 '(tool-bar-mode nil))

(server-start)
;;; edit server for editing from chrome
(when (require 'edit-server nil t)
  (setq edit-server-new-frame nil)
  (edit-server-start))

;; Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
(setq org-replace-disputed-keys t)

(windmove-default-keybindings)
