(setq visible-bell t)
(add-to-list 'load-path "~/.emacs.d/vendor/")
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
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

(if (file-exists-p "~/.emacs.d/snippets")
    (yas/load-directory "~/.emacs.d/snippets"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (wrap-region multi-web-mode jedi-direx jedi cider github-browse-file aggressive-indent ace-isearch textmate color-identifiers-mode highlight-indentation volatile-highlights ethan-wspace visual-regexp-steroids quickrun guide-key-tip region-bindings-mode multiple-cursors expand-region ace-window ace-jump-zap ace-jump-mode drag-stuff helm-projectile helm-ag-r helm-ag helm-swoop helm ido-vertical-mode paradox exec-path-from-shell company yasnippet use-package flycheck auto-complete ag smooth-scrolling jump-char gist marmalade yaml-mode markdown-mode scpaste starter-kit))))

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
 )
