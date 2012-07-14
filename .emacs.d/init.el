(setq visible-bell t)

;; adjust this path:
(add-to-list 'load-path "~/.emacs.d/emacs-jabber-0.8.90")
;; For 0.7.90 and above:
;;(require 'jabber-autoloads)

;; (setq jabber-account-list
;;       '(("<ME>@gmail.com" 
;; 	 (:network-server . "talk.google.com")
;; 	 (:connection-type . ssl))))

;;(add-to-list 'load-path "~/.emacs.d/vendor/emacs-skype")
;; (require 'skype)
;; (setq skype--my-user-handle "ME")

(add-to-list 'load-path "~/.emacs.d/vendor/")


(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)


(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages 
  '(starter-kit starter-kit-lisp starter-kit-eshell starter-kit-js
                starter-kit-bindings scpaste
                clojure-mode clojure-test-mode
                markdown-mode yaml-mode
                ecb_snap
                marmalade scpaste python-mode
                full-ack
                ;python
                python-pep8 python-pylint pyflakes ipython
                textmate coffee-mode gist
                move-text highlight-indentation
                helm projectile
                ace-jump-mode jump-char
                expand-region
                flymake-coffee
                anything anything-ipython yasnippet-bundle flymake-cursor))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p))) 

(require 'anything-show-completion)
(require 'yaml-mode)

;;;; Now overriden by emacs for python
;; (require 'python-mode)
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;; (require 'flymake-cursor)
;; (when (load "flymake" t)
;;   (defun flymake-pyflakes-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                'flymake-create-temp-inplace))
;;        (local-file (file-relative-name
;;             temp-file
;;             (file-name-directory buffer-file-name))))
;;       (list "pycheckers"  (list local-file))))
;;    (add-to-list 'flymake-allowed-file-name-masks
;;                 '("\\.py\\'" flymake-pyflakes-init)))

(setq ipython-command "/usr/local/bin/ipython")
(setq py-python-command "/usr/local/bin/ipython")

;; emacs for python https://github.com/gabrielelanaro/emacs-for-python.git
(add-to-list 'load-path "~/.emacs.d/emacs-for-python/")
(require 'epy-setup)
(require 'epy-python)
(require 'epy-completion)
(epy-setup-checker "pycheckers %f")
(require 'highlight-indentation)
(add-hook 'python-mode-hook 'highlight-indentation)

;; (require 'ipython)
;; (require 'anything)
;; (require 'anything-ipython)
;; (when (require 'anything-show-completion nil t)
;;    (use-anything-show-completion 'anything-ipython-complete
;;                                  '(length initial-pattern)))
(require 'textmate)
(textmate-mode)
(require 'full-ack)
(require `tramp)
(require 'move-text)
(require 'jump-char)
(require 'ace-jump-mode)
(require 'expand-region)

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; new
(require 'helm)
(require 'projectile)
(projectile-global-mode) ;; to enable in all buffers
(setq projectile-enable-caching t)

(yas/load-directory "~/.emacs.d/snippets")
;; (require 'ecb)
;; (require 'ecb-autoloads)
;; following command allows ecb to start
;; (setq stack-trace-on-error t)
;; (ecb-activate)
;; (ecb-byte-compile)

 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(electric-indent-mode t)
 '(electric-layout-mode t)
 '(electric-pair-mode t)
 '(hl-line-mode nil)
 '(jabber-chat-buffer-show-avatar nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Droid Sans Mono Dotted"))))
 '(jabber-chat-prompt-foreign ((t (:foreground "firebrick" :weight bold))))
 '(jabber-chat-prompt-local ((t (:foreground "deep sky blue" :weight bold)))))

(server-start)
;;; edit server for editing from chrome
(when (require 'edit-server nil t)
  (setq edit-server-new-frame nil)
  (edit-server-start))

