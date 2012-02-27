(setq visible-bell t)

;; adjust this path:
(add-to-list 'load-path "~/.emacs.d/emacs-jabber-0.8.90")
;; For 0.7.90 and above:
(require 'jabber-autoloads)

(setq jabber-account-list
      '(("<ME>@gmail.com" 
	 (:network-server . "talk.google.com")
	 (:connection-type . ssl))))

(add-to-list 'load-path "~/.emacs.d/vendor/emacs-skype")
;; (require 'skype)
;; (setq skype--my-user-handle "ME")

(add-to-list 'load-path "~/.emacs.d/vendor/")


(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
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
                ;python
                python-pep8 python-pylint pyflakes ipython
                anything anything-ipython yasnippet-bundle flymake-cursor))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p))) 

(require 'anything-show-completion)

(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(require 'flymake-cursor)
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
               'flymake-create-temp-inplace))
       (local-file (file-relative-name
            temp-file
            (file-name-directory buffer-file-name))))
      (list "pycheckers"  (list local-file))))
   (add-to-list 'flymake-allowed-file-name-masks
                '("\\.py\\'" flymake-pyflakes-init)))


(require 'ipython)
(require 'anything)
(require 'anything-ipython)
(when (require 'anything-show-completion nil t)
   (use-anything-show-completion 'anything-ipython-complete
                                 '(length initial-pattern)))

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
