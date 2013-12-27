(setq visible-bell t)

;; adjust this path:
;(add-to-list 'load-path "~/.emacs.d/emacs-jabber-0.8.90")
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
                marmalade scpaste python-mode
                ack-and-a-half
                python-pep8 python-pylint pyflakes ipython
                textmate coffee-mode gist
                highlight-indentation
                helm projectile
                ace-jump-mode jump-char
                expand-region
                flymake-coffee
                drag-stuff
                smooth-scrolling
                multi-web-mode
                visual-regexp
                visual-regexp-steroids
                volatile-highlights
                ag
                auto-complete
                flycheck
                exec-path-from-shell
                yasnippet-bundle flymake-cursor
                rainbow-mode  ;; colours css colours
                ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))


;; (require 'anything-show-completion)
(require 'yaml-mode)
(require 'smooth-scrolling)

(require 'ag)
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

(require 'auto-complete)
;; emacs for python https://github.com/gabrielelanaro/emacs-for-python.git
(add-to-list 'load-path "~/.emacs.d/emacs-for-python/")
(require 'epy-setup)
(require 'epy-python)
;; (require 'epy-completion)
(epy-setup-checker "~/bin/pycheckers %f")
(require 'highlight-indentation)
(add-hook 'python-mode-hook 'highlight-indentation)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))
(el-get 'sync)

(setq jedi:setup-keys t)                ;install with: el-get-install
                                    ;jedi
(require 'jedi)
(global-auto-complete-mode t)
(add-hook 'python-mode-hook 'jedi:setup)

;; (require 'ipython)
;; (require 'anything)
;; (require 'anything-ipython)
;; (when (require 'anything-show-completion nil t)
;;    (use-anything-show-completion 'anything-ipython-complete
;;                                  '(length initial-pattern)))
(require 'textmate)
(textmate-mode)
(require `tramp)
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

(require 'multi-web-mode) ;; multi types in html etc
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags 
      '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
        (js-mode  "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
        (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

 
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


;; magnars stuff
(when is-mac
  ;; mac friendly font
  (set-face-attribute 'default nil :font "Monaco-14")
  ;; Ignore .DS_Store files with ido mode
  (add-to-list 'ido-ignore-files "\\.DS_Store")
  ;; Don't open files from the workspace in a new frame
  (setq ns-pop-up-frames nil)
  ;; Use aspell for spell checking: brew install aspell --lang=en
  (setq ispell-program-name "/usr/local/bin/aspell")
  (global-set-key '[(control kp-delete)] 'kill-word)
  (global-set-key (kbd "�") "|")  ;; I use uk layout which hides pipe key
  )

;; magnars sane defaults

;; org-mode: Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
(setq org-replace-disputed-keys t)

;; Fontify org-mode code blocks
(setq org-src-fontify-natively t)
;; ack-and-a-half
(autoload 'ack-and-a-half-same "ack-and-a-half" nil t)
(autoload 'ack-and-a-half "ack-and-a-half" nil t)
(autoload 'ack-and-a-half-find-file-samee "ack-and-a-half" nil t)
(autoload 'ack-and-a-half-find-file "ack-and-a-half" nil t)
;; Create shorter aliases
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; awesome!
;; (define-key global-map (kbd "C-c r") 'pyregexp-replace)
;; (define-key global-map (kbd "C-c q") 'pyregexp-query-replace)
;; to use pyregexp isearch instead of the built-in regexp isearch, also include the following lines:
(define-key esc-map (kbd "C-r") 'pyregexp-isearch-backward)
(define-key esc-map (kbd "C-s") 'pyregexp-isearch-forward)
(require 'visual-regexp-steroids)

(require 'multiple-cursors)
(global-ethan-wspace-mode 1)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(require 'volatile-highlights)

(volatile-highlights-mode)
