
(add-to-list 'load-path "~/.emacs.d/vendor/")
(custom-set-variables
 '(flyspell-default-dictionary "british")
 '(haskell-indentation-left-offset 4)
 '(hindent-indent-size 4)
 '(hindent-style "johan-tibell")
 '(magit-diff-refine-hunk (quote all))
 '(org-agenda-files
   (quote
    ("~/Dropbox/docs/org/learning.goals.org" "~/Dropbox/docs/org/trampoline.org" "~/Dropbox/docs/org/todo-august-2014.org")))
 '(org-capture-templates nil t)
 '(org-directory "~/Dropbox/docs/org")
 '(org-return-follows-link t)
'(persistent-scratch-backup-directory "~/tmp/scratch-backups"))

;; Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
(setq org-replace-disputed-keys t)

(windmove-default-keybindings)
(custom-set-faces
 '(font-lock-comment-face ((t (:foreground "#808080" :slant italic))))
 '(markdown-header-face-1 ((t (:inherit font-lock-function-name-face :weight bold :height 1.3 :family "Concourse C2"))))
 '(markdown-header-face-2 ((t (:inherit font-lock-function-name-face :weight bold :height 1.3 :family "Concourse T2"))))
 '(markdown-header-face-3 ((t (:inherit font-lock-function-name-face :weight bold :height 1.2 :family "Concourse T2")))))
(put 'narrow-to-region 'disabled nil)
(setq is-mac (equal system-type 'darwin))
(load-file "~/.emacs.d/chris.el")
