;; emacs customisations after using emacs starter kit.
;; font


(setq-default tab-width 4)
(setq column-number-mode t)
(setq js-indent-level 4)
(setq recentf-max-menu-items 50)
;; (setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(global-linum-mode 1)

;; backup http://pejusdas.com/content/emacs-backup-files also see:
;; http://amitp.blogspot.com/2007/03/emacs-move-autosave-and-backup-files.html

(setq kept-new-versions 5)
(setq kept-old-versions 5)

(menu-bar-mode 1)
;(cua-mode t nil (cua-base))
;(save-place t nil (saveplace))

;; bindings

(setq x-super-keysym 'meta) ; make cmd key as meta - for apple keyboard on linux

;; (global-set-key "\r" 'newline-and-indent)
;; f1 is help prefix, press twice for help-for-help
(global-set-key [f2] 'goto-line)
;; f3 is start record macro
;; f4 is stop record macro / execute macro
(global-set-key [f5] 'previous-multiframe-window)
(global-set-key [f6] 'next-multiframe-window)
(global-set-key [f7] 'kill-buffer)
(global-set-key [f8] 'dired)
;; f10 is menu-bar-open
(global-set-key [f9] 'ido-switch-buffer)
(global-set-key [f11] 'textmate-goto-symbol)
(set-register ?e '(file . "~/.emacs.d/chris.el")) ; 'C-x r j e' opens this file 
(define-key global-map (kbd "C-;") 'iedit-mode)
(define-key global-map (kbd "C-3") 'comment-or-uncomment-region-or-line)
(global-set-key [(meta \])] 'textmate-shift-right)
(global-set-key [(meta \[)] 'textmate-shift-left)

;; (add-to-list 'load-path "~/.emacs.d/vendor/textmate.el")
;; (require 'textmate)
;; (textmate-mode)



(add-hook 'python-mode-hook 
      (lambda () 
        (unless (eq buffer-file-name nil) (flymake-mode 1)) ;dont invoke flymake on temporary buffers for the interpreter
        ;(local-set-key [f7] 'flymake-goto-prev-error)
        ;(local-set-key [f8] 'flymake-goto-next-error)
        ))
;; activate minor whitespace mode when in python mode
(add-hook 'python-mode-hook 'whitespace-mode)

;; (add-hook 'python-mode-hook
;;           (lambda()
;;             (add-hook 'local-write-file-hooks
;;                       '(lambda()
;;                          (save-excursion
;;                            (whitespace-cleanup))))))

(setq-default indicate-empty-lines t
        indicate-buffer-boundaries 'left)

(idle-highlight-mode)


;;;;; Dedicated window
;; (defadvice pop-to-buffer (before cancel-other-window first)
;;   (ad-set-arg 1 nil))

;; (ad-activate 'pop-to-buffer)

;; ;; Toggle window dedication
;; (defun toggle-window-dedicated ()
;;   "Toggle whether the current active window is dedicated or not"
;;   (interactive)
;;   (message
;;    (if (let (window (get-buffer-window (current-buffer)))
;;          (set-window-dedicated-p window 
;;                                  (not (window-dedicated-p window))))
;;        "Window '%s' is dedicated"
;;      "Window '%s' is normal")
;;    (current-buffer)))

;; ;; Press [pause] key in each window you want to "freeze"
;; (global-set-key [pause] 'toggle-window-dedicated)
;;;;;;;;;;;;; dedicated
