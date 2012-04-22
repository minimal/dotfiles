;; emacs customisations after using emacs starter kit.
;; font


(setq-default tab-width 4)
(setq-default whitespace-tab-width 4)
(setq column-number-mode t)
(setq js-indent-level 4)
(setq recentf-max-menu-items 50)
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(global-linum-mode 1)

;; backup http://pejusdas.com/content/emacs-backup-files also see:
;; http://amitp.blogspot.com/2007/03/emacs-move-autosave-and-backup-files.html

(setq kept-new-versions 5)
(setq kept-old-versions 5)

(menu-bar-mode 1)
;(cua-mode t nil (cua-base))
;(save-place t nil (saveplace))

(setq-default cursor-type 'bar)
(winner-mode 1) ;; C-c left => undo window layout change, C-c right => undo

;; bindings

;; (setq x-super-keysym 'meta) ; make cmd key as meta - for apple keyboard on linux

;; (global-set-key "\r" 'newline-and-indent)

;; http://xahlee.org/emacs/keyboard_shortcuts.html
;; To unset a key e.g:
;; (global-unset-key (kbd "C-_"))
;; f1 is help prefix, press twice for help-for-help
(global-set-key [f2] 'goto-line)
;; f3 is start record macro
;; f4 is stop record macro / execute macro
(global-set-key [f5] 'previous-multiframe-window)
(global-set-key [f6] 'next-multiframe-window)
(global-set-key [f7] 'kill-buffer)
(global-set-key [f8] 'dired)
(global-set-key (kbd "<M-f8>") 'ido-find-file)
(global-set-key [(shift f8)] 'ido-find-file-other-window)
;; f10 is menu-bar-open
(global-set-key [f9] 'ido-switch-buffer)
(global-set-key [(meta f9)] 'textmate-goto-file)
(global-set-key (kbd "<S-f9>") 'ido-switch-buffer-other-window)
(global-set-key [(meta shift f9)] 'helm-projectile)
(global-set-key [f11] 'textmate-goto-symbol)
(set-register ?e '(file . "~/.emacs.d/chris.el")) ; 'C-x r j e' opens this file 
(define-key global-map (kbd "C-;") 'iedit-mode)
(define-key global-map (kbd "C-3") 'comment-or-uncomment-region-or-line)
(global-set-key [(meta \])] 'textmate-shift-right)
(global-set-key [(meta \[)] 'textmate-shift-left)
(global-set-key [(control q)] 'fill-paragraph) ;; virtual box
;; overrides meta-q 
(global-set-key [(meta n)] 'move-text-down)
(global-set-key [(meta p)] 'move-text-up)

(global-set-key [(meta m)] 'jump-char-forward)
(global-set-key [(shift meta m)] 'jump-char-backward)
(global-set-key [(meta i)] 'back-to-indentation)
(global-set-key (kbd "M-#") 'ace-jump-mode)

(defun align-repeat (start end regexp)
  "Repeat alignment with respect to 
     the given regular expression."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end 
                (concat "\\(\\s-*\\)" regexp) 1 1 t))

;; See http://www.emacswiki.org/cgi-bin/wiki/misc-cmds.el
(defun beginning-of-line-or-indentation ()
  "move to beginning of line, or indentation"
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))

(substitute-key-definition 'beginning-of-line
                           'beginning-of-line-or-indentation
                           (current-global-map)) ;; not working??
(global-set-key [(control a)] 'beginning-of-line-or-indentation)

;; (add-hook 'python-mode-hook 
;;       (lambda () 
;;         (unless (eq buffer-file-name nil) (flymake-mode 1)) ;dont invoke flymake on temporary buffers for the interpreter
;;         ;(local-set-key [f7] 'flymake-goto-prev-error)
;;         ;(local-set-key [f8] 'flymake-goto-next-error)
        
;;         ))
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
(defadvice pop-to-buffer (before cancel-other-window first)
  (ad-set-arg 1 nil))

(ad-activate 'pop-to-buffer)

;; Toggle window dedication
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window 
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

;; Press [pause] key in each window you want to "freeze"
(global-set-key [pause] 'toggle-window-dedicated)
;;;;;;;;;;;;; dedicated
