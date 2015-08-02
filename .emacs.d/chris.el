(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)
;; (cua-mode t nil (cua-base))
(cua-mode)

(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
;(save-place t nil (saveplace))

;; bindings
;; (setq x-super-keysym 'meta) ; make cmd key as meta - for apple keyboard on linux
(setq mac-command-modifier 'meta) ;; on osx set command to meta
;; (global-set-key "\r" 'newline-and-indent)
;; http://xahlee.org/emacs/keyboard_shortcuts.html
;; (setq debug-on-error t)
(org-babel-load-file "/Users/chrismcdevitt/code/dotfiles/.emacs.d/conf.org")


(defun my-update-env (fn)
  (message "in my custom render fn")
  (let ((str
         (with-temp-buffer
           (insert-file-contents fn)
           (buffer-string))) lst)
    (setq lst (split-string str "\000"))
    (while lst
      (setq cur (car lst))
      (when (string-match "^\\(.*?\\)=\\(.*\\)" cur)
        (setq var (match-string 1 cur))
        (setq value (match-string 2 cur))
        (setenv var value))
      (setq lst (cdr lst)))))

(defun alarm (time message)
  "Popup a buffer with message at given `time'. See docs for
`run-at-time' to see what formats you can use."
  (interactive "sTime: \nsMessage: ")
  (let ((f (lambda (message)
             (let ((buf (get-buffer-create "*alarm*")))
               (save-excursion
                 (set-buffer buf)
                 (insert "Alarm: " message)
                 (display-buffer buf))))))
    (run-at-time time nil f message)))

;; begin use-package
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use this to define add hoc reset
;; (define-key clojure-mode-map (kbd "M-r")
;;   (lambda ()
;;     (interactive)
;;     (cider-interactive-eval
;;       "(require '[clojure.pprint :refer [pprint]])
;;        (pprint @interesting-atom)")))

;; indentation tweaks for korma etc


;; (put-clojure-indent 'dom/div 'defun)

;; (add-custom-clojure-indents 'dom 2)

;; compojure
;; (define-clojure-indent
;;   (defroutes 'defun)
;;   (GET 2)
;;   (POST 2)
;;   (PUT 2)
;;   (DELETE 2)
;;   (HEAD 2)
;;   (ANY 2)
;;   (context 2)
;;   (dom/div 2))
;;(put-clojure-indent 'match 1) ;; core.match

;; (put-clojure-indent 'dom/div 2)


;; end clojure
;; end use-package ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2))

(add-hook 'coffee-mode-hook
          '(lambda() (coffee-custom)))

(add-hook 'coffee-mode-hook
  '(lambda() (coffee-custom)))
(add-hook 'coffee-mode-hook 'flymake-coffee-load)

(add-hook 'js-mode-hook 'flymake-jshint-load)

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.gitconfig\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.cs\\'" . coffee-mode)) ;; /sigh

(comment
 (require 'web-mode)
 (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode)))
;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq-default indicate-empty-lines t
        indicate-buffer-boundaries 'left)

(idle-highlight-mode)
(defun my-buffer-face-mode-variable ()
  "Set different fonts in current buffer"
  (interactive)
  ;; (setq buffer-face-mode-face '(:font "Monaco-8"))
  ;; (set-face-attribute 'default nil :font "Monaco-14")
  (setq buffer-face-mode-face '(:height 122))
  (buffer-face-mode))

(add-hook 'erc-mode-hook
          'my-buffer-face-mode-variable)
;; (remove-hook 'erc-mode-hook  'my-buffer-face-mode-variable)

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

;;; change in next quotes. like vim: ci"
;;; TODO: choosing of quote char
(fset 'change-in
      (lambda (&optional arg) "Keyboard macro."
        (interactive "p")
        (kmacro-exec-ring-item
         (quote ([19 91 39 34 93 13 67108896 19 19 2] 0 "%d")) arg)))

;;; win git location
(if (eq system-type 'windows-nt)
    (setq exec-path (append exec-path '("J:/downloads/git-portable/bin")))
  )

;; Notifications
;; look at erc-track-exclude-types variable
(comment
 (require 'todochiku)
 ;; (unless (posix-string-match "^\\** *Users on " message))
 (defun my-erc-hook (match-type nick message)
   "Shows a todochiku notification, when user's nick was
mentioned. If the buffer is currently not visible, makes it
sticky."
   (unless (or (posix-string-match "^\\*** " message)
               (posix-string-match "localhost has changed mode for " message)
               (posix-string-match "^<root>" message)
               (posix-string-match "as changed mode for " message))
     (todochiku-message
      (concat "ERC: " nick " mentioned on " (buffer-name (current-buffer)))
      message
      (todochiku-icon 'compile)
      )))

 (add-hook 'erc-text-matched-hook 'my-erc-hook)

 ;; (remove-hook 'erc-text-matched-hook  'my-erc-hook)

 (setq erc-dangerous-hosts '("localhost")))

(use-package wrap-region
  :ensure t
  :config (wrap-region-global-mode t))


;; (setq line-move-visual 'nil)

(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-vc-set-filter-groups-by-vc-root)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

;; Diminish modeline clutter
(require 'diminish)
(diminish 'wrap-region-mode)
;; (diminish 'yas/minor-mode)
;; (diminish 'auto-fill-mode) ;; errors!

(comment
 (setq erc-autojoin-channels-alist
       '(;("irc.skimlinks.com" "#dev")
                                        ;("localhost")
         ("irc.freenode.net" "#typed-clojure"))))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "secret.el" user-emacs-directory))
(if (file-exists-p abbrev-file-name)
    (load custom-file))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defadvice yank-pop (around kill-ring-browse-maybe (arg))
  "If last action was not a yank, run `browse-kill-ring' instead."
  ;; yank-pop has an (interactive "*p") form which does not allow
  ;; it to run in a read-only buffer.  We want browse-kill-ring to
  ;; be allowed to run in a read only buffer, so we change the
  ;; interactive form here.  In that case, we need to
  ;; barf-if-buffer-read-only if we're going to call yank-pop with
  ;; ad-do-it
  (interactive "p")
  (if (not (eq last-command 'yank))
      (helm-show-kill-ring)
    (barf-if-buffer-read-only)
    ad-do-it))
(ad-activate 'yank-pop)

(use-package rainbow-delimiters
  :ensure t)

(add-hook #'prog-mode-hook #'rainbow-delimiters-mode)

;; go
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook
          (lambda ()
            (local-set-key [f5] 'flycheck-previous-error)
            (local-set-key [f6] 'flycheck-next-error)))


(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))


;;(add-to-list 'dash-at-point-mode-alist '(python-mode . "python2")) ;; errors 2015-04-02

(defun clone-buffer-and-narrow-to-function ()
  (interactive)
  (clone-indirect-buffer-other-window (which-function) 'pop-to-buffer)
  (mark-defun) ; works not only in emacs-lisp, but C++, Python, ...
  (narrow-to-region (mark) (point))
  (pop-mark)
  (other-window 1))

;; (define-key global-map (kbd "C-x 4 n") 'clone-buffer-and-narrow-to-function) ; or whatever key you prefer


;; sudo stuff

;; (set-default 'tramp-default-proxies-alist (quote (("my-sudo-alias" nil "/ssh:chris@rogervm.skimlinks.com#17555:"))))

;; Haskell WIP
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setenv "PATH" (concat "/Applications/ghc-7.8.3.app/Contents/bin:" "~/.cabal/bin:" (getenv "PATH")))
(add-to-list 'exec-path "~/.cabal/bin")
(add-to-list 'exec-path "/Applications/ghc-7.8.3.app/Contents/bin")
;; Use Unicode arrows in place of ugly ASCII arrows
;; (require 'bodil-defuns)
(defun font-lock-replace-symbol (mode reg sym)
  (font-lock-add-keywords
   mode `((,reg
           (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                     ,sym 'decompose-region)))))))

;; arrows: ⬅ ⟵  ➫ ➙ →
(defun setup-haskell-arrows (mode mode-map)
  (font-lock-replace-symbol mode "\\(->\\)" "➙")
  (font-lock-replace-symbol mode "\\(<-\\)" "⟵")
  (font-lock-replace-symbol mode "\\(=>\\)" "⇒")

  (define-key mode-map (kbd "➙") (lambda () (interactive) (insert "->")))
  (define-key mode-map (kbd "⟵") (lambda () (interactive) (insert "<-")))
  (define-key mode-map (kbd "⇒") (lambda () (interactive) (insert "=>"))))
(eval-after-load "haskell-mode"
  '(setup-haskell-arrows 'haskell-mode haskell-mode-map))

(add-hook 'haskell-mode-hook
          (lambda ()
            (auto-complete-mode -1)
            (setq ghc-display-error 'minibuffer)))

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
;; run M-x haskell-process-load-or-reload
;; M-? to show error at cursor

;; end haskell
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; PureScript cheat mode
(define-derived-mode purescript-mode haskell-mode "PureScript"
  "Major mode for PureScript")
(add-to-list 'auto-mode-alist (cons "\\.purs\\'" 'purescript-mode))
(setup-haskell-arrows 'purescript-mode purescript-mode-map)


;; round quotes
(eval-after-load 'org
  '(define-key org-mode-map
     "\"" #'endless/round-quotes))

(defun endless/round-quotes (italicize)
  "Insert “” and leave point in the middle.
With prefix argument ITALICIZE, insert /“”/ instead (meant for
org-mode).
If inside a code-block, simply calls `self-insert-command'."
  (interactive "P")
  (if (and (derived-mode-p 'org-mode) (org-in-src-block-p))
      (call-interactively 'self-insert-command)
    (if (looking-at "”[/=_\\*]?")
        (goto-char (match-end 0))
      (when italicize
        (insert "//")
        (forward-char -1))
      (insert "“”")
      (forward-char -1))))
;; end round quotes


;; (define-derived-mode purescript-mode haskell-mode "PureScript"
;;   "Major mode for PureScript")
;; (add-to-list 'auto-mode-alist (cons "\\.purs\\'" 'purescript-mode))

(eval-after-load 'flycheck
  '(progn
     (flycheck-define-checker purs-check
       "Use purscheck to flycheck PureScript code."
       :command ("/Users/chris/code/scratch/purescript-chapter3/.cabal-sandbox/bin/purscheck" source source-original temporary-file-name)
       :error-patterns
       ((error line-start
               (or (and "Error at " (file-name)    " line " line ", column " column ":" (zero-or-more " "))
                   (and "\""        (file-name) "\" (line " line ", column " column "):"))
               (or (message (one-or-more not-newline))
                   (and "\n"
                        (message
                         (zero-or-more " ") (one-or-more not-newline)
                         (zero-or-more "\n"
                                       (zero-or-more " ")
                                       (one-or-more not-newline)))))
               line-end))
       :modes purescript-mode)
     (add-to-list 'flycheck-checkers 'purs-check)))

(provide 'purscheck)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))


;; (py-test-define-project
;;  :name "Classifier"
;;  :python-command "/Users/chris/Envs/classi/bin/python"
;;  :base-directory (expand-file-name "~/code/product-enrichment/classifier-py/")
;;  :test-runner (expand-file-name "/Users/chris/Envs/classi/bin/py.test")
;;  :working-directory (expand-file-name "~/code/product-enrichment/classifier-py/"))


;; (pprint (sort (.split (System/getProperty "java.class.path") ":")))


;; (use-package org-projectile
;;   ;; :bind (("C-c n p" . org-projectile:project-todo-completing-read)
;;   ;;        ("C-c c" . org-capture))
;;   :config
;;   (progn
;;     (setq org-projectile:projects-file
;;           "~/Dropbox/code/projects.org")
;;     (add-to-list 'org-capture-templates (org-projectile:project-todo-entry "p")))
;;   :ensure t)


;; http://www.emacswiki.org/emacs/TransposeWindows
(defun rotate-windows (arg)
  "Rotate your windows; use the prefix argument to rotate the other direction"
  (interactive "P")
  (if (not (> (count-windows) 1))
      (message "You can't rotate a single window!")
    (let* ((rotate-times (if (and (numberp arg) (not (= arg 0))) arg 1))
           (direction (if (or (< rotate-times 0) (equal arg '(4)))
                          'reverse
                        (lambda (x) x)))
           (i 0))
      (while (not (= rotate-times 0))
        (while  (< i (- (count-windows) 1))
          (let* ((w1 (elt (funcall direction (window-list)) i))
                 (w2 (elt (funcall direction (window-list)) (+ i 1)))
                 (b1 (window-buffer w1))
                 (b2 (window-buffer w2))
                 (s1 (window-start w1))
                 (s2 (window-start w2))
                 (p1 (window-point w1))
                 (p2 (window-point w2)))
            (set-window-buffer-start-and-point w1 b2 s2 p2)
            (set-window-buffer-start-and-point w2 b1 s1 p1)
            (setq i (1+ i))))

        (setq i 0
              rotate-times
              (if (< rotate-times 0) (1+ rotate-times) (1- rotate-times)))))))
