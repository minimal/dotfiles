;; emacs customisations after using emacs starter kit.
;; font


(setq-default tab-width 4)
(setq-default whitespace-tab-width 4)
(setq column-number-mode t)
(setq js-indent-level 4)
(setq recentf-max-menu-items 50)
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(global-linum-mode 1)
(setq linum-format " %d  ")  ;; reduce glitching with fring

;; backup http://pejusdas.com/content/emacs-backup-files also see:
;; http://amitp.blogspot.com/2007/03/emacs-move-autosave-and-backup-files.html

(setq kept-new-versions 5)
(setq kept-old-versions 5)

(menu-bar-mode 1)
;(cua-mode t nil (cua-base))
;(save-place t nil (saveplace))

(setq-default cursor-type 't)
(winner-mode 1) ;; C-c left => undo window layout change, C-c right => undo

;; bindings

;; (setq x-super-keysym 'meta) ; make cmd key as meta - for apple keyboard on linux
(setq mac-command-modifier 'meta) ;; on osx set command to meta
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
(global-set-key [f8] 'dired-jump)
(global-set-key (kbd "<M-f8>") 'ido-find-file)
(global-set-key [(shift f8)] 'ido-find-file-other-window)
;; f10 is menu-bar-open
(global-set-key (kbd "<S-f9>") 'ido-switch-buffer-other-window)
(set-register ?e '(file . "~/.emacs.d/chris.el")) ; 'C-x r j e' opens this file
(define-key global-map (kbd "C-;") 'iedit-mode)
(define-key global-map (kbd "C-3") 'comment-or-uncomment-region-or-line)
(define-key global-map (kbd "M-3") 'comment-or-uncomment-region-or-line)
(global-set-key [(meta \])] 'textmate-shift-right)
(global-set-key [(meta \[)] 'textmate-shift-left)
;; (global-set-key [(control q)] 'fill-paragraph) ;; virtual box
;; overrides meta-q


(global-set-key [(meta m)] 'jump-char-forward)
(global-set-key [(shift meta m)] 'jump-char-backward)
(global-set-key [(meta i)] 'back-to-indentation)
(global-set-key (kbd "M-#") 'ace-jump-mode)

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "M-K")
                (lambda ()
                  (interactive)
                  (join-line -1)))

;; helm

;; more options http://pages.sachachua.com/.emacs.d/Sacha.html#unnumbered-14
(use-package helm
  :ensure helm
  :diminish helm-mode
  :init
  (progn
    (setq helm-quick-update t
          helm-ff-skip-boring-files t)
    (helm-mode))
                                        ;:config
  :bind (("<f9>" . helm-mini)
         ("M-<f9>" . helm-projectile-find-file)
         ("M-S-<f9>" . helm-projectile)
         ("<f11>" . helm-semantic-or-imenu)
         ("M-x" . helm-M-x)))

(use-package helm-swoop
  :bind
  (("C-M-s" . helm-swoop)
   ;; ("C-S-s" . helm-swoop)
   ;; ("M-i" . helm-swoop)
   ;; ("M-s s" . helm-swoop)
   ;; ("M-s M-s" . helm-swoop)
   ("M-I" . helm-swoop-back-to-last-point)
   ("C-c M-i" . helm-multi-swoop)
   ("C-x M-i" . helm-multi-swoop-all)
   )
  :config
  (progn
    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
    (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)))

(use-package projectile
  :ensure projectile
  :diminish projectile-mode
  :init
  (progn
    ;; (setq projectile-keymap-prefix (kbd "C-c p"))
    ;; (setq projectile-completion-system 'default)
    ;; (setq projectile-enable-caching t)
    (projectile-global-mode)))

(use-package helm-projectile
  :ensure helm-projectile)

;; end helm

(use-package drag-stuff
  :ensure t
  :bind
  (("M-n" . drag-stuff-down)
   ("M-p" . drag-stuff-up))
  :init
  (progn
    (drag-stuff-global-mode)))

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
(add-hook 'python-mode-hook
      (lambda ()
            ;dont invoke flymake on temporary buffers for the interpreter
            (unless (eq buffer-file-name nil) (flycheck-mode 1))
            (local-set-key [f5] 'flycheck-previous-error)
            (local-set-key [f6] 'flycheck-next-error)
            ;; if tabs make sure they are 4 spaces wide
            (set (make-local-variable 'tab-width) 4)
))

;; activate minor whitespace mode when in python mode
;; (add-hook 'python-mode-hook 'whitespace-mode)
(add-hook 'python-mode-hook 'auto-complete-mode)
(add-hook 'python-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\<\\(FIXME\\|TODO\\|BUG\\|XXX\\):" 1 font-lock-warning-face t)))
            (local-set-key (kbd "M-/") 'hippie-expand)
            ;; (local-set-key (kbd "M-SPC") 'rope-code-assist)
            (local-set-key (kbd "M-RET") 'newline)))

;; (add-hook 'python-mode-hook
;;           (lambda()
;;             (add-hook 'local-write-file-hooks
;;                       '(lambda()
;;                          (save-excursion
;;                            (whitespace-cleanup))))))

;; To get jedi completion with a venv:
;; * venv-workon <env>
;; * jedi:stop-server

(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'js-mode-hook 'flymake-jshint-load)

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.gitconfig\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.cs\\'" . coffee-mode)) ;; /sigh
(add-to-list 'auto-mode-alist '("\\.slate\\'" . conf-mode))

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

(setq erc-dangerous-hosts '("localhost"))

;; Annoying arrows mode <- too annoying!
;; (add-to-list 'load-path "~/.emacs.d/vendor/annoying-arrows-mode.el/")
;; (require 'annoying-arrows-mode)
;; (global-annoying-arrows-mode)

(wrap-region-global-mode t)

;; (setq line-move-visual 'nil)

(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-vc-set-filter-groups-by-vc-root)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

;; (add-hook 'nrepl-interaction-mode-hook
;;           'nrepl-turn-on-eldoc-mode)
;; (setq nrepl-popup-stacktraces nil)
;; (add-to-list 'same-window-buffer-names "*nrepl*")


;; Diminish modeline clutter
(require 'diminish)
(diminish 'wrap-region-mode)
;; (diminish 'yas/minor-mode)
;; (diminish 'auto-fill-mode) ;; errors!

(setq erc-autojoin-channels-alist
      '(;("irc.skimlinks.com" "#dev")
        ;("localhost")
        ("irc.freenode.net" "#typed-clojure")))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "secret.el" user-emacs-directory))
(load custom-file)


(ethan-wspace-mode)

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

(global-rainbow-delimiters-mode t)

;; Ido-mode customizations
(setq ido-decorations                                                      ; Make ido-mode display vertically
      (quote
       ("\n-> "           ; Opening bracket around prospect list
        ""                ; Closing bracket around prospect list
        "\n   "           ; separator between prospects
        "\n   ..."        ; appears at end of truncated list of prospects
        "["               ; opening bracket around common match string
        "]"               ; closing bracket around common match string
        " [No match]"     ; displayed when there is no match
        " [Matched]"      ; displayed if there is a single match
        " [Not readable]" ; current diretory is not readable
        " [Too big]"      ; directory too big
        " [Confirm]")))   ; confirm creation of new file or buffer

(add-hook 'ido-setup-hook                                                  ; Navigate ido-mode vertically
          (lambda ()
            (define-key ido-completion-map [down] 'ido-next-match)
            (define-key ido-completion-map [up] 'ido-prev-match)
            (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
            (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)))

;; go
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook
          (lambda ()
            (local-set-key [f5] 'flycheck-previous-error)
            (local-set-key [f6] 'flycheck-next-error)))


(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

;; clojure

;; Reloaded reset from and clojure buffer
(defun cider-namespace-refresh ()
  (interactive)
  (save-some-buffers)
  (with-current-buffer (cider-current-repl-buffer)
    (cider-interactive-eval
     "(reloaded.repl/reset)")))

;; Use this to define add hoc reset
;; (define-key clojure-mode-map (kbd "M-r")
;;   (lambda ()
;;     (interactive)
;;     (cider-interactive-eval
;;       "(require '[clojure.pprint :refer [pprint]])
;;        (pprint @interesting-atom)")))

(defun cider-eval-expression-at-point-in-repl ()
  (interactive)
  (let ((form (cider-sexp-at-point)))
    ;; Strip excess whitespace
    (while (string-match "\\`\s+\\|\n+\\'" form)
      (setq form (replace-match "" t t form)))
    (with-current-buffer (cider-current-repl-buffer)
      (goto-char (point-max))
      (insert form)
      (cider-repl-return))))

(use-package cider
  :ensure t
  :commands (cider-jack-in cider)
  :config
  (progn
    (add-hook #'cider-mode-hook
              (lambda ()
                (cider-turn-on-eldoc-mode)
                (company-mode)
                (flycheck-mode)))
    (add-hook #'cider-repl-mode-hook
              (lambda ()
                (company-mode)
                (enable-paredit-mode)
                (setq cider-stacktrace-fill-column t
                      cider-repl-print-length 100)))
    (require 'squiggly-clojure)
    ;;nrepl-hide-special-buffers t
    )
  :bind (("C-x M-r" . cider-namespace-refresh)
         ("C-`" . cider-eval-expression-at-point-in-repl)))

(use-package clojure-mode
  :ensure t
  :config
  (progn
    (add-hook #'clojure-mode-hook
              (lambda ()
                (auto-complete-mode -1)
                (clj-refactor-mode)
                (aggressive-indent-mode)
                (highlight-indentation-mode)))))

;; indentation tweaks for korma etc
(add-hook
 'clojure-mode-hook
 (lambda ()
   (define-clojure-indent
     (copy 2)
     (create-table 1)
     (delete 1)
     (drop-table 1)
     (insert 2)
     (select 1)
     (truncate 1)
     (update 2)
     (dom/div 2)
     (dom/ 2)
     (alter-var-root 1)
     ;; storm
     (nextTuple 1)
     ;;cats
     (mlet 1)
     ;; manifold
     (let-flow 1)
     ;; riemann
     (tagged 1)
     (where 1)
     (rollup 2)
     (by 1)
     (with 1)
     (splitp 2)
     (percentiles 2)
     )))

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

(add-to-list 'dash-at-point-mode-alist '(python-mode . "python2"))

(defun clone-buffer-and-narrow-to-function ()
      (interactive)
      (clone-indirect-buffer-other-window (which-function) 'pop-to-buffer)
      (mark-defun) ; works not only in emacs-lisp, but C++, Python, ...
      (narrow-to-region (mark) (point))
      (pop-mark)
      (other-window 1))
(require 'region-bindings-mode)
(region-bindings-mode-enable)
(define-key region-bindings-mode-map "a" 'mc/mark-all-like-this)
(define-key region-bindings-mode-map "p" 'mc/mark-previous-like-this)
(define-key region-bindings-mode-map "n" 'mc/mark-next-like-this)
(define-key region-bindings-mode-map "m" 'mc/mark-more-like-this-extended)
(define-key region-bindings-mode-map "s" 'mc/skip-to-next-like-this)

;; (define-key global-map (kbd "C-x 4 n") 'clone-buffer-and-narrow-to-function) ; or whatever key you prefer


;; sudo stuff

;; (set-default 'tramp-default-proxies-alist (quote (("my-sudo-alias" nil "/ssh:chris@rogervm.skimlinks.com#17555:"))))

;; (require 'flx-ido)
;; (ido-mode 1)
;; (ido-everywhere 1)
;; (flx-ido-mode 1)
;; disable ido faces to see flx highlights.
;; (setq ido-use-faces nil)


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


(use-package org-projectile
  ;; :bind (("C-c n p" . org-projectile:project-todo-completing-read)
  ;;        ("C-c c" . org-capture))
  :config
  (progn
    (setq org-projectile:projects-file
          "~/Dropbox/code/projects.org")
    (add-to-list 'org-capture-templates (org-projectile:project-todo-entry "p")))
  :ensure t)

;;http://sachachua.com/blog/2014/12/emacs-kaizen-ace-jump-zap-lets-use-c-u-zap-character/
(use-package ace-jump-zap
  :ensure ace-jump-zap
  :bind
  (("M-z" . ace-jump-zap-up-to-char-dwim)
   ("C-M-z" . ace-jump-zap-to-char-dwim)))

;; http://ericjmritz.name/2014/12/23/using-quickrun-in-emacs/
;; try quickrun-region, quickrun-replace-region
(use-package quickrun
  :ensure t)
