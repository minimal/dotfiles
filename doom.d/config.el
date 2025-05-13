;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Load private scenario test helpers if they exist
(when (file-exists-p (expand-file-name "private/scenario-helpers.el" doom-user-dir))
  (load-file (expand-file-name "private/scenario-helpers.el" doom-user-dir)))

;; (remove-hook 'org-load-hook #'+org-init-keybinds-h)

(when (featurep! spacemacs)
  (remove-hook 'org-load-hook #'+org-init-keybinds-h)
  (doom-init-ui-hook . spacemacs/home))

(setq-default evil-escape-key-sequence "jk")
(map! :leader :desc "M-x" "SPC" #'execute-extended-command
      (:prefix-map ("g" . "git")
       (:when (featurep! :tools magit)
        :desc "Magit status"              "s"   #'magit-status)))

;; Lisp keybinds
(map! (:map smartparens-mode-map ;; Some default paredit binds
       :ni "C-)" #'sp-forward-slurp-sexp
       :ni "C-}" #'sp-forward-barf-sexp
       :ni "C-(" #'sp-backward-slurp-sexp
       :ni "C-{" #'sp-backward-barf-sexp
       :ni "C-k" #'sp-kill-hybrid-sexp)
      (:map cider-repl-mode-map
       :n "s-k" #'cider-repl-backward-input
       :n "s-j" #'cider-repl-forward-input))

(setq cider-print-fn "puget")

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

(use-package wakatime-mode
  :config (global-wakatime-mode))

(use-package just-mode)
(use-package! feature-mode
  :config
  (map! (:localleader
         (:map feature-mode-map
               "s"  #'de-run-scenario-test-at-point
               "f"  #'de-run-feature))))


(+global-word-wrap-mode +1)

(use-package blamer
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :family "san-serif"
                   :height 120
                   :italic t)))
  :config
  (global-blamer-mode 0))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Chris McDevitt"
      user-mail-address "")
;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-font (font-spec :family "PragmataPro Liga" :size 13 :weight 'normal))
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
;; Need this for emacsPgtkGcc terminal wsl for some reason

(map! [mouse-4] [wheel-up])
(map! [mouse-5] [wheel-down])

(defun native-running? ()
  (interactive)
  (if (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
      (message "Native compilation is available")
    (message "Native complation is *not* available")))

(use-package! tree-sitter
  :config
  (cl-pushnew (expand-file-name "~/.tree-sitter") tree-sitter-load-path)
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (add-to-list 'tree-sitter-major-mode-language-alist '(clojure-mode . clojure)))

(after! clojure-mode
  (put 'defrule 'clojure-doc-string-elt 2)

  (font-lock-add-keywords 'clojure-mode
                          `((,(concat "(\\(?:" clojure--sym-regexp "/\\)?"
                                      "\\(defrule\\)\\>")
                             1 font-lock-keyword-face)))
  (setq cider-font-lock-dynamically '(macro core function var)))


(set-formatter! 'alejandra "alejandra --quiet" :modes '(nix-mode))

(setq! electric-pair-mode t)

(use-package! justl)

;; magit difftastic setup

(defun th/magit--with-difftastic (buffer command)
  "Run COMMAND with GIT_EXTERNAL_DIFF=difft then show result in BUFFER."
  (let ((process-environment
         (cons (concat "GIT_EXTERNAL_DIFF=difft --width="
                       (number-to-string (frame-width)))
               process-environment)))
    ;; Clear the result buffer (we might regenerate a diff, e.g., for
    ;; the current changes in our working directory).
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer))
    ;; Now spawn a process calling the git COMMAND.
    (make-process
     :name (buffer-name buffer)
     :buffer buffer
     :command command
     ;; Don't query for running processes when emacs is quit.
     :noquery t
     ;; Show the result buffer once the process has finished.
     :sentinel (lambda (proc event)
                 (when (eq (process-status proc) 'exit)
                   (with-current-buffer (process-buffer proc)
                     (goto-char (point-min))
                     (ansi-color-apply-on-region (point-min) (point-max))
                     (setq buffer-read-only t)
                     (view-mode)
                     (end-of-line)
                     ;; difftastic diffs are usually 2-column side-by-side,
                     ;; so ensure our window is wide enough.
                     (let ((width (current-column)))
                       (while (zerop (forward-line 1))
                         (end-of-line)
                         (setq width (max (current-column) width)))
                       ;; Add column size of fringes
                       (setq width (+ width
                                      (fringe-columns 'left)
                                      (fringe-columns 'right)))
                       (goto-char (point-min))
                       (pop-to-buffer
                        (current-buffer)
                        `(;; If the buffer is that wide that splitting the frame in
                          ;; two side-by-side windows would result in less than
                          ;; 80 columns left, ensure it's shown at the bottom.
                          ,(when (> 80 (- (frame-width) width))
                             #'display-buffer-at-bottom)
                          (window-width
                           . ,(min width (frame-width))))))))))))


(defun th/magit-show-with-difftastic (rev)
  "Show the result of \"git show REV\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If REV is given, just use it.
          (when (boundp 'rev) rev)
          ;; If not invoked with prefix arg, try to guess the REV from
          ;; point's position.
          (and (not current-prefix-arg)
               (or (magit-thing-at-point 'git-revision t)
                   (magit-branch-or-commit-at-point)))
          ;; Otherwise, query the user.
          (magit-read-branch-or-commit "Revision"))))
  (if (not rev)
      (error "No revision specified")
    (th/magit--with-difftastic
     (get-buffer-create (concat "*git show difftastic " rev "*"))
     (list "git" "--no-pager" "show" "--ext-diff" rev))))


(defun th/magit-diff-with-difftastic (arg)
  "Show the result of \"git diff ARG\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If RANGE is given, just use it.
          (when (boundp 'range) range)
          ;; If prefix arg is given, query the user.
          (and current-prefix-arg
               (magit-diff-read-range-or-commit "Range"))
          ;; Otherwise, auto-guess based on position of point, e.g., based on
          ;; if we are in the Staged or Unstaged section.
          (pcase (magit-diff--dwim)
            ('unmerged (error "unmerged is not yet implemented"))
            ('unstaged nil)
            ('staged "--cached")
            (`(stash . ,value) (error "stash is not yet implemented"))
            (`(commit . ,value) (format "%s^..%s" value value))
            ((and range (pred stringp)) range)
            (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
  (let ((name (concat "*git diff difftastic"
                      (if arg (concat " " arg) "")
                      "*")))
    (th/magit--with-difftastic
     (get-buffer-create name)
     `("git" "--no-pager" "diff" "--ext-diff" ,@(when arg (list arg))))))

(transient-define-prefix th/magit-aux-commands ()
  "My personal auxiliary magit commands."
  ["Auxiliary commands"
   ("d" "Difftastic Diff (dwim)" th/magit-diff-with-difftastic)
   ("s" "Difftastic Show" th/magit-show-with-difftastic)])

(after! magit
 (transient-append-suffix 'magit-dispatch "!"
   '("#" "My Magit Cmds" th/magit-aux-commands))

 (define-key magit-status-mode-map (kbd "#") #'th/magit-aux-commands))

;; end magit difftastic

(use-package! clean-kill-ring
  :config (clean-kill-ring-mode 1))
(use-package! boxquote)
(comment
 (use-package! editorconfig
   :config
   (editorconfig-mode 1)))


;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(setq copilot-node-executable "/Users/chris.mcdevitt/.nix-profile/bin/node")

(defun set-region-read-only (begin end)
  "Sets the read-only text property on the marked region.

Use `set-region-writeable' to remove this property."
  ;; See https://stackoverflow.com/questions/7410125
  (interactive "r")
  (with-silent-modifications
    (put-text-property begin end 'read-only t)))

(defun set-region-writeable (begin end)
  "Removes the read-only text property from the marked region.

Use `set-region-read-only' to set this property."
  ;; See https://stackoverflow.com/questions/7410125
  (interactive "r")
  (with-silent-modifications
    (remove-text-properties begin end '(read-only t))))
(setq! copilot-indent-offset-warning-disable t)


(use-package! difftastic
  ;; :demand t
  :bind (:map magit-blame-read-only-mode-map
              ("D" . difftastic-magit-show)
              ("S" . difftastic-magit-show))
  :config
  (eval-after-load 'magit-diff
    '(transient-append-suffix 'magit-diff '(-1 -1)
       [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)])))
(use-package! pcre2el)
(use-package! rxt)
(use-package! epoch-view
  :defer t
  :config
  (defun epoch-view--render-time (text)
    "Render the time portion of an epoch match from TEXT.
     Override to work with ms timestamps."
    (let ((epoch-time (/ (car (read-from-string text)) 1000.0)))
      (format-time-string
       epoch-view-time-format
       (seconds-to-time epoch-time))))

  (defun epoch-view--render (text)
    "Override `epoch-view--render' to avoid showing non-decoded text"
    (epoch-view--render-time text))

  (defun epoch-view-render ()
    "Override `epoch-view-render' to set a different face"
    (let ((text (match-string-no-properties 0)))
      `(face bold
        display ,(epoch-view--render text)))))

;; defvar doesn't work from :config ?
(defvar epoch-view-font-lock-keywords
  '(("\\<[0-9]\\{13\\}\\>"
     (0 (epoch-view-render))))
  "Font-lock keywords of epoch timestamps.")

(put 'lsp-booster--advice-json-parse 'native-comp-never-compile t)

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))

;; Optional: Use non-erroring require and defer execution slightly
(eval-after-load 'json
  '(advice-add (if (fboundp 'json-parse-buffer)
                   'json-parse-buffer
                 'json-read)
    :around
    #'lsp-booster--advice-json-parse))

;; Ensure json is loaded if not already by something else
(require 'json nil t)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
