(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)
;; really make suspend frame get in the sea
(global-unset-key (kbd "C-z"))
(cua-mode)
(global-unset-key (kbd "C-x C-z"))
(put 'suspend-frame 'disabled t)

;(save-place t nil (saveplace))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(persistent-scratch-setup-default) ;; use 1 for not auto restore

;; (setq x-super-keysym 'meta) ; make cmd key as meta - for apple keyboard on linux
(setq mac-command-modifier 'meta) ;; on osx set command to meta

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

;; from mac port emacs
(mac-auto-operator-composition-mode)
;; end railwaycat
(org-babel-load-file "/Users/christophermcdevitt/code/dotfiles/.emacs.d/conf.org")

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

(defun theme-dark ()
  "Flatland with smart modeline"
  (interactive)
  (helm-themes--load-theme "flatland")
  (smart-mode-line-enable)
  (sml/apply-theme "dark"))

(defun theme-spacemacs-dark ()
  "Flatland with smart modeline"
  (interactive)
  (helm-themes--load-theme "spacemacs-dark")
  (smart-mode-line-enable)
  (sml/apply-theme "dark"))

(defun theme-charcoal ()
  "Soft charcoal with smart modeline"
  (interactive)
  (helm-themes--load-theme "soft-charcoal")
  (smart-mode-line-enable)
  (sml/apply-theme "dark"))

(defun theme-material ()
  "Soft charcoal with smart modeline"
  (interactive)
  (helm-themes--load-theme "material")
  (smart-mode-line-enable)
  (sml/apply-theme "dark"))

(defun theme-light ()
  "Soft morning with smart modeline"
  (interactive)
  (helm-themes--load-theme "soft-morning")
  (smart-mode-line-enable)
  (sml/apply-theme "light"))

;; (theme-dark)
;; (theme-spacemacs-dark)
;; (theme-charcoal)
;; (theme-material) ;; causes crashes

(setq-default indicate-empty-lines t
        indicate-buffer-boundaries 'left)

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

;; Notifications
;; look at erc-track-exclude-types variable
;; Diminish modeline clutter
(require 'diminish)
(diminish 'wrap-region-mode)
;; (diminish 'aggressive-indent-mode)
;; (diminish 'highlight-indentation-mode)
;; (diminish 'yas/minor-mode)
;; (diminish 'auto-fill-mode) ;; errors!

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

;; (pprint (sort (.split (System/getProperty "java.class.path") ":")))

(defun xah-copy-file-path (&optional φdir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.
URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2015-12-02"
  (interactive "P")
  (let ((ξfpath
         (if (equal major-mode 'dired-mode)
             (expand-file-name default-directory)
           (if (null (buffer-file-name))
               (user-error "Current buffer is not associated with a file.")
             (buffer-file-name)))))
    (kill-new
     (if (null φdir-path-only-p)
         (progn
           (message "File path copied: 「%s」" ξfpath)
           ξfpath
           )
       (progn
         (message "Directory path copied: 「%s」" (file-name-directory ξfpath))
         (file-name-directory ξfpath))))))


(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))


(eval-after-load 'edit-server
    '(add-to-list 'edit-server-url-major-mode-alist
                  '("inbox\\.google\\." . gmail-message-edit-server-mode)))

;; org latex export
(require 'ox-latex)
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))
(comment
 (add-to-list 'org-latex-classes
              '("article"
                "\\documentclass{article}"
                ("\\section{%s}" . "\\section*{%s}"))))


(add-to-list 'org-latex-classes
          '("koma-article"
             "\\documentclass{scrartcl}"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
