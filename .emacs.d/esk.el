;; stuff to keep from old starter kit

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  ;;(tooltip-mode -1)
  ;;(mouse-wheel-mode t)
  ;;(blink-cursor-mode -1)
  )


(setq
      inhibit-startup-message t
      ;; color-theme-is-global t
      ;; sentence-end-double-space nil
      shift-select-mode nil
      mouse-yank-at-point t
      ;; uniquify-buffer-name-style 'forward
      ;; whitespace-style '(face trailing lines-tail tabs)
      ;; whitespace-line-column 80
      ;; ediff-window-setup-function 'ediff-setup-windows-plain
      ;; oddmuse-directory (concat user-emacs-directory "oddmuse")
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      diff-switches "-u")

(defalias 'yes-or-no-p 'y-or-n-p)

(load-file "~/.emacs.d/starter-kit-defuns.el")
