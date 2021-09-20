(when (featurep! spacemacs)
  (remove-hook 'org-load-hook #'+org-init-keybinds-h)
  (doom-init-ui-hook . spacemacs/home))

(map! :leader :desc "M-x" "SPC" #'execute-extended-command
      (:prefix-map ("g" . "git")
       (:when (featurep! :tools magit)
        :desc "Magit status"              "s"   #'magit-status)))

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))
