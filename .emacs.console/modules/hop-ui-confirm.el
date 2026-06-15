(setq dired-deletion-confirmer 'y-or-n-p
      dired-confirm-shell-command 'y-or-n-p

      ;; dired-no-confirm '()

      ;; files/urs
      url-confirmation-func 'y-or-n-p
      url-cookie-confirmation 'y-or-n-p
      log-edit-confirm  'changed
      ;; log-edit-confirm 'yes-or-no-p ; defaults to 'changed

      ;; emacs
      confirm-kill-processes 'y-or-n-p
      confirm-kill-emacs 'yes-or-no-p

      ;; email
      message-confirm-send 'y-or-n-p

      ;; org
      org-table-fix-formulas-confirm nil ;; 'y-or-n-p ; no default is no

      ;; eglot

      ;; smerge-mode is lazy loaded, default: t
      smerge-change-buffer-confirm t)

(provide 'config-ui-confirm)
