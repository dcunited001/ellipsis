(use-package magit
  :demand t
  :bind ((:map global-map
               ("C-c v g" . #'magit-status)))
  :custom (magit-delete-by-moving-to-trash nil))

(provide 'config-vcs)
