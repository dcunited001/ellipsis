;; (use-package tab-bar) ;; just forget compiled lambdas
(global-set-key (kbd "C-<prior>") 'tab-bar-switch-to-tab)
(global-set-key (kbd "C-<next>") 'tab-bar-switch-to-recent-tab)
(global-set-key (kbd "C-M-S-<return>") 'duplicate-line)
(global-set-key (kbd "C-x M-f") 'find-file-at-point)

(provide 'config-keys)
