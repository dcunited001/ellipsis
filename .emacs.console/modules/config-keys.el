;; (use-package tab-bar) ;; just forget compiled lambdas
(global-set-key (kbd "C-<prior>") 'tab-bar-switch-to-tab)
(global-set-key (kbd "C-<next>") 'tab-bar-switch-to-recent-tab)
(global-set-key (kbd "C-M-S-<return>") 'duplicate-line)
(global-set-key (kbd "C-x M-f") 'find-file-at-point)

;; these are hit/miss in console (/maybe/ create a profile specific directory)
;;  .... or use protesilaos' package-everything approach (the correct answer)
(global-set-key (kbd "<f11> <f11>") #'maximize-window)
(global-set-key (kbd "S-<f11> S-<f11>") #'balance-windows)

(provide 'config-keys)
