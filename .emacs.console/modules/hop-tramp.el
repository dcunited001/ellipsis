(use-package tramp
  :demand t
  :config
  (require 'tramp-container)
  (cl-dolist (p '("~/.guix-profile/bin"
                  "~/.guix-profile"
                  "~/.nix-profile"
                  "/run/current-system/profile/bin"
                  "/run/current-system/profile/sbin"
                  "/run/current-system/sw/sbin"))
    (add-to-list 'tramp-remote-path p))
  :custom ((tramp-default-method "ssh")))

(provide 'config-tramp)
