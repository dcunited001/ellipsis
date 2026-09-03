;;; .doom.d/modules/dc-bufler.el -*- lexical-binding: t; -*-

(setf bufler-groups (dc/bufler-defgroups))

(defun dc/bufler-defgroups ()
  "Run `bufler-defgroups'"
  (bufler-defgroups
    (group (auto-workspace))
    (group (group-or "*Help/Info*"
                     (mode-match "*Help*" (rx bos "help-"))
                     (mode-match "*helpful*" (rx bos "helpful"))
                     (mode-match "*Info*" (rx bos "info-"))))
    (group (group-and "*Special*"
                      (lambda (buffer)
                        (unless (or (funcall (mode-match "Magit" (rx bos "magit-status")) buffer)
                                    (funcall (mode-match "Dired" (rx bos "dired")) buffer)
                                    (funcall (auto-file) buffer))
                          "*Special*")))
           (group (name-match "**Special**"
                              (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace") "*")))
           (group (mode-match "*Magit* (non-status)" (rx bos (or "magit" "forge") "-"))
                  (auto-directory))
           (mode-match "*Helm*" (rx bos "helm-"))
           (auto-mode))
    
    ;; preemptively hide dired in several layers (use bufler-filter-buffer-modes)
    ;; 
    ;; (group (group-and "*Dired*" (mode-match "Dired" (rx bos "dired")))
    ;;        (group
    ;;         (group-and "*Dired2*" (mode-match "Dired" (rx bos "dired")))
    ;;         (group (group-and "*Dired3*" (mode-match "Dired" (rx bos "dired")))
    ;;                (auto-file))))
    
    (group (dir "~/.dotfiles/nixos")
           (group (auto-indirect) (auto-file))
           (group-and "*special*" (auto-indirect))
           (auto-mode))
    (group (dir "~/.dotfiles/guix")         
           (group (auto-indirect) (auto-file))
           (group-not "*special*" (auto-file))
           (auto-mode))
    (group (dir "~/.dotfiles/.config/hypr")         
           (group (auto-indirect) (auto-file))
           (group-not "*special*" (auto-file))
           (auto-mode))
    (group
     (dir (if (bound-and-true-p doom-user-dir) doom-user-dir "~/.doom.d"))
     (group (auto-indirect) (auto-file))
     (group-not "*special*" (auto-file))
     (auto-mode))
    (group
     (dir (if (bound-and-true-p org-directory) org-directory "~/org"))
     (group (auto-indirect) (auto-file))
     (group-not "*special*" (auto-file))
     (auto-mode))
    (group
     (dir (if (file-exists-p (expand-file-name ".local/straight/repos" user-emacs-directory))
              (expand-file-name ".local/straight/repos" user-emacs-directory) "~/.emacs.doom/.local/straight/repos"))
     (group (auto-indirect) (auto-file))
     (group-not "*special*" (auto-file))
     (auto-mode))
    
    (group (auto-projectile))
    (group (auto-project))
    (dir user-emacs-directory)
    ;; (auto-directory)
    (auto-mode)))

(defun dc/bufler-defgroups2 ()
  "The predicates are alright, but i don't understand the hierachy/matchers"
  (bufler-defgroups
    (group (auto-workspace))
    (group (group-or "*Help/Info*"
                     (mode-match "*Help*" (rx bos "help-"))
                     (mode-match "*Info*" (rx bos "info-"))))
    (group
     (group-and "~/forge" (dir "~/forge")
                (group-or "forge-projects" (auto-projectile) (auto-project)))
     (group-and "~/Work/tries" (dir "~/Work/tries")
                (group-or "Work/tries-projects" (auto-projectile) (auto-project))))
    (group
     (group
      (group-and "~/.dotfiles/.doom.d" (dir "~/.dotfiles/.doom.d") (auto-mode) (auto-directory)))
     (group
      (group-and "~/.dotfiles/nixos" (dir "~/.dotfiles/nixos") (auto-mode) (auto-directory)))
     (group
      (group-and "~/.dotfiles/guix" (dir "~/.dotfiles/guix")  (auto-mode) (auto-directory)))
     (group
      (group-and "~/.dotfiles/.config/hypr" (dir "~/.dotfiles/.config/hypr")  (auto-directory) (auto-mode)))
     (group
      (group-and "~/.dotfiles" (auto-projectile) (auto-project))))
    
    ;; (dir "~/.dotfiles/")
    ;; (dir "~/.dotfiles/")
    ;; (dir "~/.dotfiles/")
    ;; (dir "~/.dotfiles/")
    
    ;; Subgroup collecting all special buffers (i.e. ones that are not
    ;; file-backed), except `magit-status-mode' buffers (which are allowed to fall
    ;; through to other groups, so they end up grouped with their project buffers).
    (group
     (group-and "*Special*"
                (lambda (buffer)
                  (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
                                       buffer)
                              (funcall (mode-match "Dired" (rx bos "dired"))
                                       buffer)
                              (funcall (auto-file) buffer))
                    "*Special*")))
     
     ;; Subgroup collecting these "special special" buffers
     ;; separately for convenience.
     (group
      (name-match "**Special**"
                  (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace") "*")))
     ;; Subgroup collecting all other Magit buffers, grouped by directory.
     (group
      (mode-match "*Magit* (non-status)" (rx bos (or "magit" "forge") "-"))
      (auto-directory))
     ;; Subgroup for Helm buffers.
     (mode-match "*Helm*" (rx bos "helm-"))
     ;; Remaining special buffers are grouped automatically by mode.
     (auto-mode))
    
    ;; All buffers under "~/.emacs.d" (or wherever it is).
    (dir user-emacs-directory)
    (dir doom-user-dir)
    (group
     ;; Subgroup collecting buffers in `org-directory' (or "~/org" if
     ;; `org-directory' is not yet defined).
     (dir (if (bound-and-true-p org-directory)
              org-directory
            "~/org"))
     (group
      ;; Subgroup collecting indirect Org buffers, grouping them by file.
      ;; This is very useful when used with `org-tree-to-indirect-buffer'.
      (auto-indirect)
      (auto-file))
     ;; Group remaining buffers by whether they're file backed, then by mode.
     (group-not "*special*" (auto-file))
     (auto-mode))
    
    (group
     (group-and "/data/ecto" (dir "/data/ecto")
                (group-or "ecto-projects" (auto-projectile) (auto-project)))
     (group-and "/data/repo" (dir "/data/repo")
                (group-or "repo-projects" (auto-projectile) (auto-project))))    
    ;; (group (auto-projectile))
    ;; (group (auto-project))
    (auto-directory)
    (auto-mode)))

(provide 'dc-bufler)
