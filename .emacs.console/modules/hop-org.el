;; NOTE: keep org nice and simple!

;; NOTE: C-c C-, for structure templates

(setq org-use-property-inheritance t
      org-log-done t
      org-list-allow-alphabetical t
      org-catch-invisible-edits 'smart
      org-export-with-sub-superscripts '{}
      ;; org-export-allow-bind-keywords t
      org-image-actual-width '(0.9)

      ;; from .emacs.guix
      org-capture-bookmark t ;; nil
      org-cycle-separator-lines 2
      org-eldoc-breadcrumb-separator " → "
      org-enforce-todo-dependencies t
      ;; startup
      org-startup-folded 'content
      org-startup-indented nil)

(defun dc/org-babel-do-load-shell ()
  (add-to-list 'org-babel-load-languages '((shell . t)))
  (org-babel-do-load-languages))

(provide 'config-org)
