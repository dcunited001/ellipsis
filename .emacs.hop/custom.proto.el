(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-file "~/.emacs.d/custom.el")
 '(package-selected-packages
   '(a ace-window avy cape consult consult-dir corfu corfu-terminal ef-themes
     embark embark-consult git-link json-mode lispy marginalia
     markdown-mode multiple-cursors no-littering orderless
     toml-mode vertico x509-mode xkb-mode yuck-mode zoutline))
 '(safe-local-variable-values
   '((eval let
      ((root-dir-unexpanded
	(locate-dominating-file default-directory ".dir-locals.el")))
      (when root-dir-unexpanded
       (let*
	   ((root-dir
	     (file-local-name (expand-file-name root-dir-unexpanded)))
	    (dc-dir
	     (directory-file-name
	      (file-local-name
	       (expand-file-name "dc" root-dir-unexpanded))))
	    (guix-pr
	     (or (and (bound-and-true-p guix-pulled-profile))
		 (expand-file-name ".config/guix/current"
				   (getenv "HOME"))))
	    (guix-lp (expand-file-name "share/guile/site/3.0" guix-pr))
	    (guix-lcp
	     (expand-file-name "lib/guile/3.0/site-ccache" guix-pr)))
	 (unless (boundp 'geiser-guile-load-path)
	   (defvar geiser-guile-load-path 'nil))
	 (make-local-variable 'geiser-guile-load-path) (require 'cl-lib)
	 (cl-dolist
	     (pathdir (list guix-lp guix-lcp dc-dir))
	   (cl-pushnew pathdir geiser-guile-load-path :test
		       #'string-equal)))))
     (eval add-to-list 'completion-ignored-extensions ".go")
     (texinfo-mode (indent-tabs-mode) (fill-column . 72))
     (emacs-lisp-mode (indent-tabs-mode)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'customize-variable 'disabled nil)
(put 'customize-browse 'disabled nil)
(put 'customize-changed 'disabled nil)
(put 'customize-rogue 'disabled nil)
(put 'customize-group 'disabled nil)
