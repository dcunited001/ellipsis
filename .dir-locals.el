((nil
  . ((fill-column . 78)
     (sentence-end-double-space . t)
     (emacs-lisp-mode . ((indent-tabs-mode . nil)))
     (texinfo-mode    . ((indent-tabs-mode . nil)
                         (fill-column . 72)))

     (eval . (add-to-list 'completion-ignored-extensions ".go"))

     ;; (eval . (with-eval-after-load 'geiser-guile
     ;;           (let ((root-dir
     ;;                  (file-name-directory
     ;;                   (locate-dominating-file default-directory ".dir-locals.el"))))
     ;;             (unless (member root-dir geiser-guile-load-path)
     ;;               (setq-local geiser-guile-load-path
     ;;                           (cons root-dir geiser-guile-load-path))))))


     ;; altered to ensure that guix load path is set for the
     (eval
      . (let ((root-dir-unexpanded (locate-dominating-file
                                    default-directory ".dir-locals.el")))
          ;; While Guix should in theory always have a .dir-locals.el
          ;; (we are reading this file, after all) there seems to be a
          ;; strange problem where this code "escapes" to some other buffers,
          ;; at least vc-mode.  See:
          ;;   https://lists.gnu.org/archive/html/guix-devel/2020-11/msg00296.html
          ;; Upstream report: <https://bugs.gnu.org/44698>
          ;; Hence the following "when", which might otherwise be unnecessary;
          ;; it prevents causing an error when root-dir-unexpanded is nil.
          (when root-dir-unexpanded
            (let* ((root-dir (file-local-name (expand-file-name root-dir-unexpanded)))
                   ;; (directory-file-name ...) is Workaround for bug https://issues.guix.gnu.org/43818?
                   (dc-dir (directory-file-name (file-local-name (expand-file-name "dc" root-dir-unexpanded))))
                   (ellipsis-dir (directory-file-name (file-local-name (expand-file-name "ellipsis" root-dir-unexpanded))))
                   ;; TODO: guix-pulled-profile doesn't exist before
                   ;; guix.el repl runs
                   (guix-pr (or (and (bound-and-true-p guix-pulled-profile))
                                (expand-file-name ".config/guix/current" (getenv "HOME"))))
                   (guix-lp (expand-file-name "share/guile/site/3.0"
                                              guix-pr))
                   (guix-lcp (expand-file-name "lib/guile/3.0/site-ccache"
                                               guix-pr)))
              (unless (boundp 'geiser-guile-load-path)
                (defvar geiser-guile-load-path '()))
              (make-local-variable 'geiser-guile-load-path)
              (require 'cl-lib)
              (cl-dolist (pathdir (list guix-lp guix-lcp ellipsis-dir dc-dir))
                (cl-pushnew pathdir geiser-guile-load-path
                            :test #'string-equal))))))))

 (shell-mode
  (eval . (add-hook 'shell-mode-hook 'guix-build-log-minor-mode)))
 ;; (hyprlang-ts-mode
 ;;  (eval . (add-hook 'hyprlang-ts-mode-hook #'lsp)))

 (c-mode
  . ((c-file-style . "gnu")))
 (lisp-mode
  . ((sly-contribs nil)
     (eval . (cl-flet ((enhance-imenu-lisp
                         (&rest keywords)
                         (dolist (keyword keywords)
                           (let ((prefix (when (listp keyword) (cl-second keyword)))
                                 (keyword (if (listp keyword)
                                              (cl-first keyword)
                                            keyword)))
                             (add-to-list
                              'lisp-imenu-generic-expression
                              (list (purecopy (concat (capitalize keyword)
                                                      (if (string= (substring-no-properties keyword -1) "s")
                                                          "es"
                                                        "s")))
                                    (purecopy (concat "^\\s-*("
                                                      (regexp-opt
                                                       (list (if prefix
                                                                 (concat prefix "-" keyword)
                                                               keyword)
                                                             (concat prefix "-" keyword))
                                                       t)
                                                      "\\s-+\\(" lisp-mode-symbol-regexp "\\)"))
                                    2))))))
               ;; This adds the argument to the list of imenu known keywords.
               (enhance-imenu-lisp
                '("bookmarklet-command" "define")
                '("class" "define")
                '("command" "define")
                '("ffi-method" "define")
                '("ffi-generic" "define")
                '("function" "define")
                '("internal-page-command" "define")
                '("internal-page-command-global" "define")
                '("mode" "define")
                '("parenscript" "define")
                "defpsmacro")))))

 (scheme-mode
  (eval . (and (featurep 'guix) (add-hook 'scheme-mode-hook 'guix-devel-mode)))
  (eval . (and (featurep 'prism) (add-hook 'scheme-mode-hook 'prism-mode)))
  (eval . (add-to-list 'ffap-alist '("\\.patch" . guix-devel-ffap-patch)))

  (indent-tabs-mode . nil)
  (tab-width   .  8)

  (eval . (put 'eval-when 'scheme-indent-function 1))
  (eval . (put 'call-with-prompt 'scheme-indent-function 1))
  (eval . (put 'test-assert 'scheme-indent-function 1))
  (eval . (put 'test-assertm 'scheme-indent-function 2))
  (eval . (put 'test-equalm 'scheme-indent-function 1))
  (eval . (put 'test-equal 'scheme-indent-function 1))
  (eval . (put 'test-eq 'scheme-indent-function 1))
  (eval . (put 'call-with-input-string 'scheme-indent-function 1))
  (eval . (put 'call-with-port 'scheme-indent-function 1))
  (eval . (put 'guard 'scheme-indent-function 1))
  (eval . (put 'lambda* 'scheme-indent-function 1))
  (eval . (put 'substitute* 'scheme-indent-function 1))
  (eval . (put 'match-record 'scheme-indent-function 3))
  (eval . (put 'match-record-lambda 'scheme-indent-function 2))

  ;; TODO: Contribute these to Emacs' scheme-mode.
  (eval . (put 'let-keywords 'scheme-indent-function 3))

  ;; 'modify-inputs' and its keywords.
  (eval . (put 'modify-inputs 'scheme-indent-function 1))
  (eval . (put 'replace 'scheme-indent-function 1))

  ;; 'modify-phases' and its keywords.
  (eval . (put 'modify-phases 'scheme-indent-function 1))
  (eval . (put 'replace 'scheme-indent-function 1))
  (eval . (put 'add-before 'scheme-indent-function 2))
  (eval . (put 'add-after 'scheme-indent-function 2))

  (eval . (put 'modify-services 'scheme-indent-function 1))
  (eval . (put 'with-directory-excursion 'scheme-indent-function 1))
  (eval . (put 'with-file-lock 'scheme-indent-function 1))
  (eval . (put 'with-file-lock/no-wait 'scheme-indent-function 1))
  (eval . (put 'with-profile-lock 'scheme-indent-function 1))
  (eval . (put 'with-writable-file 'scheme-indent-function 2))

  (eval . (put 'package 'scheme-indent-function 0))
  (eval . (put 'package/inherit 'scheme-indent-function 1))
  (eval . (put 'origin 'scheme-indent-function 0))
  (eval . (put 'build-system 'scheme-indent-function 0))
  (eval . (put 'bag 'scheme-indent-function 0))
  (eval . (put 'graft 'scheme-indent-function 0))
  (eval . (put 'operating-system 'scheme-indent-function 0))
  (eval . (put 'file-system 'scheme-indent-function 0))
  (eval . (put 'manifest-entry 'scheme-indent-function 0))
  (eval . (put 'manifest-pattern 'scheme-indent-function 0))
  (eval . (put 'substitute-keyword-arguments 'scheme-indent-function 1))
  (eval . (put 'with-store 'scheme-indent-function 1))
  (eval . (put 'with-external-store 'scheme-indent-function 1))
  (eval . (put 'with-error-handling 'scheme-indent-function 0))
  (eval . (put 'with-mutex 'scheme-indent-function 1))
  (eval . (put 'with-atomic-file-output 'scheme-indent-function 1))
  (eval . (put 'call-with-compressed-output-port 'scheme-indent-function 2))
  (eval . (put 'call-with-decompressed-port 'scheme-indent-function 2))
  (eval . (put 'call-with-gzip-input-port 'scheme-indent-function 1))
  (eval . (put 'call-with-gzip-output-port 'scheme-indent-function 1))
  (eval . (put 'call-with-lzip-input-port 'scheme-indent-function 1))
  (eval . (put 'call-with-lzip-output-port 'scheme-indent-function 1))
  (eval . (put 'signature-case 'scheme-indent-function 1))
  (eval . (put 'emacs-batch-eval 'scheme-indent-function 0))
  (eval . (put 'emacs-batch-edit-file 'scheme-indent-function 1))
  (eval . (put 'emacs-substitute-sexps 'scheme-indent-function 1))
  (eval . (put 'emacs-substitute-variables 'scheme-indent-function 1))
  (eval . (put 'with-derivation-narinfo 'scheme-indent-function 1))
  (eval . (put 'with-derivation-substitute 'scheme-indent-function 2))
  (eval . (put 'with-status-report 'scheme-indent-function 1))
  (eval . (put 'with-status-verbosity 'scheme-indent-function 1))
  (eval . (put 'with-build-handler 'scheme-indent-function 1))

  (eval . (put 'mlambda 'scheme-indent-function 1))
  (eval . (put 'mlambdaq 'scheme-indent-function 1))
  (eval . (put 'syntax-parameterize 'scheme-indent-function 1))
  (eval . (put 'with-monad 'scheme-indent-function 1))
  (eval . (put 'mbegin 'scheme-indent-function 1))
  (eval . (put 'mwhen 'scheme-indent-function 1))
  (eval . (put 'munless 'scheme-indent-function 1))
  (eval . (put 'mlet* 'scheme-indent-function 2))
  (eval . (put 'mlet 'scheme-indent-function 2))
  (eval . (put 'run-with-store 'scheme-indent-function 1))
  (eval . (put 'run-with-state 'scheme-indent-function 1))
  (eval . (put 'wrap-program 'scheme-indent-function 1))
  (eval . (put 'wrap-script 'scheme-indent-function 1))
  (eval . (put 'with-imported-modules 'scheme-indent-function 1))
  (eval . (put 'with-extensions 'scheme-indent-function 1))
  (eval . (put 'with-parameters 'scheme-indent-function 1))
  (eval . (put 'let-system 'scheme-indent-function 1))
  (eval . (put 'with-build-variables 'scheme-indent-function 2))

  (eval . (put 'with-database 'scheme-indent-function 2))
  (eval . (put 'call-with-database 'scheme-indent-function 1))
  (eval . (put 'call-with-transaction 'scheme-indent-function 2))
  (eval . (put 'with-statement 'scheme-indent-function 3))
  (eval . (put 'call-with-retrying-transaction 'scheme-indent-function 2))
  (eval . (put 'call-with-savepoint 'scheme-indent-function 1))
  (eval . (put 'call-with-retrying-savepoint 'scheme-indent-function 1))

  (eval . (put 'call-with-container 'scheme-indent-function 1))
  (eval . (put 'container-excursion 'scheme-indent-function 1))
  (eval . (put 'eventually 'scheme-indent-function 1))

  (eval . (put 'call-with-progress-reporter 'scheme-indent-function 1))
  (eval . (put 'with-repository 'scheme-indent-function 2))
  (eval . (put 'with-temporary-git-repository 'scheme-indent-function 2))
  (eval . (put 'with-environment-variables 'scheme-indent-function 1))
  (eval . (put 'with-fresh-gnupg-setup 'scheme-indent-function 1))

  (eval . (put 'with-paginated-output-port 'scheme-indent-function 1))

  (eval . (put 'with-shepherd-action 'scheme-indent-function 3))

  (eval . (put 'with-http-server 'scheme-indent-function 1))

  ;; This notably allows '(' in Paredit to not insert a space when the
  ;; preceding symbol is one of these.
  (eval . (modify-syntax-entry ?~ "'"))
  (eval . (modify-syntax-entry ?$ "'"))
  (eval . (modify-syntax-entry ?+ "'"))

  (eval . (progn
            (require 'lisp-mode)
            (defun emacs27-lisp-fill-paragraph (&optional justify)
              (interactive "P")
              (or (fill-comment-paragraph justify)
                  (let ((paragraph-start
                         (concat paragraph-start
                                 "\\|\\s-*\\([(;\"]\\|\\s-:\\|`(\\|#'(\\)"))
                        (paragraph-separate
                         (concat paragraph-separate "\\|\\s-*\".*[,\\.]$"))
                        (fill-column (if (and (integerp emacs-lisp-docstring-fill-column)
                                              (derived-mode-p 'emacs-lisp-mode))
                                         emacs-lisp-docstring-fill-column
                                       fill-column)))
                    (fill-paragraph justify))
                  ;; Never return nil.
                  t))
            (setq-local fill-paragraph-function #'emacs27-lisp-fill-paragraph)))))
