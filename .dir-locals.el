;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

;; TODO: try org-babel-tangle-body-hook
;; - none of the org-babel-tangle-body-hooks are passed variables when run

;; ((nil . (
;;          (org-babel-tangle-comment-format-beg . "** [[%link][%source-name]]")
;;         )
;;       ))


;;          (eval . (progn
;;                    ;; NOTE: This is undefined until geiser loads
;;                    ;; (setq-local geiser-guile-load-path
;;                    ;;             (cons "/data/ecto/guix/guix" geiser-guile-load-path))
;;
;;                    ;; Also NOTE: updating geiser-guile-load-path in this way is not correct
;;                    ;; - keeping for now to document modifying loadpath in dir-locals
;;                    ;; - see https://github.com/hlissner/doom-emacs/pull/5415/commits/1b191b14575fb2bd73fe084d6c686666bd6fb4f3
;;
;;                    ;; from guix/guix
;;                    (unless (boundp 'geiser-guile-load-path)
;;                      (defvar geiser-guile-load-path '()))
;;                    (make-local-variable 'geiser-guile-load-path)
;;                    (require 'cl-lib)
;;                    (cl-pushnew "/data/ecto/guix/guix" geiser-guile-load-path
;;                                :test #'string-equal)
;;                    )
;;                )
