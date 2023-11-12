(define-module (ellipsis packages)
  #:use-module ((gnu packages) #:prefix gnu:)
  #:use-module (srfi srfi-1)

  #:export (%ellipsis-patch-path))

;; rde and flatwhatson approach this differently:

;; - rde sets %rde-patch-path without overriding (gnu packages) %patch-path

;; - this likely more compatible for channels sourcing from mutliple
;;   extra-guix channels (outside of guix)

;; - flatwhatson adds search helpers that can search either in one go. this
;;   can be done in either approach

(define %channel-root
  (find (lambda (path)
          (file-exists? (string-append path "/ellipsis/packages.scm")))
        %load-path))

(define %ellipsis-patch-path
  (make-parameter
   (list (string-append %channel-root "/ellipsis/packages/patches"))))
