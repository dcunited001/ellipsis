(define-module (ellipsis utils)
  #:use-module (guix gexp)
  #:use-module (json)
  #:use-module (srfi srfi-1))

;; TODO: look into #:replace

(define-public (read-json-config file)
  (call-with-input-file file
    json->scm))

(define-public (alist-append-uniq . rest)
  (fold
   (lambda (el li)
     (assoc-set! li (car el) (cdr el)))
   '() (apply append rest)))
