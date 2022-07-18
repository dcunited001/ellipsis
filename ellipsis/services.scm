(define-module (ellipsis services)
  #:use-module (guix gexp)

  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)

  #:use-module (ice-9 pretty-print)

  #:export (foo)
  )

(define foo "foo")
