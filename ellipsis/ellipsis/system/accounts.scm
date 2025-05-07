;;; Module: common
(define-module (ellipsis system accounts)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system shadow)

  #:export (%ellipsis-user-groups))

;; most of the common user groups, except "users" and the users' own group

(define %ellipsis-user-groups
  (cons* (user-group (name "realtime") (system? #t))
         ;; (user-group (system? #t) (name "docker"))
         (user-group (name "render") (system? #t))
         (user-group (name "yubikey") (system? #t))
         (user-group (name "fuse") (system? #t))
         (user-group (name "cgroup") (system? #t))
         (user-group (name "seat") (system? #t))
         (remove (lambda (g) (equal? (user-group-name g) "users"))
                 %base-groups)))
