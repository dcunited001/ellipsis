(define-module (dc hosts users dc)
  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)
  #:use-module (guix channels)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system nss)
  #:use-module (gnu system privilege)
  #:use-module (gnu system setuid)
  #:use-module (dc system common)
  #:use-module (dc hosts common))

(define-public hosts-users-dc-foo "foo")

(define-public %dc-base-groups
  (cons* (user-group (name "realtime") (system? #t))
         (user-group (name "render") (system? #t))
         (user-group (name "plugdev") (system? #t))
         (user-group (name "yubikey") (system? #t))
         (user-group (name "fuse") (system? #t))
         (user-group (name "cgroup") (system? #t))
         ;; (user-group (name "seat") (system? #t)) ; seatd creates this
         (user-group (name "users") (id 1100))
         (user-group (name "dc") (id 1000))
         (remove (lambda (g) (equal? (user-group-name g) "users"))
                 %base-groups)))

