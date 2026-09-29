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

(define-public (dc-user user-groups)
  (user-account
    (uid 1000)
    (name "dc")
    (comment "David Conner")
    (group "dc")
    (home-directory "/home/dc")
    (supplementary-groups user-groups)))

(define-public %dc-my-groups
  ;; "kmem"
  '("wheel" "users" "tty" "dialout"
    "input" "video" "audio" "netdev" "lp"
    ;; "disk" "floppy" "cdrom" "tape" "kvm"
    "fuse" "realtime" "yubikey" "plugdev"
    "libvirt" "docker" "cgroup"))

;; (define test-dc
;;   (user-account
;;     (uid 1000)
;;     (name "dc")
;;     (comment "David Conner")
;;     (group "dc")
;;     (home-directory "/home/dc")
;;     (supplementary-groups %dc-my-groups)))

;; (define test-dc2
;;   (user-account
;;     (inherit test-dc)
;;     (supplementary-groups (append '("kvm") %dc-my-groups))))
