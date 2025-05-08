;;; Module: common
(define-module (ellipsis system common)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (gnu)
  #:use-module (gnu system)

  #:export (%el-extra-files-svc))

(define %el-extra-files-svc
  (list
   (extra-special-file "/usr/bin/env"
                       (file-append coreutils "/bin/env"))
   (extra-special-file "/lib64/ld-linux-x86-64.so.2"
                       (file-append glibc "/lib/ld-linux-x86-64.so.2"))))

(define-public %el-altgr-kbd
  (keyboard-layout "us" "altgr-intl"
                   #:model "pc105"
                   #:options '("caps:ctrl_modifier")))

(define-public (el-nonguix-chan guix-chan)
  (guix-configuration
   (inherit guix-chan)
   (substitute-urls
    (append (list "https://substitutes.nonguix.org")
            %default-substitute-urls))
   (authorized-keys
    (append
     (list
      (plain-file "nonguix.pub"
                  "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))
     %default-authorized-guix-keys))))
