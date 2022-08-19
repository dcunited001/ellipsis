(define-module (ellipsis packages security-token)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build utils)

  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix licenses)

  #:use-module (gnu packages security-token)

  #:use-module (srfi srfi-1))

;; TODO: confirm that udev processes yubikey events
;; - and that can be used with opensc
;; - these files are in /run/current-system/profile/lib/udev/rules.d
;; - not found with the links in /etc/udev/rules.d!

;; (define-public libfido2-udev
;;   (package
;;     (name "libfido2-udev")
;;     (version "1.11.0")
;;     (source (origin
;;               (method git-fetch)
;;               (uri (git-reference
;;                     (url "https://github.com/Yubico/libfido2")
;;                     (commit version)))
;;               (file-name (git-file-name name version))
;;               (sha256
;;                (base32
;;                 "1234"))))
;;     (build-system trivial-build-system)
;;     (native-inputs `(("source" ,source)))
;;     (arguments
;;      '(#:modules ((guix build utils))
;;        #:builder
;;        (begin
;;          (use-modules (guix build utils))
;;          (let ((source (assoc-ref %build-inputs "source")))
;;            (install-file (string-append))
;;            ;;; TODO: here
;;            ))))
;;     (home-page "https://github.com/Yubico/libfido2")
;;     (synopsis "Provides udev rules for FIDO2 via yubikey devices")
;;     (description "'nuff said")
;;     (license license:bsd-2)))



;; (define-public android-udev-rules
;;   (package
;;     (name "android-udev-rules")
;;     (version "20210501")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://github.com/M0Rf30/android-udev-rules")
;;              (commit version)))
;;        (file-name (git-file-name name version))
;;        (sha256
;;         (base32 "0pl1wfd7k9vz8mvy2jb2icc5f11c5p07aixpyhjs6gi5cyaywm5f"))))
;;     (build-system trivial-build-system)
;;     (native-inputs `(("source" ,source)))
;;     (arguments
;;      '(#:modules ((guix build utils))
;;        #:builder
;;        (begin
;;          (use-modules (guix build utils))
;;          (let ((source (assoc-ref %build-inputs "source")))
;;            (install-file (string-append source "/51-android.rules")
;;                          (string-append %output "/lib/udev/rules.d"))))))
;;     (home-page "https://github.com/M0Rf30/android-udev-rules")
;;     (synopsis "udev rules for Android devices")
;;     (description "Provides a set of udev rules to allow using Android devices
;; with tools such as @command{adb} and @command{fastboot} without root
;; privileges.  This package is intended to be added as a rule to the
;; @code{udev-service-type} in your @code{operating-system} configuration.
;; Additionally, an @code{adbusers} group must be defined and your user added to
;; it.

;; @emph{Simply installing this package will not have any effect.}  It is meant
;; to be passed to the @code{udev} service.")
;;     (license license:gpl3+)))
