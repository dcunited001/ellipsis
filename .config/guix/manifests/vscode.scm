(use-modules
 (gnu packages)
 ;; (guix packages)
 ;; (guix transformations)
 ;; ;; coreutils
 ;; (gnu packages)
 ;; gcc:lib,
 (gnu packages gcc)
 ;; patchelf
 (gnu packages elf)
 ;; glib:bin
 (gnu packages glib)
 ;; nss, nss-certs
 (gnu packages nss)
 ;; libtasn1, p11-kit
 (gnu packages tls)
 ;; e2fsprogs
 (gnu packages linux)
 ;; libsm
 (gnu packages xorg)
 ;; sqlite
 (gnu packages sqlite)
 ;; libgpg-error
 (gnu packages gnupg)
 ;; gmp
 (gnu packages multiprecision)
 (gnu packages gl)
 ;; ungoogled-chromium
 (gnu packages chromium)

 (gnu packages java)

 (guix packages))

;; (concatenate-manifests
;;  (list (packages->manifest
;;         (list p11-kit
;;               sqlite
;;               libsm
;;               libgpg-error))
;;        (package->development-manifest
;;         (list ungoogled-chromium
;;               libtasn1
;;               gmp
;;               glu
;;               glib "bin"
;;               gcc "lib"
;;               e2fsprogs))))

(concatenate-manifests
 (list (package->development-manifest
        ungoogled-chromium)
       (packages->manifest
        (list p11-kit
              sqlite
              libsm
              libgpg-error
	      patchelf
	      libtasn1
              gmp
              glu
              ;; `(,glib "bin")
              ;; `(,gcc "lib")
              (list glib "bin")
              (list gcc "lib")
              openjdk
              e2fsprogs))))

;; debug tools
;; (package->manifest (list strace ...))
;; "glibc"
;; "gdb"
;; "strace"
;; "patchelf"
