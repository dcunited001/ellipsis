(define-module (ellipsis packages engineering)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  ;; #:use-module (guix build-system ???)

  #:use-module (srfi srfi-1))

;; (define-public cura-52
;;   (package
;;     (inherit cura)
;;     (name "cura-52")
;;     (version "5.2.1")
;;     (source (origin
;;               (method git-fetch)
;;               (uri (git-reference
;;                     (url "https://github.com/Ultimaker/Cura")
;;                     (commit version)))
;;               (file-name (git-file-name name version))
;;               (sha256
;;                (base32 "0q0zz3nxf8cdrmc1n58cb9ggigngkxl709rdhi3mrl3jrg8r2bc8"))))
;;     (inputs
;;      (list cura-engine-52
;;            libcharon
;;            libsavitar
;;            python
;;            python-keyring
;;            python-pynest2d
;;            python-pyserial
;;            python-sentry-sdk
;;            python-sip
;;            uranium))
;;     (home-page "https://github.com/Ultimaker/Cura")
;;     (synopsis "Slicer for 3D Printers")
;;     (description "Cura is a slicing software from Ultimaker. A @emph{slicer} generates G-Code for 3D printers.")
;;     (license license:lgpl3+)))

;; (define-public cura-engine-52
;;   (package
;;     (inherit cura-engine)
;;     (name "cura-engine-52")
;;     (version "5.2.1")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://github.com/Ultimaker/CuraEngine")
;;              (commit version)))
;;        (file-name (git-file-name name version))
;;        (sha256
;;         (base32 "0xp2r0m5wwfsh9wdb3biqzvfqfz5jsmyw4bww93aksw0rgli07bp"))))))

;; (define-public libarcus-52
;;   (package
;;     (inherit libarcus)
;;     (name "libarcus-52")
;;     (version "5.2.0")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://github.com/Ultimaker/libArcus")
;;              (commit version)))
;;        (file-name (git-file-name name version))
;;        (sha256
;;         (base32 "19fi0y0lk45cyampczgh3kz2swai4zcgzkffp0xf5h55rxxrdpvk"))))
;;     (home-page "https://github.com/Ultimaker/libArcus")
;;     (synopsis "Communication library for Ultimaker software components")
;;     (description "This library contains C++ code and Python3 bindings for
;; creating a socket in a thread and using this socket to send and receive
;; messages based on the Protocol Buffers library.  It is designed to
;; facilitate the communication between Cura and its backend and similar code.")
;;     (license license:lgpl3+)))
