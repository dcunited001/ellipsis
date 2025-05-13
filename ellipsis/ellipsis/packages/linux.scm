(define-module (ellipsis packages linux)
  #:use-module ((guix licenses) #:prefix license:)

  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)

  #:use-module (srfi srfi-1))

(use-package-modules linux ncurses ninja bison flex pkg-config python check)

(define-public menuconfig
  (let* ((commit "adb23e435283e76f17d173010bbf9d5f100b12dd")
         (revision "0"))
    (package
      (name "menuconfig")
      (version (string-append revision "-" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/anatol/menuconfig")
                      (commit commit)))
                ;; (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1lh9mnpxs17jwnd9s05dvs2gf4ib7ybzah7xsklwrm9s4hgmmxnr"))))
      ;; (build-system meson-build-system)
      (build-system gnu-build-system)
      (inputs (list ncurses bison flex))
      (native-inputs (list pkg-config ninja python python-pytest))
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)
            (replace 'build
              (lambda* (#:key parallel-build? #:allow-other-keys)
                (let ((job-count (if parallel-build?
                                     (number->string (parallel-job-count))
                                     "1")))
                  (invoke "ninja" "-j" job-count))))
            (delete 'check)
            (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (bin (string-append out "/bin")))
                  (install-file "conf" bin)
                  (install-file "mconf" bin))
                #t))
            )))
      (home-page "https://github.com/anatol/menuconfig")
      (synopsis "Out-of-tree version of Linux' kconfig tool.")
      (description "KConfig is a flexible Linux project configuration
mechanism. It allows to define and use set of configuration options and then
build a @code{.config} file that takes all the Kconfig dependencies and restrictions
into account.")
      (license license:gpl2))))
