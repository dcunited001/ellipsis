(define-module (ellipsis packages python-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages imagemagick)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)

  #:use-module (gnu packages gcc)
  #:use-module (nonguix build-system binary)

  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

;; from: https://github.com/engstrand-config/farg/tree/main/farg/packages.scm
;; (define-public python-pywal-imagemagick
;;   (package
;;     (inherit python-pywal)
;;     (name "python-pywal-imagemagick")
;;     (input '())
;;     (propagated-inputs (list imagemagick))))

;; TODO: missing tests directory, convert from pypi url to clone from git

;; Using pytest
;; ============================= test session starts ==============================
;; platform linux -- Python 3.10.7, pytest-7.1.3, pluggy-1.0.0 -- /gnu/store/375350pi1l1izgnx6dnsqmg4xjyprx8q-python-wrapper-3.10.7/bin/python
;; cachedir: .pytest_cache
;; hypothesis profile 'default' -> database=DirectoryBasedExampleDatabase('/tmp/guix-build-python-convcolors-2.2.0.drv-0/convcolors-2.2.0/.hypothesis/examples')
;; rootdir: /tmp/guix-build-python-convcolors-2.2.0.drv-0/convcolors-2.2.0, configfile: setup.cfg, testpaths: tests
;; plugins: hypothesis-6.54.5
;; collecting ... ERROR: file or directory not found: tests

;; collected 0 items

;; ============================ no tests ran in 0.59s =============================
;; error: in phase 'check': uncaught exception:
;; %exception #<&invoke-error program: "/gnu/store/m8li9l31vqfl7f3m4zmdqykc5madv2hr-python-pytest-7.1.3/bin/pytest" arguments: ("-vv") exit-status: 4 term-signal: #f stop-signal: #f>
;; phase `check' failed after 1.1 seconds
;; command "/gnu/store/m8li9l31vqfl7f3m4zmdqykc5madv2hr-python-pytest-7.1.3/bin/pytest" "-vv" failed with status 4

(define-public python-pytricia
  (package
    (name "python-pytricia")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytricia" version))
       (sha256
        (base32 "0gw5r3w97zm7gh1cr6dbkb90kqc14hm59r0g5yf4q3g115lksfhw"))))
    (build-system python-build-system)
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/jsommers/pytricia")
    (synopsis "An efficient IP address storage and lookup module for Python.")
    (description
     "An efficient IP address storage and lookup module for Python.")
    (license license:lgpl3)))

(define-public python-convcolors
  (package
    (name "python-convcolors")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "convcolors" version))
       (sha256
        (base32 "1gf1hyi5jqrnhmf82s7pybfgh4b7kf3wnjm5qjx2lxdfk2788dj0"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pytest python-tox python-yapf))
    (home-page "https://github.com/CairX/convert-colors-py")
    (synopsis "Convert colors between different color spaces.")
    (description "Convert colors between different color spaces.")
    (license license:expat)))

(define-public python-extcolors
  (package
    (name "python-extcolors")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "extcolors" version))
       (sha256
        (base32 "09sggji31ps2mm7klqvm9f7mwm8xgmkxwpbsq0vblmc5z5bi9s0l"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-convcolors python-pillow))
    (native-inputs (list python-pytest python-tox python-yapf))
    (home-page "https://github.com/CairX/extract-colors-py")
    (synopsis
     "Extract colors from an image. Colors are grouped based on visual similarities using the CIE76 formula.")
    (description
     "Extract colors from an image.  Colors are grouped based on visual similarities
using the CIE76 formula.")
    (license license:expat)))

(define-public uv-bin
  (let* ((bin-platform "x86_64-unknown-linux-gnu")
         (bin-version "0.5.24")
         (bin-name (string-append "uv-" bin-platform)))

    ;; bin-name: k0s-v1.28.4+k0s.0-amd64
    ;; url: v1.28.4+k0s.0/k0s-v1.28.4+k0s.0-amd64

    (package
      (name "uv-bin")
      (version bin-version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/astral-sh/uv/releases/download/"
                      version "/" bin-name ".tar.gz"))
                (sha256
                 (base32
                  "1qh079kbvc9qms6j3i6kw2w1snc7y7qijj0wxwa6x2n3gx7n3sx0"))))

      (build-system binary-build-system)
      (arguments
       (list

        #:patchelf-plan #~`(("uv" ("gcc" "libc"))
                            ("uvx" ("gcc" "libc")))
        #:install-plan ''(("." "bin/"
                           #:include-regexp ("uv.*$")))))

      (inputs `((,gcc "lib")))
      (propagated-inputs '())
      (home-page "https://github.com/astral-sh/uv")
      (synopsis "In which python installs things")
      (description "... TODO this was never going to work. UV needs to
understand how to build with the correct paths and things.")
      (license license:asl2.0))))
