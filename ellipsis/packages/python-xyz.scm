(define-module (ellipsis packages python-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-check)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

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
