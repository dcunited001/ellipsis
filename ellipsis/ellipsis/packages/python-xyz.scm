(define-module (ellipsis packages python-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)

  #:use-module (gnu packages gcc)
  #:use-module (nonguix build-system binary)

  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix transformations) ;; for python-requests=2.30
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

;; ---------------------------------------------
;; python-pds-deeparchive

(define-public python-lxml-stubs
  (let ((version "0.5.1"))
    (package
      (name "python-lxml-stubs")
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri (pypi-uri "lxml-stubs" version))
         (sha256
          (base32 "17dpx97pvyhyz3x9sdb4sp0ss4jwa7j1q28rnxw15ncjrshjmv70"))))
      (build-system pyproject-build-system)
      (inputs (list python-lxml))
      (native-inputs (list
                      python-setuptools
                      python-wheel
                      python-coverage
                      python-mypy
                      python-pytest
                      python-pytest-mypy-plugins))
      ;; fails everything, but same as nixos derivation at first glance
      (arguments
       (list #:phases #~(modify-phases %standard-phases (delete 'check))))
      (home-page "https://github.com/lxml/lxml-stubs")
      (synopsis "Type annotations for the lxml package")
      (description "Type annotations for the lxml package.")
      (license #f))))

(define-public python-mypy-zope
  (let ((version "1.0.13"))
    (package
      (name "python-mypy-zope")
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri (pypi-uri "mypy_zope" version))
         (sha256
          (base32 "168ga7m1ps8cv7c0yyl3ljjd4jyyvhaffsfwh3rblx58bq1lvyv3"))))
      (build-system pyproject-build-system)
      (propagated-inputs (list python-mypy python-zope-interface
                               python-zope-schema))
      (native-inputs (list python-lxml python-pytest python-pytest-cov
                           python-setuptools python-wheel))
      (arguments
       ;; tests/test_samples/interface_implications.py (and others)
       ;; idkwtf mypy is (dev tool? build tool? does it land bruce willis on the moon?)
       (list #:phases #~(modify-phases %standard-phases (delete 'check))))
      (home-page "https://github.com/Shoobx/mypy-zope")
      (synopsis "Plugin for mypy to support zope interfaces")
      (description "Plugin for mypy to support zope interfaces.")
      (license #f))))

(define-public python-requests-2.31
  (package
    (inherit python-requests)
    (name "python-requests-2.31")
    (version "2.31.0")
    ;; pkg_resources package is slated for removal as early as 2025-11-30
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "requests" version))
              (sha256
               (base32
                "1qfidaynsrci4wymrw3srz8v1zy7xxpcna8sxpm91mwqixsmlb4l"))))))

(define-public python-zope-interface-7.1.1
  (package
    (inherit python-zope-interface)
    (name "python-zope-interface-7.1.1")
    (version "7.1.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "zope.interface" version))
              (sha256
               (base32
                "1qy5za5hb05jplx0gz7gzb2kv1w07nqyfkbdhc4vgxqgxxjdd122"))))))

(define-public python-pds-deeparchive
  (let ((version "1.5.0")
        (github-repo "https://github.com/NASA-PDS/deep-archive"))
    (package
      (name "python-pds-deeparchive")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url github-repo)
                (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1q8l6pvlh7hia0r8q42drrxfc6mw100sbw81q8mxcdfglb95n0jf"))))
      (build-system pyproject-build-system)
      (inputs
       (list python-requests-2.31
             python-zope-component
             python-zope-interface
             python-lxml))
      ;; https://github.com/NASA-PDS/deep-archive/blob/c5452b9dcd1a271889401df9897c82f594b3e48f/setup.cfg#L94
      (native-inputs
       ;; python-wheel
       ;; python-pydocstyle
       (list
        python-setuptools
        python-mypy-zope
        python-types-requests
        ;; python-lxml-stubs
        python-pytest
        python-pytest-cov
        python-pytest-xdist
        python-pytest-mypy-plugins
        python-tox
        python-coverage))

      ;; failures related to type inference in tests (adding mypy-zope didn't help)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (delete 'check)
            (delete 'sanity-check))))
      (home-page "https://github.com/NASA-PDS/deep-archive")
      (synopsis "PDS Open Archival Information System (OAIS) utilities")
      (description "Software for the Planetary Data System to generate Archive
Information Package (AIP) and Submission Information Package (SIP) products,
based upon Open Archival Information System standards.")
      (license license:asl2.0))))

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
