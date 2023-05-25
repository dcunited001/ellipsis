(define-module (ellipsis packages wayland-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system meson)

  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages image)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages freedesktop)
  ;; #:use-module (gnu packages xorg) ;; egl-wayland
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages crates-graphics)
  ;; #:use-module (gnu packages base)

  #:use-module (srfi srfi-1))

;; git meson ninja-build pkg-config
;; libpng,libfreetype,
;; libgl,libglew,libegl,libwegl,gles2
;; xkbcommon,wayland-client,wayland-protocols
;; fonts-terminus

;; meson says egl-wayland is runtime dep, has these deps:
;; externalplatform@1.1 libglvnd@1.5.0
;; mesa-headers@23.0.3 mesa@23.0.3
;; pkg-config@0.29.2 wayland-protocols@1.29
;; wayland@1.22.0

(define-public sov
  (let ((commit "750e3e6dbffc515441ae804dc8768b2634de1a61"))
    (package
      (name "sov")
      (version "0.92b")
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://github.com/milgra/sov/releases/download/"
                             version "/sov-" version ".tar.xz"))
         (sha256
          (base32
           "02s6915l09wpw5brndxvf6vhvdjc3gf9fc2wcyaxg8gx648p2v4l")))
       ;; (origin
       ;;   (method git-fetch)
       ;;   (uri (git-reference
       ;;         (url "https://github.com/milgra/sov")
       ;;         (commit  "750e3e6dbffc515441ae804dc8768b2634de1a61")))
       ;;   (file-name (git-file-name name version))
       ;;   (sha256
       ;;    (base32
       ;;     "195gn14qn22aqclwz2i9w2qq9bxabwcasfkna1fj41jwv2bpbykc")))
       )
      ;; glew: 0.92w  no longer depends on glew
      ;; fonts-terminus not in debian/arch deps
      (inputs (list libxkbcommon freetype libpng font-terminus mesa))
      ;; mesa-utils
      (native-inputs (list pkg-config wayland wayland-protocols mesa-utils cmake))
      (build-system meson-build-system)
      (home-page "https://github.com/milgra/sov")
      (synopsis "An overlay that shows schemas for all workspaces to make navigation in sway easier")
      (description "Sway overview draws a schematic layout of all your workspaces on each
output. It contains all windows, window titles and window contents. The common
usage of Sway overview is to bound its appereance to the desktop switcher
button with a little latency. Sway overview can be structured via html, styled
via css.")
      (license license:gpl3))))
