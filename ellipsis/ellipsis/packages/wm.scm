(define-module (ellipsis packages wm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system meson)

  #:use-module (gnu packages base)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages freedesktop)

  #:use-module (ellipsis packages freedesktop)

  ;; #:use-module (ice-9 pretty-print)

  #:use-module (srfi srfi-1))



;; NOTE: the hyprland packages aren't building.
;; undefined reference to `drmGetDeviceFromDevId'

;; hyprland
;; https://github.com/hyprwm/Hyprland/blob/main/flake.nix
;;
;; inputs:
;; - wlroots
;; - hyprland-protocols
;; - xdg-desktop-portal-hyprland

(define-public hyprland
  (package
    (name "hyprland")
    (version "0.25.0")
    ;; (source (origin (...)))
    ;; https://github.com/hyprwm/Hyprland/releases/download/v0.25.0/source-v0.25.0-2.tar.gz
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/hyprwm/Hyprland/releases/download/v"
                    version "/source-"
                    "v" version ".tar.gz"))
              (sha256
               (base32
                "0sxyzzaw6x1dhv7q3nspi662qq7d1g2a6vli38qmg116d5s2jhqd"))))
    (build-system meson-build-system)
    (inputs (list hyprland-protocols
                  xdg-desktop-portal-hyprland
                  wlroots))

    (home-page "")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

;; hyprland-protocols
;; https://github.com/hyprwm/hyprland-protocols/blob/main/flake.nix
;;
;; inputs:

;; nativeBuildInputs = [meson ninja];

(define-public hyprland-protocols
  (package
    (name "hyprland-protocols")
    (version "0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/hyprwm/hyprland-protocols/archive/refs/tags/"
                    "v" version ".tar.gz"))
              (sha256
               (base32
                "1w05g9f4w56z0jwnvcmjs564qbqp71ra24hsw47frr7vs24v2v0h"))))
    ;; (source (origin (...)))
    ;; https://github.com/hyprwm/hyprland-protocols/archive/refs/tags/v0.2.tar.gz
    (build-system meson-build-system)

    (home-page "https://github.com/hyprwm/hyprland-protocols")
    (synopsis "Wayland protocol extensions for Hyprland")
    (description "")
    (license license:bsd-3)))

