(define-module (ellipsis packages freedesktop)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system meson)

  #:use-module (gnu packages)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages ninja)
  ;; #:use-module (ice-9 pretty-print)

  #:use-module (ellipsis services wm)

  #:use-module (srfi srfi-1))


;; NOTE: the hyprland packages aren't building.
;; undefined reference to `drmGetDeviceFromDevId'
;; adding egl-wayland doesn't fix this (shot in the dark)

;; lib,
;; stdenv,
;; makeWrapper,
;; meson,
;; ninja,
;; pkg-config,
;; wayland-protocols,
;; wayland-scanner,
;; hyprland-share-picker,
;; grim,
;; slurp,
;; hyprland-protocols,
;; inih,
;; libdrm,
;; libuuid,
;; mesa,
;; pipewire,
;; systemd,
;; wayland,
;; version ? "git",

;; nativeBuildInputs = [
;;   meson
;;   ninja
;;   pkg-config
;;   wayland-scanner
;;   makeWrapper
;; ];
;; buildInputs = [
;;   hyprland-protocols
;;   inih
;;   libdrm
;;   libuuid
;;   mesa
;;   pipewire
;;   systemd
;;   wayland
;;   wayland-protocols
;; ];

;; meson-build-system: extends from gnu-build-system

;; gnu build phases:
;; configure
;; build
;; check

;; meson run phases:
;; fix-runpath
;; glib-or-gtk-wrap
;; glib-or-gtk-compile-schemas

(define-public xdg-desktop-portal-hyprland
  (package
    (name "xdg-desktop-portal-hyprland")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/hyprwm/xdg-desktop-portal-hyprland/archive/refs/tags/v"
                    version "/v" version ".tar.gz"))
              (sha256
               (base32
                "1cvga9z261kybpzrw57jagrgxhsnln8pm63n2b8flk7r6x7lkb7c"))))
    (build-system meson-build-system)
    ;; (propagated-inputs (list ))
    (inputs (list pipewire
                  libdrm
                  `(,util-linux "lib")
                  ;; elogind
                  basu
                  libinih
                  wayland))
    (native-inputs (list pkg-config
                         cmake
                         ninja
                         ;; TODO undefined wayland-protocols-next
                         hyprland-protocols
                         mesa))

    (home-page "https://github.com/hyprwm/xdg-desktop-portal-hyprland")
    (synopsis "xdg-desktop-portal backend for Hyprland")
    (description "")
    (license license:bsd-3)))

;; https://github.com/hyprwm/xdg-desktop-portal-hyprland/releases/download/v0.3.1/v0.3.1.tar.gz
