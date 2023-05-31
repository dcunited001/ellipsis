(define-module (dc home common)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services guix)
  #:use-module (gnu home services fontutils))

;; TODO: home-files-service-type
;; files to copy into /gnu/store

;; TODO: home-xdg-configuration-files-service-type
;; files to generate for /gnu/store

;; TODO: home-symlink-manager-service-type

;; TODO: kitnil/dotfiles/dotfiles/guixsd/modules/home/services/ansible.scm

;; TODO: services that extend home-mcron-service-type?

(define-public %dc-env-universal
  `(("SHELL" . ,(file-append zsh "/bin/bash"))
    ("LESSHISTFILE" . "$XDG_CACHE_HOME/.lesshst")
    ("_JAVA_AWT_WM_NONREPARENTING" . #t)
    ;; ("LITERAL_VALUE" . ,(literal-string "${abc}"))
    ))

(define-public %dc-env-applications
  '(("MAIL" . "geary")
    ("BROWSER" . "firefox")
    ("VISUAL" . "emacsclient -c")
    ("EDITOR" . "emacsclient")
    ("ALTERNATE_EDITOR" . "vim")))

(define-public %dc-env-gtk
  '(("GTK2_RC_FILES" . "$HOME/.gtkrc-2.0")))

(define-public %dc-env-x11
  ;; this seems to fix alacritty HiDPI
  '(("WINIT_X11_SCALE_FACTOR=1")))

(define-public %dc-env-i3
  '(("XDG_CURRENT_DESKTOP=i3")))

;; override with specifics
(define-public %dc-env-sway
  '(("XDG_CURRENT_DESKTOP=sway")))

(define-public %dc-env-kde
  '(("XDG_CURRENT_DESKTOP=KDE")))

;; does this even work in wayland?
(define-public fcitx-environment
  '(("QT_IM_MODULE" . "fcitx")
    ("GTK_IM_MODULE" . "fcitx")
    ("SDL_IM_MODULE" . "fcitx")
    ("XMODIFIERS" . "@im=fcitx")))

(define-public %dc-env-wayland
  `( ;; necessary for wayland
    ("XDG_SESSION_TYPE" . "wayland")
    ("QT_QPA_PLATFORM" . "wayland-egl")
    ;; potential necessary for styling/theming
    ("QT_QPA_PLATFORMTHEME" . "qt5ct")
    ("QT_WAYLAND_FORCE_DPI" . "physical")
    ("QT_WAYLAND_DISABLE_WINDOWDECORATION" . "1")
    ;; firefox in wayland
    ("MOZ_ENABLE_WAYLAND" . "1")
    ;; fix for firefox (already running but not responding)
    ;; ("MOZ_DBUS_REMOTE" . "1")

    ;; disables accessibility??
    ;; http://library.gnome.org/devel/accessibility-devel-guide/stable/gad-how-it-works.html.en
    ;; ("NO_AT_BRIDGE" . "1")

    ;; this can prevent programs from starting (e.g. chromium and electron
    ;; apps).  therefore, this should be set per app instead of globally.
    ("GDK_BACKEND" . "wayland")
    ;; this can prevent programs from starting old sdl games. therefore, this
    ;; should be set per app instead of globally.

    ;; ("SDL_VIDEODRIVER" . "wayland") ; decide on per-app

    ;; ("SDL_DYNAMIC_API" . "/usr/lib/libSDL2-2.0.so") ; steam tweaks
    ))

;; (define* (dc-env-universal #:optional
;;          (env-vars %env-universal))
;;
;; doesn't make sense. ensure load order, override with subsequent servic
(define-public dc-env-universal-service
  (simple-service 'dc-env-universal
                  home-environment-variables-service-type
                  %dc-env-universal))

(define-public dc-nix-fontconfig-service
  (simple-service
   'nix-fontconfig
   home-fontconfig-service-type
   (list "~/.nix-profile/share/fonts"
         '(alias
           (family "monospace")
           (prefer
            (family "Liberation Mono"))))))

(define-public dc-channels-service
  (simple-service
   'dc-channels
   home-channels-service-type
   (list
    (list
     (channel
      (name 'rde)
      (url "https://git.sr.ht/~abcdw/rde")
      (introduction
       (make-channel-introduction
        "257cebd587b66e4d865b3537a9a88cccd7107c95"
        (openpgp-fingerprint
         "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
     (channel
      (name 'guixrus)
      (url "https://git.sr.ht/~whereiseveryone/guixrus")
      (introduction
       (make-channel-introduction
        "7c67c3a9f299517bfc4ce8235628657898dd26b2"
        (openpgp-fingerprint
         "CD2D 5EAA A98C CB37 DA91  D6B0 5F58 1664 7F8B E551"))))
     (channel
      (name 'nonguix)
      (url "https://gitlab.com/nonguix/nonguix")
      (introduction
       (make-channel-introduction
        "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
        (openpgp-fingerprint
         "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))))))
