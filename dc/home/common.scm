(define-module (dc home common)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (guix gexp)
  #:use-module (guix channels)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services guix)
  #:use-module (gnu home services fontutils)

  #:export (udiskie-packages
            gtk-packages
            gtk-theme-packages
            fontconfig-packages
            gpg-packages
            dmenu-packages
            terminator-packages
            printer-packages
            x11-packages
            fcitx5-packages

            ;; environments
            gtk-environment
            wayland-environment
            wayland-kde-environment

            dc-channels-service
            ))


(define udiskie-packages
  (list "udiskie"))

(define gtk-packages
  ;; gsettings-desktop-schemas

  (list "xsettingsd"
        "dconf"))

(define gtk-theme-packages
  (list "arc-icon-theme"
        "matcha-theme"
        "hicolor-icon-theme"
        "gnome-icon-theme"
        "gnome-backgrounds"
        "papirus-icon-theme"
        "breeze-icons"
        "yad"))

(define fontconfig-packages
  ;; TODO FONTS: maybe break fontconfig-packages into its own profile?
  (list "font-fira-code"
        "font-jetbrains-mono"
        "font-iosevka"
        "font-iosevka-nerd"
        "font-iosevka-aile"
        ;; "font-abattis-cantarell"
        "font-overpass"
        "font-dejavu"
        "font-google-noto"
        "font-gnu-freefont"
        "font-liberation"
        "font-awesome"
        "font-google-material-design-icons"
        "font-ghostscript"
        "font-bitstream-vera"

        ;; japanese (CJK)
        "font-adobe-source-han-sans"
        "font-wqy-zenhei"

        ;; more fonts
        "font-juliamono"
        "font-adobe-source-han-sans"))

(define dmenu-packages
  (list "dmenu"
        "rofi"))

(define gpg-packages
  (list "pinentry-gtk2"
        "gnupg"))

(define terminator-packages
  (list "terminator"))

(define printer-packages
  (list "system-config-printer"))

(define x11-packages
  ;; trash-cli?
  (list "xset"
        "xrdb"
        "xhost"
        "xss-lock"
        "xscreensaver"
        "xrandr"
        "arandr"
        "autorandr"))

(define desktop-packages
  (list "xdg-utils"
        "xdg-user-dirs"
        "libinput"))

(define fcitx5-packages
  (list "fcitx5"
        "anthy"
        "fcitx5-anthy"
        "fcitx5-configtool"
        "fcitx5-chinese-addons"
        "fcitx5-material-color-theme"
        "fcitx5-gtk"
        "fcitx5-gtk4"
        "fcitx5-qt"))

(define wayland-environment
  ;; on guix system, agreety will set XDG_SESSION_TYPE
  '(("XDG_SESSION_TYPE" . "wayland")
    ("QT_QPA_PLATFORM" . "wayland-egl")))

(define gtk-environment
  '(("GTK2_RC_FILES" . "$HOME/.gtkrc-2.0")))

(define wayland-kde-environment
  ;; necessary for sway
  `(("XDG_CURRENT_DESKTOP" . "KDE")

    ;; potential necessary for styling/theming
    ("QT_QPA_PLATFORMTHEME" . "qt5ct")
    ("QT_WAYLAND_FORCE_DPI" . "physical")
    ("QT_WAYLAND_DISABLE_WINDOWDECORATION" . "1")

    ;; necessary on a per-app basis or for the entire wm session,
    ("SDL_VIDEODRIVER" . "wayland")
    ("SDL_IM_MODULE" . "fcitx")))



;; TODO: refactor env-vars into records & services
;;
;; extend from home-environment-variables-service-type
;;
;;  see: akagi/dotfiles/magi/home/{base,scheme}.scm


;; ==[ old notes ]===========================================

;; TODO: home-files-service-type
;; files to copy into /gnu/store

;; TODO: home-xdg-configuration-files-service-type
;; files to generate for /gnu/store

;; TODO: home-symlink-manager-service-type

;; TODO: kitnil/dotfiles/dotfiles/guixsd/modules/home/services/ansible.scm

;; TODO: services that extend home-mcron-service-type?

;;* Env
;;** Universal

(define-public %dc-env-universal
  `(("SHELL" . ,(file-append bash "/bin/bash"))
    ("LESSHISTFILE" . "$XDG_CACHE_HOME/.lesshst")
    ("_JAVA_AWT_WM_NONREPARENTING" . #t)
    ;; ("LITERAL_VALUE" . ,(literal-string "${abc}"))
    ))

;;** Applications

(define-public %dc-env-applications
  '(("MAIL" . "geary")
    ("BROWSER" . "firefox")
    ("VISUAL" . "emacsclient -c")
    ("EDITOR" . "emacsclient")
    ("ALTERNATE_EDITOR" . "vim")))

;;** Display Servers
;;*** X11

(define-public %dc-env-x11
  ;; this seems to fix alacritty HiDPI
  '(("WINIT_X11_SCALE_FACTOR=1")))

;;*** Wayland

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
;;** Toolkits

;;*** GTK

(define-public %dc-env-gtk
  '(("GTK2_RC_FILES" . "$HOME/.gtkrc-2.0")))

;;*** QT

;;** Window Managers

;;*** i3

(define-public %dc-env-i3
  '(("XDG_CURRENT_DESKTOP=i3")))

;;*** Sway

;; override with specifics
(define-public %dc-env-sway
  '(("XDG_CURRENT_DESKTOP=sway")))

;;*** KDE

(define-public %dc-env-kde
  '(("XDG_CURRENT_DESKTOP=KDE")))

;;** Applications

;;*** FCITX

;; does this even work in wayland?
(define-public fcitx-environment
  '(("QT_IM_MODULE" . "fcitx")
    ("GTK_IM_MODULE" . "fcitx")
    ("SDL_IM_MODULE" . "fcitx")
    ("XMODIFIERS" . "@im=fcitx")))

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

;; services that do more than one thing need to be defined privately as service-type
;;  - in order to bundle together extensions of different types


(define dc-gpg-agent-configruation
  (home-gpg-agent-configuration
   (pinentry-program (file-append pinentry-gtk2 "/bin/pinentry-gtk-2"))
   (ssh-support? #t)
   (default-cache-ttl 60)
   (default-cache-ttl-ssh 60)
   (max-cache-ttl 600)
   (max-cache-ttl-ssh 600)
   (extra-content "
no-allow-external-cache
no-allow-mark-trusted
no-allow-emacs-pinentry
no-allow-loopback-pinentry")))

(define dc-channels-service
  (simple-service
   'dc-channels
   home-channels-service-type
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
        "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))))))
