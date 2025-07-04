;;; Copyright © 2025 David Conner <aionfork@gmail.com>

(define-module (dc home common)
  #:use-module (dc home config)
  #:use-module (gnu home services fontutils)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services guix)
  #:use-module (gnu home services mcron)
  #:use-module (gnu home services)

  #:use-module (gnu packages base)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages)

  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu)
  #:use-module (guix channels)
  #:use-module (guix gexp)

  #:use-module (ice-9 format)
  #:use-module (rde packages fonts)     ; font-iosevka-nerd
  #:use-module (rde serializers ini)
  #:use-module (srfi srfi-1)

  #:export (essential-packages
            guile-packages
            yubikey-packages
            udiskie-packages
            gtk-packages
            gtk-theme-packages
            fontconfig-packages
            fontapp-packages
            dmenu-packages
            gpg-packages
            terminator-packages
            printer-packages
            hardware-packages
            x11-packages
            dbus-packages

            gstreamer-packages
            notification-packages
            image-viewer-packages

            desktop-packages
            fcitx5-packages

            ;; environments
            wayland-environment

            ;; services
            dc-gpg-agent-configuration

            dc-channels-service))

;; =============================================
;;; Packages
(use-package-modules bash guile guile-xyz gnupg security-token
                     fonts fontutils ghostscript admin shellutils
                     linux image-viewers video gstreamer
                     suckless freedesktop libcanberra
                     xorg xdisorg fcitx5 anthy
                     kde-frameworks glib gtk gnome gnome-xyz)

(define guile-packages
  (list guile-next guile-ares-rs glibc-locales))

(define essential-packages
  (list tree                            ; admin
        ))

(define yubikey-packages
  ;; libyubikey yubico-pam pam-u2f
  (list yubikey-personalization
        yubikey-manager-qt
        python-yubikey-manager
        yubico-piv-tool
        libu2f-host))

(define udiskie-packages
  (list udiskie))                       ; freedesktop

(define gtk-packages
  ;; gsettings-desktop-schemas

  (list xsettingsd                      ; xdisorg
        dconf))                         ; gnome

(define gtk-theme-packages
  (list arc-icon-theme                  ; gnome-xyz
        matcha-theme                    ; gnome-xyz
        papirus-icon-theme              ; gnome-xyz
        hicolor-icon-theme              ; gnome
        adwaita-icon-theme              ; gnome
        gnome-backgrounds               ; gnome
        breeze-icons                    ; kde-frameworks
        yad))                           ; gtk

(define fontconfig-packages
  ;; TODO FONTS: maybe break fontconfig-packages into its own profile?
  (list font-fira-code
        font-jetbrains-mono
        font-iosevka
        font-iosevka-nerd               ; rde packages fonts
        font-iosevka-aile
        font-abattis-cantarell
        font-overpass
        font-dejavu
        font-google-noto
        font-gnu-freefont
        font-liberation
        font-ghostscript                ; ghostscript
        font-bitstream-vera

        ;; waybar icons
        font-awesome
        font-google-material-design-icons

        ;; japanese (CJK)
        font-adobe-source-han-sans
        font-wqy-zenhei

        ;; more fonts
        font-juliamono
        font-adobe-source-han-sans))

(define fontapp-packages
  (list

   ;; GUI
   gucharmap                            ; gnome (unicode picker)
   fontmanager))

(define dmenu-packages
  (list dmenu                           ; suckless
        rofi))                        ; xdisorg

(define gpg-packages
  (list pinentry-gtk2
        pinentry-qt
        gnupg))

(define terminator-packages
  ;; gnome
  (list terminator))

(define printer-packages
  ;; gnome
  (list system-config-printer))

(define hardware-packages
  (list brightnessctl))

;; trash-cli?
(define x11-packages
  ;; xorg
  (list xset
        xrdb
        xhost
        xrandr
        xinput

        ;; compton                         ; compton
        ;;"redshift"

        ;; xdisorg
        scrot
        xss-lock
        xscreensaver
        xwallpaper
        arandr
        autorandr))

(define dbus-packages
  (list xdg-dbus-proxy                  ; glib (for flatpak)
        xdg-desktop-portal))

(define gstreamer-packages
  (list gstreamer
        gst-plugins-base
        gst-plugins-good
        gst-plugins-bad
        gst-libav

        ;; testing video devices, but it lacks qv4l2, thus guvcview
        v4l-utils
        guvcview

        intel-vaapi-driver
        libva-utils))

(define notification-packages
  (list libnotify
        libcanberra
        sound-theme-freedesktop))

(define desktop-packages
  (list xdg-utils
        xdg-user-dirs
        libinput

        ;; shellutils
        trash-cli))

(define image-viewer-packages
  (list feh))

(define fcitx5-packages
  (list fcitx5
        anthy
        fcitx5-anthy
        fcitx5-configtool
        fcitx5-chinese-addons
        fcitx5-material-color-theme
        fcitx5-gtk
        fcitx5-gtk4
        fcitx5-qt))

;; #+begin_src sh :tangle bin/q-wayland :shebang #!/bin/sh
;; qdbus org.kde.KWin /KWin org.kde.KWin.showDebugConsole
;; #+end_src


;; TODO: refactor env-vars into records & services
;;
;; extend from home-environment-variables-service-type
;;
;;  see: akagi/dotfiles/magi/home/{base,scheme}.scm


;; ==[ old notes ]===========================================

;; TODO: kitnil/dotfiles/dotfiles/guixsd/modules/home/services/ansible.scm

;; =============================================
;;; Env

;; - MAIL gets interpreted as the default for mutt's spoolfile
;; - MAIL is supposed to point to system mail account https://askubuntu.com/a/474982

(define-public %dc-env-base
  `(("SHELL" . ,(file-append bash "/bin/bash"))
    ("MAIL" . "geary")
    ("LESSHISTFILE" . "$XDG_CACHE_HOME/.lesshst")
    ("_JAVA_AWT_WM_NONREPARENTING" . #t)))

;; ("LITERAL_VALUE" . ,(literal-string "${abc}"))

;; TODO: ensure XDG_CACHE_HOME gets set

;;;; Display Servers
;;;;; X11

(define-public %dc-env-x11
  ;; this seems to fix alacritty HiDPI
  '(("WINIT_X11_SCALE_FACTOR" . #t)))

;;;;; Wayland

(define-public %dc-env-wayland
  `(("XDG_SESSION_TYPE" . "wayland")
    ("QT_QPA_PLATFORM" . "wayland-egl")
    ("QT_QPA_PLATFORMTHEME" . "qt5ct")
    ("QT_WAYLAND_FORCE_DPI" . "physical")
    ("QT_WAYLAND_DISABLE_WINDOWDECORATION" . #t)))

;;;; Applications

;;;;; FCITX

;; does this even work in wayland?
(define-public fcitx-environment
  '(("QT_IM_MODULE" . "fcitx")
    ("GTK_IM_MODULE" . "fcitx")
    ("SDL_IM_MODULE" . "fcitx")
    ("XMODIFIERS" . "@im=fcitx")))

;; for guix, fix to load from ~/.guix-profile
;;
;; export FCITX_ADDON_DIRS=$GUIX_EXTRA/fcitx5/fcitx5/lib:$FCITX_ADDON_DIRS

;; =============================================
;;; Mcron
(define dc-mcron-job-bgcycle
  '(job
    '(next-minute-from
      (next-minute (range 0 1440 5)))
    "setbg $_WALLPAPERS"))

(define dc-mcron-service
  (simple-service
   'dc-mcron-service
   home-mcron-service-type
   (home-mcron-configuration
    (jobs
     ;; updates guix
     `(,dc-mcron-job-bgcycle
       (job "24 5,16 */5 * *" "$HOME/.config/guix/current/bin/guix pull"))))))

;; =============================================
;;; GPG

(define dc-gpg-agent-configuration
  (home-gpg-agent-configuration
   ;; (pinentry-program (file-append pinentry-gtk2 "/bin/pinentry-gtk-2"))
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

;; =============================================
;;; Channels
;;;
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
     (name 'nonguix)
     (url "https://gitlab.com/nonguix/nonguix")
     (introduction
      (make-channel-introduction
       "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
       (openpgp-fingerprint
        "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))))))
