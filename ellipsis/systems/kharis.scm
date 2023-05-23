;;* Module: kharis
(define-module (ellipsis systems kharis)
  #:use-module (ellipsis systems base)
  #:use-module (srfi srfi-1)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system nss)
  #:use-module (gnu system setuid)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd))

(use-service-modules guix admin sysctl pm nix
                     avahi dbus cups desktop linux
                     mcron networking xorg ssh
                     docker audio virtualization)

(use-package-modules nfs certs shells ssh tls gnupg security-token
                     bash emacs emacs-xyz gnome networking libusb
                     fonts cups audio xorg xdisorg linux file-systems
                     version-control package-management freedesktop
                     cryptsetup hardware guile vim)

(define %host-name "kharis")

;; system-specific users should go here
(define-public %dc-users '())

(define-public %kharis-default-shell-keyboard
  (keyboard-layout "us" "altgr-intl"
                   #:model "pc105"
		   ;; see gitlab.freedesktop.org/xkeyboard-config/xkeyboard-config/-/issue/344
                   #:options '("caps:ctrl_modifier"
                               ;; "ctrl:swapcaps_hyper" ; in 1.3.0 (hyper as Mod3)
                               ;; "ctrl:hyper_capscontrol" ; in 1.5.0 (hyper as Mod4)
                               "lv3:ralt_alt"
                               "lv3:menu_switch")))

(define system
  (operating-system
    (host-name %host-name)
    (timezone "America/New_York")
    (locale "en_US.UTF-8")

    (kernel linux)
    (firmware (cons* linux-firmware
                     amd-microcode
                     ;; realtek-firmware
                     %base-firmware))

    ;; (kernel-loadable-modules (wacom))
    (kernel-loadable-modules (list v4l2loopback-linux-module))

    (keyboard-layout %kharis-default-shell-keyboard)

    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (targets '("/boot/efi"))
                 (keyboard-layout keyboard-layout)))

    ;; TODO: check whether I need to add my own user
    ;; - not showing up in /etc/group on kharis
    (groups %dc-base-groups)
    (users (append (list (dc-user (cons* "libvirt"
                                         "docker"
                                         %dc-my-groups)))
                   %dc-users
                   %base-user-accounts))

    ;; kharis would only need xf86-video-amdgpu for xorg
    ;; - otherwise the driver is included in the kernel
    ;; https://www.reddit.com/r/linux_gaming/comments/105sy93/comment/j3z2oct/?utm_source=share&utm_medium=web2x&context=3
    (packages %dc-desktop-packages)

    (services
     (append
      (modify-services %base-services
        ;; (delete console-font-service-type)
        (delete login-service-type)
        (delete mingetty-service-type))
      (list
       (service
        greetd-service-type
        (greetd-configuration
         (greeter-supplementary-groups (list "video" "input"))
         (terminals
          (list
           ;; TTY1 is the graphical login screen for Sway
           (greetd-terminal-configuration
            (terminal-vt "1")
            (terminal-switch #t)
            (default-session-command
              (greetd-wlgreet-sway-session
               ;; TODO background
               (sway-configuration
                (plain-file "sway-greet.conf" (string-append
                                               "output * bg /data/xdg/Wallpapers/" %host-name "-greetd.jpg fill\n"))))))

           ;; Set up remaining TTYs for terminal use
           (greetd-terminal-configuration (terminal-vt "2"))
           (greetd-terminal-configuration (terminal-vt "3"))
           (greetd-terminal-configuration (terminal-vt "4"))
           (greetd-terminal-configuration (terminal-vt "5"))
           (greetd-terminal-configuration (terminal-vt "6"))))))

       (simple-service
        'add-nonguix-substitutes
        guix-service-type
        (guix-extension
         (substitute-urls
          (append (list "https://substitutes.nonguix.org")
                  %default-substitute-urls))
         (authorized-keys
          (append (list (plain-file "nonguix.pub" "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))
                  %default-authorized-guix-keys))))

       polkit-wheel-service
       (simple-service
        'mount-setuid-helpers
        setuid-program-service-type
        (map (lambda (program)
               (setuid-program
                (program program)))
             (list (file-append nfs-utils "/sbin/mount.nfs")
                   (file-append ntfs-3g "/sbin/mount.ntfs-3g")
                   (file-append
                    (specification->package "swaylock")
                    "/bin/swaylock"))))

       (service ntp-service-type
                (ntp-configuration
                 (servers
                  (list (ntp-server
                         (type 'pool)
                         (address "1.us.pool.ntp.org")
                         (options '("iburst")))
                        (ntp-server
                         (type 'pool)
                         (address "2.us.pool.ntp.org")
                         (options '("iburst")))
                        (ntp-server
                         (type 'pool)
                         (address "3.us.pool.ntp.org")
                         (options '("iburst")))))))

       (service network-manager-service-type
                (network-manager-configuration
                 (vpn-plugins
                  (list network-manager-openvpn))))

       (service wpa-supplicant-service-type)
       (service modem-manager-service-type)
       (service bluetooth-service-type
                (bluetooth-configuration
                 (auto-enable? #t)))
       (service usb-modeswitch-service-type)

       (service avahi-service-type)
       (service udisks-service-type)
       (service upower-service-type)
       (service geoclue-service-type)
       (service polkit-service-type)
       (service elogind-service-type
                (elogind-configuration
                 (handle-lid-switch-external-power 'suspend)))
       (service dbus-root-service-type)

       ;; Manage the fontconfig cache)
       fontconfig-file-system-service

       (service thermald-service-type)
       (service tlp-service-type
                (tlp-configuration
                 (cpu-boost-on-ac? #t)
                 (tlp-default-mode "AC") ;; this is the default
                 (sound-power-save-on-bat 0)
                 (nmi-watchdog? #t)
                 (cpu-scaling-min-freq-on-bat 1700000)
                 (cpu-scaling-max-freq-on-bat 2100000)
                 (cpu-scaling-min-freq-on-ac 2100000)
                 (cpu-scaling-max-freq-on-ac 2100000)
                 (wifi-pwr-on-bat? #t)))

       ;; rasdaemon-service/type, rasdaemon-configuration
       ;; - helps anticipate hardware failures by scanning events, analysis appended to syslog
       ;; - with record? t, also structure logs as sqlite database: /var/lib/rasdaemon/ras-mc_event.db
       (service rasdaemon-service-type
                (rasdaemon-configuration (record? #t)))

       ;; for GNUS
       (simple-service 'nntp-config etc-service-type
                       (list `("nntpserver"
                               ,%dc-nntpserver)))

       (service
        openssh-service-type
        (openssh-configuration
         (openssh openssh-sans-x)
         (port-number (string->number
                       (or (getenv "_OPENSSH_PORT") "22")))
         (password-authentication? #f)
         (allow-agent-forwarding? #f)
         (allow-tcp-forwarding? #t)
         (accepted-environment '("COLORTERM"))
         (authorized-keys
          `(("dc" ,(local-file ".ssh/dc.pub"))))))

       ;; for yubikey
       (service pcscd-service-type)
       (udev-rules-service 'fido2 libfido2 #:groups '("plugdev"))
       (udev-rules-service 'u2f libu2f-host #:groups '("plugdev"))
       (udev-rules-service 'yubikey yubikey-personalization)

       ;; to have mouse at console (more annoying than anything tbh)
       (service gpm-service-type
                (gpm-configuration
                 ;; defaults, should work for IBM trackpoints
                 (options '("-m" "/dev/input/mice" "-t" "ps2"))))

       (service docker-service-type
                (docker-configuration
                 (enable-proxy? #f)))

       (service libvirt-service-type
                (libvirt-configuration
                 (unix-sock-group "libvirt")
                 (tls-port "16555")))

       (service virtlog-service-type
                (virtlog-configuration
                 ;; (max-clients 1024) ;; default
                 (max-size (* 32 (expt 1024 2)))))

       ;; scanning
       (service sane-service-type)

       ;; printing
       (service cups-pk-helper-service-type)
       (service cups-service-type
                (cups-configuration
                 (web-interface? #t)
                 ;; TODO ssl-options? TLS 1.0+
                 (extensions
                  (list cups-filters
                        epson-inkjet-printer-escpr
                        hplip-minimal))))

       ;; to enable JACK to enter realtime mode
       (pam-limits-service
        (list
         (pam-limits-entry "@realtime" 'both 'rtprio 99)
         (pam-limits-entry "@realtime" 'both 'nice -19)
         (pam-limits-entry "@realtime" 'both 'memlock 'unlimited)))

       ;; Add udev rules for a few packages
       (udev-rules-service 'pipewire-add-udev-rules pipewire)
       (udev-rules-service 'brightnessctl-udev-rules brightnessctl)

       (service
        unattended-upgrade-service-type
        (unattended-upgrade-configuration
         (schedule "30 2 * * 0")
         (channels #~(list
                      (channel
                       (name 'nonguix)
                       (url "https://gitlab.com/nonguix/nonguix")
                       (branch "master"))
                      %default-channels))
         ;; TODO is this running?
         (system-expiration (* 6 7 24 3600))
         (operating-system-file
          (file-append
           (local-file "." "systems-dir" #:recursive? #t)
           (string-append
            "/root/.config/guix/systems/" %host-name ".scm")))))

       )))

    (mapped-devices
     (list (mapped-device
            (source (uuid "c6684f7e-a5e2-4096-a7d0-a0970221c971"))
            (targets (list "pde"))
            (type luks-device-mapping))

           ;; TODO: change these label names (to work across systems)
           (mapped-device
            (source "matrix")
            (targets (list "matrix-root"
                           "matrix-swapvol"
                           "matrix-home"
                           "matrix-flatpak"
                           "matrix-data"))
            (type lvm-device-mapping))))

    (file-systems (cons*
                   (file-system
                     (device (file-system-label "kharisRoot"))
                     (mount-point "/")
                     (type "btrfs")
                     (flags '(no-atime))
                     (options "space_cache=v2")
                     (needed-for-boot? #t)
                     (dependencies mapped-devices))

                   (file-system
                     (device (file-system-label "Home"))
                     (mount-point "/home")
                     (type "ext4")
                     (needed-for-boot? #f)
                     (dependencies mapped-devices))

                   (file-system
                     (device (file-system-label "Data"))
                     (mount-point "/data")
                     (type "ext4")
                     (needed-for-boot? #f)
                     (dependencies mapped-devices))

                   (file-system
                     (device (file-system-label "Flatpak"))
                     (mount-point "/flatpak")
                     (type "ext4")
                     (needed-for-boot? #f)
                     (dependencies mapped-devices))

                   (file-system
                     (device (file-system-label "Steam"))
                     (mount-point "/flatpak/steam")
                     (type "ext4")
                     (needed-for-boot? #f))

                   ;; /boot/efi needs to be enumerated here
                   ;;   in addition to the (bootloader...) declaration
                   (file-system
                     ;; (device (uuid "B184-6A00" 'fat))
                     ;; or: (device (file-system-label "KHARISEFI"))
                     (device "/dev/nvme0n1p1")
                     (mount-point "/boot/efi")
                     (type "vfat"))
                   %base-file-systems))

    (swap-devices (list (swap-space
                         (target (file-system-label "kharisSwap"))
                         (dependencies mapped-devices))))))

system
