;;; Module
;;
;; TODO: herd can't query service status or restart services. installation
;; needs to occur at login. sshd rejects connections (can't restart service
;; after IP address assignment). GDM doesnt permit login.
;;
(define-module (ellipsis system nonguix-install-amd)
  #:use-module (srfi srfi-1)
  #:use-module (gnu)
  #:use-module (gnu services sddm)
  #:use-module (gnu system)
  #:use-module (gnu system nss)
  #:use-module (gnu system pam)

  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (nongnu packages vpn)

  #:use-module (ellipsis packages gnupg)
  #:use-module (ellipsis packages tls)
  #:use-module (ellipsis packages emacs-xyz)
  #:use-module (ellipsis packages password-utils)
  #:use-module (ellipsis packages security-token)
  #:use-module (ellipsis packages golang-crypto)
  #:use-module (ellipsis system accounts)
  #:use-module (ellipsis system common)
  #:use-module (ellipsis system usb-gpg-tools)

  ;; get a list of channels
  #:use-module (guix describe)

  ;; gnutls packages
  #:use-module (gnu packages tls)

  #:export (nonguix-install-amd))

;; networking is [probably] needed for loopback
(use-service-modules networking ssh security-token authentication
                     desktop linux mcron networking xorg)
(use-package-modules curl wget rsync vim emacs emacs-xyz
                     wm freedesktop xdisorg fontutils fonts
                     networking linux time mtools acl hardware
                     package-management
                     lsof file-systems disk version-control
                     ssh gnupg cryptsetup security-token tls certs libusb
                     screen password-utils golang golang-crypto)

(define %host-name "nonguix-install")
(define %my-channels (current-channels))

(define %my-system-groups
  (append (map (lambda (g) (user-group (name g) (system? #t)))
               (list "realtime" "render" "plugdev" "yubikey" "fuse" "cgroup"
                     "docker" ;; "seat"
                     ))
          %base-groups))
(define %my-groups
  '("wheel" "users" "tty" "dialout"
    "input" "seat" "video" "audio" "netdev" "lp"
    ;; "kmem" "disk" "floppy" "cdrom" "tape" "kvm"
    "fuse" "realtime" "yubikey" "plugdev"
    "docker" "cgroup"))

(define wayland-packages
  (list grimblast
        xdg-desktop-portal))

(define sway-packages
  (list sway
        swaylock))

(define cage-packages
  (list cage))

(define hyprland-packages
  (list hyprland
        hyprlock
        hypridle
        hyprcursor
        xdg-desktop-portal-hyprland))

(define font-packages
  (list fontconfig
        font-google-roboto
        font-google-noto-emoji
        font-recursive
        font-microsoft-cascadia
        font-victor-mono
        font-jetbrains-mono
        font-intel-one-mono
        font-adwaita
        font-liberation
        font-dejavu
        font-awesome
        font-fira-code
        font-google-noto))

(define %base-desktop-services
  (remove (lambda (service)
            (memq (service-kind service)
                  (list gdm-service-type sddm-service-type)))
          %desktop-services))

;; NOTE: this doesn't set up ~/.ssh/authorized_keys
(define openssh-conf
  (openssh-configuration
   (openssh openssh-sans-x)
   (port-number (string->number
                 (or (getenv "_OPENSSH_PORT") "22")))
   (password-authentication? #f)
   (allow-agent-forwarding? #f)
   (allow-tcp-forwarding? #t)
   (accepted-environment '("COLORTERM"))
   (authorized-keys
    `(("dc"
       ,(local-file
         (string-append (getenv "HOME")
                        "/.ssh/authorized_keys")))))))

(define %wlgreet-sway-conf
  (plain-file "sway-greet.conf"
              (string-append
               "output * bg /data/xdg/Wallpapers/"
               %host-name "-greetd.jpg fill\n")))

(define %greetd-conf
  (greetd-configuration
   (greeter-supplementary-groups (list "video" "input" "seat"))
   (terminals
    (list
     (greetd-terminal-configuration (terminal-vt "1"))
     (greetd-terminal-configuration (terminal-vt "2"))
     (greetd-terminal-configuration (terminal-vt "3"))
     (greetd-terminal-configuration (terminal-vt "4"))
     (greetd-terminal-configuration (terminal-vt "5"))
     (greetd-terminal-configuration (terminal-vt "6"))
     (greetd-terminal-configuration
      (terminal-vt "7")
      (terminal-switch #t)
      (extra-shepherd-requirement '(seatd))
      (default-session-command
        (greetd-wlgreet-sway-session
         (sway-configuration %wlgreet-sway-conf)
         (command (greetd-user-session
                   (xdg-session-type "wayland"))))))
     (greetd-terminal-configuration
      (terminal-vt "8")
      (extra-shepherd-requirement '(seatd))
      (default-session-command
        (greetd-gtkgreet-sway-session
         (command (greetd-user-session
                   (xdg-session-type "wayland"))))))))))

;;;; Image
(define nonguix-install-amd
  (operating-system
    (host-name "usbgpgtool")
    (timezone "America/New_York")
    (locale "en_US.UTF-8")

    (keyboard-layout %el-altgr-kbd)

    ;; to install on a system with just BIOS (e.g. a VM)
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (targets "/dev/sda")))
    (file-systems (cons (file-system
                          (device (file-system-label "usb-gpg-disk"))
                          (mount-point "/")
                          (type "btrfs")
                          (flags '(no-atime))
                          (options "space_cache=v2"))
                        %base-file-systems))

    ;; NONFREE
    (kernel linux)
    ;; linux-firmware contains everything anyways (amd-microcode and
    ;; amdgpu-firmware are redundant)
    ;; https://gitlab.com/nonguix/nonguix/-/issues/327
    (firmware (cons* ;; linux-firmware
               amd-microcode
               amdgpu-firmware
               realtek-firmware
               %base-firmware))

    (kernel-arguments '("modprobe.blacklist=radeon"
                        ;; "quiet" ;; .....
                        ;; "net.iframes=0"
                        ))

    ;; TODO: users/groups (autologin to tty

    (groups %my-system-groups)
    (users (append (list
                    (user-account
                     (uid 1000)
                     (name "dc")
                     (password (crypt "dc1234321" "$6$abc"))
                     (comment "Default User")
                     (group "users")
                     (supplementary-groups %my-groups)))
                   %base-user-accounts))

    ;; misc packages:
    ;; f3: test flash storage
    ;; paperkey: print keys to paper
    ;; certdata2pem: convert between cert formats
    ;; datefudge: mock system time to set arbitrary cert start times
    ;; exfat-utils: work with FAT disks
    ;; pwsafe: manage passwords

    (packages
     (append

      %ugt-packages-cli
      %ugt-packages-net
      %ugt-packages-net-plus
      %ugt-packages-hardware
      %ugt-packages-age
      %ugt-packages-tls
      %ugt-packages-smartcard
      %ugt-packages-yubikey
      %ugt-packages-step
      %ugt-packages-gnupg
      %ugt-packages-secrets
      %ugt-packages-tpm

      (list emacs)
      %ugt-packages-emacs

      wayland-packages
      sway-packages
      hyprland-packages
      font-packages

      (list
       zerotier)

      %base-packages))

    (services
     (append %el-extra-files-svc
             (list (service gnome-desktop-service-type)
                   (service greetd-service-type %greetd-conf)
                   (service seatd-service-type))

             (list
              (service pcscd-service-type)
              (service openssh-service-type openssh-conf)

              ;; testing removing the fido2 functionality to restore yubikey
              (udev-rules-service 'fido2 libfido2)
              (udev-rules-service 'u2f libu2f-host)
              (udev-rules-service 'yubikey yubikey-personalization))

             ;; dbus complains about the name of plasma's notification
             ;;
             ;; service "should've been named
             ;; 'org.freedesktop.Notifications' or something
             ;;
             ;; (service plasma-desktop-service-type)

             (modify-services %base-desktop-services
               (delete agetty-service-type)
               (delete mingetty-service-type)
               (delete elogind-service-type)

               ;; consoles are mingetty
               ;; (gdm-service-type
               ;;  config => (gdm-configuration
               ;;             (inherit config)
               ;;             (wayland? #t)))

               (guix-service-type config => (el-nonguix-chan config)))))))

;; TODO: add gnupg service if configuration file is in place
