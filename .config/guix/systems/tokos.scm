;;* Module: tokos
(define-module (tokos)
  #:use-module (base-system)
  #:use-module (guix gexp)
  #:use-module (guix channels)

  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services pm)             ;; for thermald
  #:use-module (gnu services virtualization) ;; for libvirt
  #:use-module (gnu services audio)
  #:use-module (gnu services cups)
  #:use-module (gnu services security-token)

  #:use-module (gnu system)
  #:use-module (gnu system uuid)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)

  #:use-module (gnu packages firmware)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages cups)

  ;; NONFREE
  #:use-module (nongnu packages linux))

(use-service-modules desktop xorg networking ssh admin)
(use-package-modules certs shells linux)

;;** system info
(define %host-name (or (gethostname) "tokos"))

;;** keyboard
;; (define %tokos-default-shell-keyboard

;;** hardware
(define %tokos-modprobe-blacklist
  (dc-modprobe-blacklist '("usbmouse")))

;;** desktop-packages
;; (define %tokos-desktop-packages ...)

;;** desktop-services
;; TODO: docker is no longer added as a service by default
(define %tokos-desktop-services
  (modify-services
   dc-desktop-services
   (gdm-service-type config =>
                     (gdm-configuration
                      (inherit config)
                      (xorg-configuration
                       (xorg-configuration
                        (keyboard-layout %dc-default-shell-keyboard)
                        ;; (modules (append (list xf86-input-wacom)
                        ;;                  %default-xorg-modules))
                        (extra-config (list %xorg-libinput-config)))
                       )
                      (default-user "dc")))
   
   (guix-service-type config =>
                      (guix-configuration
                       (inherit config)
                       (extra-options '("-c3"))
                       (substitute-urls
                        (append (list "https://substitutes.nonguix.org")
                                %default-substitute-urls))
                       (authorized-keys
                        (append (list (local-file "../nonguix.pub"))
                                %default-authorized-guix-keys))))))

;;** operating-system
(define tokos-operating-system
  (operating-system
   (inherit base-operating-system)
   (host-name %host-name)

   (kernel linux)
   (kernel-arguments (list %tokos-modprobe-blacklist))

   (firmware (list linux-firmware
                   ;; has Broadcom BCM4331 wifi
                   openfwwf-firmware))
   
   (swap-devices (list (file-system-label "swap")))

   (packages %dc-desktop-packages)
   
   (services (cons*

	            (service tlp-service-type
                       (tlp-configuration
                        (nmi-watchdog #t)
                        (cpu-boost-on-ac? #t)
                        (tlp-default-mode "AC") ;; this is the default
                        (wifi-pwr-on-bat? #t)))

              ;; TODO: hmmmmmmm
	            ;; (pam-limits-service-type ;; This enables JACK to enter realtime mode
              ;;  (list
              ;;   (pam-limits-entry "@realtime" 'both 'rtprio 99)
              ;;   (pam-limits-entry "@realtime" 'both 'memlock 'unlimited)))

	            (extra-special-file "/usr/bin/env"
				                          (file-append coreutils "/bin/env"))

	            (service thermald-service-type)
              
	            ;; (service docker-service-type)

              ;; (service libvirt-service-type ;; TODO how is libvirt configured?
              ;;          (libvirt-configuration
              ;;           (unix-sock-group "libvirt")
              ;;           (tls-port "16555")))

	            (service pcscd-service-type)

              (service unattended-upgrade-service-type
                       (unattended-upgrade-configuration
                        (schedule "30 2 * * 0")
                        (channels #~(list
                                     (channel
                                      (name 'nonguix)
                                      (url "https://gitlab.com/nonguix/nonguix")
                                      (branch "master"))
                                     %default-channels))
                        (system-expiration (* 6 7 24 3600))
                        (operating-system-file
                         (file-append
                          (local-file "." "systems-dir" #:recursive? #t)
                          (string-append
                           "/root/.config/guix/systems/" %host-name ".scm")))))

	            (udev-rules-service 'pipewire-add-udev-rules pipewire)
              (udev-rules-service 'yubikey yubikey-personalization)
              ;; (bluetooth-service #:auto-enable? #t)

              %tokos-desktop-services))
   
   ;; NOTE expect patch for:
   ;; - (mapped-devices ...)
   ;; - (file-systems ...)
   (file-systems (cons*

                  (file-system
                   (device (file-system-label "tokosRoot"))
                   (mount-point "/")
		   (flags '(no-atime))
                   (type "btrfs")
		   (options "space_cache=v2")
                   (needed-for-boot? #t))

                  (file-system
                   (device (file-system-label "tokosHome"))
                   (mount-point "/home")
                   (type "ext4")
                   (needed-for-boot? #f))
		  
                  (file-system
                   (device (file-system-label "tokosData"))
                   (mount-point "/data")
                   (type "ext4")
                   (needed-for-boot? #f))

                  (file-system
                   (device "/dev/sda1")
                   (mount-point "/boot/efi")
                   (type "vfat"))
                  %base-file-systems))))

tokos-operating-system
