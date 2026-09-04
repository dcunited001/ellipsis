(use-modules (dc system images usb-gpg-tools)
             (dc services security-token)
             (srfi srfi-1)
             (ice-9 match)
             (guix)
             (guix channels)
             (gnu)
             (gnu packages firmware)
             (gnu packages package-management)
             (gnu system)
             (gnu system nss)
             (gnu system pam)
             (gnu system image))

(use-service-modules mcron networking ssh security-token
                     authentication)
(use-package-modules bootloaders fonts package-management)

(define %ugt-vm-services
  (append
   dc-smartcard-services
   (modify-services %base-services
     ;; https://forum.systemcrafters.net/t/how-to-get-rid-of-original-tty2/882
     ;; (delete mingetty-service-type)
     ;; (delete mingetty-service-type)
     ;; (delete mingetty-service-type)
     ;; (delete mingetty-service-type)
     ;; (delete mingetty-service-type)
     ;; (delete mingetty-service-type)
     (agetty-service-type
      config => (agetty-configuration
                  (inherit config)
                  (login-pause? #t)
                  (timeout 30)
                  ;;  -L, --local-line[=mode] Control the CLOCAL line flag
                  (extra-options '("-L"))
                  (baud-rate "115200")
                  ;; (tty "ttyS0")
                  (term "vt100"))))))

;; must use `guix image` otherwise building a UEFI VM is non-trivial,
;; since the `guix vm` qemu launcher script needs to find host ovmf
;; firmware
;;
;; See: https://codeberg.org/guix/guix/src/commit/4de2d5f68f630e9a22ba206644449993da4cf45f/gnu/system/image.scm#L598-L603
;;
;; and trace through references to (uefi-firmware system) in install.scm
;;     https://codeberg.org/guix/guix/src/commit/4de2d5f68f630e9a22ba206644449993da4cf45f/gnu/tests/install.scm#L213-L222
;; 
;; (firmware (list ovmf-x86-64))

(define usb-gpg-tools-vm
  (operating-system
    (inherit usb-gpg-tools)
    (host-name "usbgpgvm")
    (label (string-append "GNU Guix "
                          (or (getenv "GUIX_DISPLAYED_VERSION")
                              (package-version guix))))
    
    (bootloader (bootloader-configuration
                  (bootloader grub-bootloader)
                  (targets '("/dev/sda"))
                  (terminal-outputs '(console))))

    (kernel-arguments '("noquiet"))
    (file-systems (cons (file-system
                          (mount-point "/")
                          (device (file-system-label "usb-gpg-disk"))
                          (type "ext4"))
                        %base-file-systems))
    (services %ugt-vm-services)))

usb-gpg-tools-vm
