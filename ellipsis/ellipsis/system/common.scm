;;; Module: common
(define-module (ellipsis system common)

  #:use-module (ellipsis packages emacs-xyz)
  #:use-module (ellipsis packages gnupg)
  #:use-module (ellipsis packages golang-crypto)
  #:use-module (ellipsis packages password-utils)
  #:use-module (ellipsis packages security-token)
  #:use-module (ellipsis packages tls)

  #:use-module (ice-9 format)

  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu services)
  #:use-module (gnu services base)

  #:use-module (srfi srfi-1)
  #:export (%el-extra-files-svc))

(use-package-modules wget curl screen password-utils vim tmux emacs emacs-xyz
                     package-management ; remove?
                     networking linux hardware rsync acl admin diffoscope
                     time mtools lsof file-systems disk version-control web
                     ssh gnupg cryptsetup security-token tls certs libusb
                     fontutils fonts
                     golang-crypto)

(define %el-extra-files-svc
  (list
   (extra-special-file "/usr/bin/env"
                       (file-append coreutils "/bin/env"))
   (extra-special-file "/lib64/ld-linux-x86-64.so.2"
                       (file-append glibc "/lib/ld-linux-x86-64.so.2"))))

(define-public %el-altgr-kbd
  (keyboard-layout "us" "altgr-intl"
                   #:model "pc105"
                   #:options '("caps:ctrl_modifier")))

;;; Nonguix Subs

(define-public %nonguix-chan-key
  (plain-file "nonguix.pub"
              "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))

(define-public el-nonguix-chan-subs
  (guix-extension
   (substitute-urls
    (append (list "https://substitutes.nonguix.org")
            %default-substitute-urls))
   (authorized-keys
    (append
     (list %nonguix-chan-key)
     %default-authorized-guix-keys))))

(define-public (el-guix-configuration channels)
  (guix-configuration
   (inherit %default-guix-configuration)
   (guix (guix-for-channels channels))
   (channels channels)
   (authorize-key? #t)
   (authorized-keys
    (cons* %nonguix-key
           %default-authorized-guix-keys))
   (substitute-urls
    '("https://ci.guix.gnu.org"
      "https://substitutes.nonguix.org"
      "https://bordeaux.guix.gnu.org"))
   (extra-options '("--max-jobs=6"
                    "--cores=0"))))

;;; Packages

;;;; CLI Tools

(define-public %el-profile-pkgs-cli
  (list lsof git stow vim screen tmux diffoscope))

;; TODO: enable local networking for usb-gpg-tools
(define-public %el-profile-pkgs-net
  (list tunctl bridge-utils iptables-nft))

(define-public %el-profile-pkgs-net-plus
  (list wget curl rsync))

(define-public %el-profile-pkgs-data
  (list jq yq jc))

(define-public %el-profile-pkgs-fs
  (list lvm2 cryptsetup dosfstools ntfs-3g exfat-utils fuse-exfat f3 acl))

(define-public %el-profile-pkgs-hardware
  (list hwinfo rng-tools hw-probe dmidecode fiano du-dust))
;; fiano: uefi image utils; du-dust: diskonaut

(define-public %el-profile-pkgs-i2c
  (list ;; ddcci-driver-linux
   i2c-tools ddcutil))

(define-public %el-profile-pkgs-age
  (list age age-keygen age-plugin-tpm-bin age-plugin-yubikey-bin))

(define-public %el-profile-pkgs-tls
  ;; desec-certbot-hook
  (list openssh openssl le-certs gnutls certdata2pem))

(define-public %el-profile-pkgs-smartcard
  ;; hidapi: HID Devices for FIDO/OTP
  (list ccid pcsc-lite opensc pinentry-tty hidapi libu2f-host libfido2))

(define-public %el-profile-pkgs-yubikey
  (list yubico-piv-tool yubikey-personalization python-yubikey-manager))

;; NOTE: step-kms-plugin should work if ldd discovers
;; pscscd via rpath

(define-public %el-profile-pkgs-step
  (list step-kms-plugin-bin step-ca-bin step-cli-bin))

(define-public %el-profile-pkgs-gnupg
  (list gnupg paperkey datefudge))

(define-public %el-profile-pkgs-secrets
  (list sops-bin))

(define-public %el-profile-pkgs-tpm
  (list tpm2-tss ssh-tpm-agent-bin))

;;;; Emacs

(define-public %el-profile-pkgs-terminal-emacs
  (list emacs-x509-mode
        emacs-better-defaults
        emacs-auto-complete
        emacs-a
        emacs-hydra
        emacs-modus-themes
        emacs-dash
        emacs-lispy
        emacs-geiser
        emacs-geiser-guile
        emacs-ac-geiser
        emacs-guix
        emacs-yasnippet
        emacs-yasnippet-snippets))

;; added "nice to have" packages, which should not normally be
;; installed for root

(define-public %el-profile-pkgs-consult-emacs
  (list emacs-cape
        emacs-consult
        emacs-consult-dir
        ;; emacs-consult-flycheck
        emacs-corfu
        emacs-corfu-terminal
        emacs-embark
        emacs-marginalia
        emacs-orderless
        emacs-vertico))

;;;; Desktop

(define-public %el-profile-pkgs-font
  (list font-abattis-cantarell
        font-adwaita
        font-dejavu
        font-fira-code
        font-iosevka
        font-iosevka-aile
        font-gnu-freefont
        font-google-noto
        font-google-noto-emoji
        font-google-roboto
        font-intel-one-mono
        font-jetbrains-mono
        font-liberation))

;; font-microsoft-cascadia
;; font-recursive
;; font-victor-mono
;;
;; font-awesome
