;;; Module
(define-module (dc system images pxe-boot-tools)
  #:use-module (srfi srfi-1)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system nss)
  #:use-module (gnu system pam)

  #:use-module (dc packages networking)
  #:use-module (dc packages tls)
  #:use-module (dc packages security-token)
  #:use-module (dc packages golang-crypto)
  #:use-module (dc services security-token)
  #:use-module (dc system common)
  #:use-module (dc system images usb-gpg-tools)

  #:use-module (nongnu packages linux)
  #:use-module (nongnu packages vpn)
  #:use-module (nongnu services vpn)
  #:use-module (nongnu system linux-initrd)

  #:export (pxe-boot-tools))

;;;;; PXE Boot Tools

;; TODO: pxe-boot-tools: add services/packages/etc for pxe-tools
;; - netflow-like? network debugging?
;;
;; https://github.com/danderson/netboot/tree/main/pixiecore
;; https://nixos.wiki/wiki/Netboot

(use-service-modules networking ssh security-token)
(use-package-modules networking linux hardware admin ssh)

(define %pbt-openssh-conf
  (openssh-configuration
    (openssh openssh-sans-x)
    (password-authentication? #f)
    (allow-agent-forwarding? #f)
    (allow-tcp-forwarding? #t)
    (accepted-environment '("COLORTERM"))
    (authorized-keys
     `(("dc"
        ,(local-file
          (string-append (getenv "HOME")
                         "/.ssh/authorized_keys")))))))

;; tunctl bridge-utils iptables-nft
(define %pbt-packages
  (append (list
           ;; devops
           ansible
           debops
           ;; net
           netcat
           ngrep                        ; regexp on packets
           net-base
           iftop                        ; monitor net stats on interface
           nettop
           nmap
           nfdump
           wakelan
           wpa-supplicant
           wpa-supplicant-gui
           ndppd
           tcpdump                      ; libpcap
           sipcalc                      ; retrieve address info from interface
           tcptrack
           masscan
           lynis                        ; security auditing tool
           nmrpflash                    ; unbrick netgear devices
           zerotier

           ;; cpu
           corectrl                     ; app-specific CPU profiles
           pscircle
           stress
           stress-ng
           s-tui
           px                           ; newer ps/top/pstree

           ;; sysadmin
           lsofgraph
           sysdig                     ; strace+tcpdump+lsof w/ container support
           libcap-ng)
          %dc-profile-pkgs-cli
          %dc-profile-pkgs-net
          %dc-profile-pkgs-net-plus
          %dc-profile-pkgs-data
          %dc-profile-pkgs-fs
          %dc-profile-pkgs-hardware
          %dc-profile-pkgs-i2c
          %dc-profile-pkgs-age
          %dc-profile-pkgs-tls
          %dc-profile-pkgs-step
          %dc-profile-pkgs-gnupg
          %dc-profile-pkgs-secrets
          %dc-profile-pkgs-tpm))

;; listen on 'all for now; requires a service-activation gexp, nftables and
;; shepherd requirements to do much anything else (without knowing the hardware)
(define %pbt-services
  (list (service dhcp-service-type)
        (service openssh-service-type %pbt-openssh-conf)
        (service network-manager-service-type)))

(define pxe-boot-tools
  (operating-system
    (inherit usb-gpg-tools)
    (hostname "pxetools")
    (timezone "America/New_York")
    (locale "en_US.UTF-8")
    (bootloader (bootloader-configuration
                  (bootloader grub-efi-netboot-bootloader)
                  (targets '("/"))))
    (packages %pbt-packages)
    (services
     (append %pbt-services
             %ugt-services))))
