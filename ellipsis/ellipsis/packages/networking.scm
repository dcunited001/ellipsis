(define-module (ellipsis packages networking)
  #:use-module (srfi srfi-1)
  #:use-module (gnu)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages rrdtool)
  #:use-module (gnu packages)
  #:use-module (guix build utils)
  #:use-module (guix build gnu-build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:))

;; NOTE: probably a dirty build of nfdump. i'd be surprised if it has a
;; relatively full feature set (or worse ,if it doesn 't have runtime
;; problems). getting lots of config-check failures like:
;;
;; - sockaddr_storage
;; - bgp.h missing
;;
;; i'll probably remove it. this is a utility image anyways. the TFTP/etc that
;; serves the image will be on a restricted subnet (possibly inside a VRF)

;; https://github.com/dcunited001/zettelkasten/blob/master/dcguix/packages/networking/nfdump-and-ivre.org?plain=1#L101

(define-public nfdump
  (package
    (name "nfdump")
    (version "1.7.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/phaag/nfdump")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10v27k1y0q4zsshpgsx376099csmf04ih7hmx0xmavd2ziby1vgd"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; may also need to delete bootstrap and rerun
          (delete 'check))
      #:configure-flags
      #~(list
         "--enable-sflow"
         "--enable-nfpcapd"
         "--enable-nftrack"  ;; PortTracker
         "--enable-readpcap" ;; yes (test packets -> forwarded to collector)
         "--enable-tor"
         (string-append "--with-zstdpath=" #$(this-package-input "zstd")
                        "/include"))))
    (native-inputs
     (list autoconf-2.71 automake gettext-minimal libtool bison flex pkg-config))
    (inputs
     (list zlib rrdtool libpcap `(,zstd "lib") lzo lz4))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

;; "--enable-ftconv" ;; flow-tools to nfdump converter
;; "--enable-maxmind" ;; maxmind (paid), IVRE includes some data
;; "--enable-jnat" ;; JunOS
;; "--enable-nfprofile" ;; nfsen
;; ja4 default: NO; See JA4-Fingerprinting module (enabling req FoxIO license)
;; "--enable-ja4"
;; "--enable-devel"
