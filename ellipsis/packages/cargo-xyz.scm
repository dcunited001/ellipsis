;; -*- mode: scheme -*-
(define-module (ellipsis packages cargo-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)

  #:use-module (guix build utils)

  #:use-module (guix build-system copy)

  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  ;; #:use-module (gnu packages hardware) ;; for tpm2-tss

  #:use-module (ellipsis packages)
  #:use-module (ellipsis packages perl)
  #:use-module (srfi srfi-1))

(define-public rust-age-plugin-yubikey-0.4
  (package
    (name "rust-age-plugin-yubikey")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "age-plugin-yubikey" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xdqb92fzpxl91a1v95w405a783k68jgjazjsfgy78c8v860b018"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-age-core" ,rust-age-core-0.9)
                       ("rust-age-plugin" ,rust-age-plugin-0.4)
                       ("rust-bech32" ,rust-bech32-0.9)
                       ("rust-i18n-embed" ,rust-i18n-embed-0.13)
                       ("rust-i18n-embed-fl" ,rust-i18n-embed-fl-0.6)
                       ("rust-pcsc" ,rust-pcsc-2)
                       ("rust-yubikey" ,rust-yubikey-0.8)

                       ("rust-rust-embed" ,rust-rust-embed-6) ;; 5.9.0
                       ("rust-sysinfo" ,rust-sysinfo-0.28)    ;; 0.27.9
                       ("rust-x509-parser" ,rust-x509-parser-0.14) ;; 0.15.0 (or 0.12.0)

                       ("rust-base64" ,rust-base64-0.21)         ;; 0.21.4
                       ("rust-console" ,rust-console-0.15)       ;; 0.15.5
                       ("rust-dialoguer" ,rust-dialoguer-0.10)   ;; 0.10.4
                       ("rust-env-logger" ,rust-env-logger-0.10) ;; 0.10.0
                       ("rust-gumdrop" ,rust-gumdrop-0.8)        ;; 0.8.0
                       ("rust-hex" ,rust-hex-0.4)                ;; 0.4.3
                       ("rust-lazy-static" ,rust-lazy-static-1)  ;; 1.4.0
                       ("rust-log" ,rust-log-0.4)                ;; 0.4.20
                       ("rust-p256" ,rust-p256-0.13)             ;; 1.13.2
                       ("rust-rand" ,rust-rand-0.8)              ;; 0.8.5
                       ("rust-sha2" ,rust-sha2-0.10)             ;; 0.10.8
                       ("rust-which" ,rust-which-4)              ;; 4.3.0
                       ("rust-x509" ,rust-x509-0.2)              ;;

                       )
       #:cargo-development-inputs (("rust-flate2" ,rust-flate2-1)
                                   ("rust-man" ,rust-man-0.3)
                                   ("rust-tempfile" ,rust-tempfile-3)
                                   ("rust-test-with" ,rust-test-with-0.9)
                                   ("rust-which" ,rust-which-4))))
    (home-page "https://github.com/str4d/age-plugin-yubikey")
    (synopsis "YubiKey plugin for age clients")
    (description "@code{YubiKey} plugin for age clients")
    (license (list license:expat license:asl2.0))))

(define-public rust-der-derive-0.7
  (package
    (name "rust-der-derive")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "der_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0jg0y3k46bpygwc5cqha07axz5sdnsx5116g3nxf0rwrabj7rs2z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/der/derive")
    (synopsis
     "Custom derive support for the `der` crate's `Choice` and `Sequence` traits")
    (description
     "Custom derive support for the `der` crate's `Choice` and `Sequence` traits")
    (license (list license:asl2.0 license:expat))))

(define-public rust-der-0.7
  (package
    (name "rust-der")
    (version "0.7.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "der" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "070bwiyr80800h31c5zd96ckkgagfjgnrrdmz3dzg2lccsd3dypz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-bytes" ,rust-bytes-1)
                       ("rust-const-oid" ,rust-const-oid-0.9)
                       ("rust-der-derive" ,rust-der-derive-0.7)
                       ("rust-flagset" ,rust-flagset-0.4)
                       ("rust-pem-rfc7468" ,rust-pem-rfc7468-0.7)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/der")
    (synopsis
     "Pure Rust embedded-friendly implementation of the Distinguished Encoding Rules
(DER) for Abstract Syntax Notation One (ASN.1) as described in ITU X.690 with
full support for heapless no_std targets
")
    (description
     "Pure Rust embedded-friendly implementation of the Distinguished Encoding Rules
(DER) for Abstract Syntax Notation One (ASN.1) as described in ITU X.690 with
full support for heapless no_std targets")
    (license (list license:asl2.0 license:expat))))

(define-public rust-const-oid-0.9
  (package
    (name "rust-const-oid")
    (version "0.9.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "const-oid" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1y0jnqaq7p2wvspnx7qj76m7hjcqpz73qzvr9l2p9n2s51vr6if2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/const-oid")
    (synopsis
     "Const-friendly implementation of the ISO/IEC Object Identifier (OID) standard
as defined in ITU X.660, with support for BER/DER encoding/decoding as well as
heapless no_std (i.e. embedded) support
")
    (description
     "Const-friendly implementation of the ISO/IEC Object Identifier (OID) standard as
defined in ITU X.660, with support for BER/DER encoding/decoding as well as
heapless no_std (i.e.  embedded) support")
    (license (list license:asl2.0 license:expat))))

(define-public rust-x509-cert-0.2
  (package
    (name "rust-x509-cert")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "x509-cert" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0aggyd8b5fqkiklkkhcz0nxanqpwg587xc7yawxdl0cpv6hzrvi5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-const-oid" ,rust-const-oid-0.9)
                       ("rust-der" ,rust-der-0.7)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-signature" ,rust-signature-2)
                       ("rust-spki" ,rust-spki-0.7))))
    (home-page "https://github.com/RustCrypto/formats/tree/master/x509-cert")
    (synopsis
     "Pure Rust implementation of the X.509 Public Key Infrastructure Certificate
format as described in RFC 5280
")
    (description
     "Pure Rust implementation of the X.509 Public Key Infrastructure Certificate
format as described in RFC 5280")
    (license (list license:asl2.0 license:expat))))

(define-public rust-secrecy-0.8
  (package
    (name "rust-secrecy")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "secrecy" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "07p9h2bpkkg61f1fzzdqqbf74kwv1gg095r1cdmjzzbcl17cblcv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytes" ,rust-bytes-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page "https://github.com/iqlusioninc/crates/")
    (synopsis
     "Wrapper types and traits for secret management which help ensure
they aren't accidentally copied, logged, or otherwise exposed
(as much as possible), and also ensure secrets are securely wiped
from memory when dropped.
")
    (description
     "Wrapper types and traits for secret management which help ensure they aren't
accidentally copied, logged, or otherwise exposed (as much as possible), and
also ensure secrets are securely wiped from memory when dropped.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-pcsc-sys-1
  (package
    (name "rust-pcsc-sys")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pcsc-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1si37v9n07r3csqcnnqn4i82j75b6dssyz0fzdg1n3rcpbnbzdz1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-pkg-config" ,rust-pkg-config-0.3))))
    (home-page "https://github.com/bluetech/pcsc-rust")
    (synopsis "Low-level bindings to the PC/SC C API")
    (description "Low-level bindings to the PC/SC C API")
    (license license:expat)))

(define-public rust-pcsc-2
  (package
    (name "rust-pcsc")
    (version "2.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pcsc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nnsljri06w5c9lj99h08rprfkn0dxb4f900wx39zcri4gsyw4xv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-pcsc-sys" ,rust-pcsc-sys-1))))
    (home-page "https://github.com/bluetech/pcsc-rust")
    (synopsis "Bindings to the PC/SC API for smart card communication")
    (description "Bindings to the PC/SC API for smart card communication")
    (license license:expat)))

(define-public rust-zeroize-1
  (package
    (name "rust-zeroize")
    (version "1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "zeroize" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0bfvby7k9pdp6623p98yz2irqnamcyzpn7zh20nqmdn68b0lwnsj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-zeroize-derive" ,rust-zeroize-derive-1))))
    (home-page "https://github.com/RustCrypto/utils/tree/master/zeroize")
    (synopsis "Securely clear secrets from memory with a simple trait built on
stable Rust primitives which guarantee memory is zeroed using an
operation will not be 'optimized away' by the compiler.
Uses a portable pure Rust implementation that works everywhere,
even WASM!
")
    (description
     "Securely clear secrets from memory with a simple trait built on stable Rust
primitives which guarantee memory is zeroed using an operation will not be
optimized away by the compiler.  Uses a portable pure Rust implementation that
works everywhere, even WASM!")
    (license (list license:asl2.0 license:expat))))

(define-public rust-elliptic-curve-0.13
  (package
    (name "rust-elliptic-curve")
    (version "0.13.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "elliptic-curve" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0ixx4brgnzi61z29r3g1606nh2za88hzyz8c5r3p6ydzhqq09rmm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-base16ct" ,rust-base16ct-0.2)
                       ("rust-base64ct" ,rust-base64ct-1)
                       ("rust-crypto-bigint" ,rust-crypto-bigint-0.5)
                       ("rust-digest" ,rust-digest-0.10)
                       ("rust-ff" ,rust-ff-0.13)
                       ("rust-generic-array" ,rust-generic-array-0.14)
                       ("rust-group" ,rust-group-0.13)
                       ("rust-hex-literal" ,rust-hex-literal-0.4)
                       ("rust-hkdf" ,rust-hkdf-0.12)
                       ("rust-pem-rfc7468" ,rust-pem-rfc7468-0.7)
                       ("rust-pkcs8" ,rust-pkcs8-0.10)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-sec1" ,rust-sec1-0.7)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serdect" ,rust-serdect-0.2)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-tap" ,rust-tap-1)
                       ("rust-zeroize" ,rust-zeroize-1))))
    (home-page
     "https://github.com/RustCrypto/traits/tree/master/elliptic-curve")
    (synopsis
     "General purpose Elliptic Curve Cryptography (ECC) support, including types
and traits for representing various elliptic curve forms, scalars, points,
and public/secret keys composed thereof.
")
    (description
     "General purpose Elliptic Curve Cryptography (ECC) support, including types and
traits for representing various elliptic curve forms, scalars, points, and
public/secret keys composed thereof.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-ecdsa-0.16
  (package
    (name "rust-ecdsa")
    (version "0.16.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ecdsa" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jhb0bcbkaz4001sdmfyv8ajrv8a1cg7z7aa5myrd4jjbhmz69zf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-der" ,rust-der-0.7)
                       ("rust-digest" ,rust-digest-0.10)
                       ("rust-elliptic-curve" ,rust-elliptic-curve-0.13)
                       ("rust-rfc6979" ,rust-rfc6979-0.4)
                       ("rust-serdect" ,rust-serdect-0.2)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-signature" ,rust-signature-2)
                       ("rust-spki" ,rust-spki-0.7))))
    (home-page "https://github.com/RustCrypto/signatures/tree/master/ecdsa")
    (synopsis
     "Pure Rust implementation of the Elliptic Curve Digital Signature Algorithm
(ECDSA) as specified in FIPS 186-4 (Digital Signature Standard), providing
RFC6979 deterministic signatures as well as support for added entropy
")
    (description
     "Pure Rust implementation of the Elliptic Curve Digital Signature Algorithm
(ECDSA) as specified in FIPS 186-4 (Digital Signature Standard), providing
RFC6979 deterministic signatures as well as support for added entropy")
    (license (list license:asl2.0 license:expat))))

(define-public rust-yubikey-0.8
  (package
    (name "rust-yubikey")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "yubikey" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jzdmpy8zsl1nw8anaqz7h4gmaq0v40cb38whz7x9vg3q51zn7kx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-base16ct" ,rust-base16ct-0.2)
                       ("rust-der" ,rust-der-0.7)
                       ("rust-des" ,rust-des-0.8)
                       ("rust-ecdsa" ,rust-ecdsa-0.16)
                       ("rust-elliptic-curve" ,rust-elliptic-curve-0.13)
                       ("rust-hmac" ,rust-hmac-0.12)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-nom" ,rust-nom-7)
                       ("rust-num-bigint-dig" ,rust-num-bigint-dig-0.8)
                       ("rust-num-integer" ,rust-num-integer-0.1)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-p256" ,rust-p256-0.13)
                       ("rust-p384" ,rust-p384-0.13)
                       ("rust-pbkdf2" ,rust-pbkdf2-0.12)
                       ("rust-pcsc" ,rust-pcsc-2)
                       ("rust-rand-core" ,rust-rand-core-0.6)
                       ("rust-rsa" ,rust-rsa-0.9)
                       ("rust-secrecy" ,rust-secrecy-0.8)
                       ("rust-sha1" ,rust-sha1-0.10)
                       ("rust-sha2" ,rust-sha2-0.10)
                       ("rust-signature" ,rust-signature-2)
                       ("rust-subtle" ,rust-subtle-2)
                       ("rust-uuid" ,rust-uuid-1)
                       ("rust-x509-cert" ,rust-x509-cert-0.2)
                       ("rust-zeroize" ,rust-zeroize-1))
       #:cargo-development-inputs (("rust-env-logger" ,rust-env-logger-0.10)
                                   ("rust-once-cell" ,rust-once-cell-1)
                                   ("rust-signature" ,rust-signature-2))))
    (home-page "https://github.com/iqlusioninc/yubikey.rs")
    (synopsis
     "Pure Rust cross-platform host-side driver for YubiKey devices from Yubico with
support for hardware-backed public-key decryption and digital signatures using
the Personal Identity Verification (PIV) application. Supports RSA (1024/2048)
or ECC (NIST P-256/P-384) algorithms e.g, PKCS#1v1.5, ECDSA
")
    (description
     "Pure Rust cross-platform host-side driver for @code{YubiKey} devices from Yubico
with support for hardware-backed public-key decryption and digital signatures
using the Personal Identity Verification (PIV) application.  Supports RSA
(1024/2048) or ECC (NIST P-256/P-384) algorithms e.g, PKCS#1v1.5, ECDSA")
    (license license:bsd-2)))
