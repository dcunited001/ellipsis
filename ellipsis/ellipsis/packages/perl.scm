(define-module (ellipsis packages perl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system perl)

  #:use-module (gnu packages base)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)

  #:use-module (srfi srfi-1))

(define-public perl-text-markdown
  (package
    (name "perl-text-markdown")
    (version "1.000031")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/B/BO/BOBTFISH/Text-Markdown-" version
             ".tar.gz"))
       (sha256
        (base32 "06y79lla8adkqhrs41xdddqjs81dcrh266b50mfbg37bxkawd4f1"))))
    (build-system perl-build-system)
    (inputs (list perl))
    (native-inputs (list perl-list-moreutils
                         perl-test-differences
                         perl-test-exception))
    (arguments
     (list
      ;; #:modules '((guix build perl-build-system)
      ;;             (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'set-perl-search-path
            (lambda _
              ;; Work around for "@INC" build failure.
              (setenv "PERL5LIB"
                      (string-append (getcwd) ":"
                                     (getenv "PERL5LIB")))
              #t)))))
    (home-page "https://metacpan.org/release/Text-Markdown")
    (synopsis "Convert Markdown syntax to (X)HTML")
    (description "Markdown is a text-to-HTML filter; it translates an
easy-to-read / easy-to-write structured text format into HTML.  Markdown's
text format is most similar to that of plain text email, and supports features
such as headers, *emphasis*, code blocks, blockquotes, and links.")
    (license license:bsd-3)))
