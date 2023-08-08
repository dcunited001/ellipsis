(use-modules ((guix licenses) #:prefix license:)
              (guix gexp)
              (guix packages)
              (guix utils)
              (guix download)
              (guix build-system gnu)
              (guix build-system texlive)
              (gnu packages))

;; ....ok wow `guix import texlive roboto` ... nice
(define texlive-roboto
  (package
    (name "texlive-roboto")
    (version (number->string %texlive-revision))
    (source (texlive-origin name version
                            (list "doc/fonts/roboto/"
                                  "fonts/enc/dvips/roboto/"
                                  "fonts/map/dvips/roboto/"
                                  "fonts/opentype/google/roboto/"
                                  "fonts/tfm/google/roboto/"
                                  "fonts/type1/google/roboto/"
                                  "fonts/vf/google/roboto/"
                                  "tex/latex/roboto/")
                            (base32
                             "1gbg9p9y6a2fis88qfcsscksrkkcnqvsrhdkak2jm2dfjnq6v2n8")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/roboto")
    (synopsis "Support for the Roboto family of fonts")
    (description
     "This package provides @code{LaTeX,} @code{pdfLaTeX,} @code{XeLaTeX} and
@code{LuaLaTeX} support for the Roboto Sans, Roboto Condensed, Roboto Mono,
Roboto Slab and Roboto Serif families of fonts, designed by Christian Robertson
and Greg Gazdowicz for Google.")
    (license (list license:asl2.0 license:silofl1.1 license:lppl))))

(define texlive-moresize
  (package
    (name "texlive-moresize")
    (version (number->string %texlive-revision))
    (source (texlive-origin name version
                            (list "doc/latex/moresize/" "source/latex/moresize/"
                                  "tex/latex/moresize/")
                            (base32
                             "0wa1pp7k5s8xcxjw37543jdjclnx5hzcx4gnivhsb34s9nhwcjzn")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/moresize")
    (synopsis "Allows font sizes up to 35.83pt")
    (description
     "This package provides a package for using font sizes up to 35.88pt, for example
with the EC fonts.  New commands \\HUGE and \\ssmall for selecting font sizes are
provided together with some options working around current @code{LaTeX2e}
shortcomings in using big font sizes.  The package also provides options for
improving the typesetting of paragraphs (or headlines) with embedded math
expressions at font sizes above 17.28pt.")
    (license license:lppl)))

(define texlive-beamerposter
  (package
    (name "texlive-beamerposter")
    (version (number->string %texlive-revision))
    (source (texlive-origin name version
                            (list "doc/latex/beamerposter/"
                                  "tex/latex/beamerposter/")
                            (base32
                             "1xp8a6d82n1kgagdc7mm7hjihdzn1k7y4lijy924hjdvnvdmqa2i")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/beamerposter")
    (synopsis "Extend beamer and a0poster for custom sized posters")
    (description
     "The package enables the user to use beamer style operations on a canvas of the
sizes provided by a0poster; font scaling is available (using packages such as
type1cm if necessary).  In addition, the package allows the user to benefit from
the nice colour box handling and alignment provided by the beamer class (for
example, with rounded corners and shadows).  Good looking posters may be created
very rapidly.  Features include: scalable fonts using the fp and type1cm
packages; posters in A-series sizes, and custom sizes like double A0 are
possible; still applicable to custom beamer slides, e.g. 16:9 slides for a
wide-screen (i.e.  1.78 aspect ratio); orientation may be portrait or landscape;
a debug mode is provided.")
    (license (list license:lppl license:gpl3+))))


(concatenate-manifests
 (list
  (specifications->manifest
   '("rubber"

     "texlive-base"
     "texlive-wrapfig"

     "texlive-microtype"
     "texlive-listings" "texlive-hyperref"

     ;; PGF/TikZ
     "texlive-pgf"

     ;; required for org-latex (ulem.sty)
     ;; "texlive-jadetex" ;; fails
     "texlive-ulem"

     ;; Additional fonts.
     "texlive-cm-super" "texlive-amsfonts"
     "texlive-times" "texlive-helvetic" "texlive-courier"

     "texlive-fonts-ec"
     "texlive-fonts-latex"

     "texlive-latex-cmap"
     "texlive-tex-gyre"
     "texlive-unicode-math"
     "font-tex-gyre"

     "texlive-latex-geometry" ;; document formatting
     "texlive-units"          ;; unit formatting (alt: siunits)
     "texlive-comment"        ;; comment formatting
     "texlive-caption"        ;; caption
     "texlive-capt-of"        ;; \caption-of
     "texlive-lm"             ;;

     ;; table formatting: req. for tabu
     "texlive-tabu"           ;; longtables?
     "texlive-latex-colortbl" ;;
     "texlive-xcolor"         ;; alternating colors in tables
     "texlive-booktabs"       ;; table optimization

     "texlive-grfext" ;; also texlive-grffile
     "texlive-hyphenat"

     ;; included as dependencies:
     ;; "texlive-hyperref"

     ;; for jankapunkt/latexcv
     "texlive-xifthen"
     "texlive-ifmtarg"
     ;; "texlive-roboto"
     "texlive-xkeyval"
     "texlive-fontaxes"
     "texlive-mweights"
     ;; "texlive-moresize"
     "texlive-multirow"
     "texlive-enumitem"
     "texlive-graphics"
     "texlive-float"

     ;; for PDF generation in ox-latex
     "texlive-latexmk"

     ;; for Beamer presentation exports
     "texlive-beamer"

     "gnuplot"
     ))
  (packages->manifest
   (list texlive-roboto
         texlive-moresize
         texlive-beamerposter))))

;; (guix build-system texlive) provides a few helpers

;; copy-recursively? ...
;; (define simpler-texlive-package (name locations hash)
;;   (package
;;     (name name)
;;     (version (number->string %texlive-revision))
;;     (source (texlive-origin name version
;;                             locations hash))
;;     ;; (outputs '("out"))
;;     (build-system gnu-build-system)
;;     (arguments
;;      (let ((copy-files
;;             `(lambda* (#:key outputs inputs #:allow-other-keys)
;;                (let (out (string-append (assoc-ref outputs "out")
;;                                         "/share/texmf-dist/"))
;;                  (mkdir-p out)
;;                  (copy-recursively "." out)
;;                  #t))))
;;        `(#:tests? #f
;;          #:phases
;;          (modify-phases %standard-phases
;;            (delete 'configure)
;;            (replace 'build (const #t))
;;            (replace 'install ,copy-files)))))
;;     (home-pgae #f)
;;     (synopsis #f)
;;     (description #f)
;;     (license #f)))
