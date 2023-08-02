(use-packages ((guix licenses) #:prefix license:)
              (guix gexp)
              (guix download)
              (guix build-system gnu)
              (guix build-system texlive)
              (gnu packages))

;; ....ok wow `guix import texlive roboto` ... nice
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
  (license (list asl2.0 silofl1.1 lppl)))

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

   ;; included as dependencies:
   ;; "texlive-hyperref"

   ;; for jankapunkt/latexcv
   "texlive-xifthen"
   "texlive-ifmtarg"

   "gnuplot"
   ))


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
