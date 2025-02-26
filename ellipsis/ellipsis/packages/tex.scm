(define-module (ellipsis packages tex)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system texlive)
  #:use-module (gnu packages))

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

(define texlive-lato
  (package
    (name "texlive-lato")
    (version (number->string %texlive-revision))
    (source (texlive-origin name version
                            (list "doc/fonts/lato/"
                                  "fonts/enc/dvips/lato/"
                                  "fonts/map/dvips/lato/"
                                  "fonts/tfm/typoland/lato/"
                                  "fonts/truetype/typoland/lato/"
                                  "fonts/type1/typoland/lato/"
                                  "fonts/vf/typoland/lato/"
                                  "tex/latex/lato/")
                            (base32
                             "1ykwm108zh79dv96axl74izzhzvmbx74pcl46i99ix2qpicyvcf7")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/lato")
    (synopsis "Lato font family and LaTeX support")
    (description
     "Lato is a sanserif typeface family designed in the Summer 2010 by Warsaw-based
designer Lukasz Dziedzic for the @code{tyPoland} foundry.  This font, which
includes five weights (hairline, light, regular, bold and black), is available
from the Google Font Directory as @code{TrueType} files under the Open Font
License version 1.1.  The package provides support for this font in
@code{LaTeX.} It includes the original @code{TrueType} fonts, as well as Type 1
versions, converted for this package using @code{FontForge} for full support
with Dvips.")
    (license (list license:silofl1.1 license:lppl1.3c))))

(define texlive-raleway
  (package
    (name "texlive-raleway")
    (version (number->string %texlive-revision))
    (source (texlive-origin name version
                            (list "doc/latex/raleway/"
                                  "fonts/enc/dvips/raleway/"
                                  "fonts/map/dvips/raleway/"
                                  "fonts/opentype/impallari/raleway/"
                                  "fonts/tfm/impallari/raleway/"
                                  "fonts/type1/impallari/raleway/"
                                  "fonts/vf/impallari/raleway/"
                                  "tex/latex/raleway/")
                            (base32
                             "1pp5m31rr4lww0z92q3vsaz1l01nb78ll5mn2l9w469hpghf2gd3")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/raleway")
    (synopsis "Use Raleway with TeX(-alike) systems")
    (description
     "The package provides the Raleway family in an easy to use way.  For
@code{XeLaTeX} and @code{LuaLaTeX} users the original @code{OpenType} fonts are
used.  The entire font family is included.")
    (license license:silofl1.1)))

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

(define texlive-anyfontsize
  (package
    (name "texlive-anyfontsize")
    (version (number->string %texlive-revision))
    (source (texlive-origin name version
                            (list "doc/latex/anyfontsize/"
                                  "tex/latex/anyfontsize/")
                            (base32
                             "0wr4brhggmkb1rwzmcc2r5ygzqp6090z0bp3sfbarwvwz903wpdn")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/anyfontsize")
    (synopsis "Select any font size in LaTeX")
    (description
     "The package allows the to user select any font size (via e.g.
\\fontsize{...}{...}\\selectfont), even those sizes that are not listed in the .fd
file.  If such a size is requested, @code{LaTeX} will search for and select the
nearest listed size; anyfontsize will then scale the font to the size actually
requested.  Similar functionality is available for the CM family, for the EC
family, or for either computer modern encoding; the present package generalises
the facility.")
    (license license:lppl)))
