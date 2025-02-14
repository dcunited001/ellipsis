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
   "texlive-tabu"            ;; longtables?
   "texlive-latex-colortbl"  ;;
   "texlive-xcolor"          ;; alternating colors in tables
   "texlive-booktabs"        ;; table optimization

   "texlive-grfext" ;; also texlive-grffile
   "texlive-hyphenat"

   ;; included as dependencies:
   ;; "texlive-hyperref"

   ;; for xetex
   "texlive-collection-xetex"

   ;; or with less dependencies:
   ;; "texlive-xetex"

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

   ;; for EPS to PDF conversion in luatex commandline
   ;; (it seems that ox-beamer doesn't use this)
   "texlive-epstopdf"

   ;; for Beamer presentation exports
   "texlive-beamer"

   ;; for Beamer Poster exports
   "texlive-type1cm"
   "texlive-textpos"
   "texlive-fp"
   "texlive-paralist"

   ;; for beamerposter with gemini theme (and just generally)
   "texlive-ragged2e"
   "texlive-changepage"
   "texlive-pgfplots"

   ;; for dangom/org-beamerposter template
   "texlive-biblatex"
   "texlive-sourceserifpro"
   "texlive-sourcesanspro"
   "texlive-sourcecodepro"
   "texlive-sourceserifpro"
   "texlive-ly1"                        ; generally req. for beamerposter?

   ;; for tecosaur/engrave-faces latex defaults
   "texlive-fvextra"
   "texlive-fancyvrb"
   "texlive-upquote"
   "texlive-lineno"
   "texlive-tcolorbox"
   "texlive-environ"
   "texlive-pdfcol"

   ;; to add russian fonts via T2A encoding
   ;; https://tex.stackexchange.com/questions/646561/how-to-use-cyrillic-russian-with-sciposter
   "texlive-babel-russian"

   "gnuplot"

   ;; for jankapunkt/latexcv templates
   "texlive-roboto"
   "texlive-moresize"
   ;; for beamerposter
   "texlive-beamerposter"
   "texlive-anyfontsize"
   "texlive-lato"
   "texlive-raleway"))
