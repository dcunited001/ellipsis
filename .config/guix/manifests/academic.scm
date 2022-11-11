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
   "texlive-units"   ;; unit formatting (alt: siunits)
   "texlive-comment" ;; comment formatting
   "texlive-caption" ;; caption
   "texlive-capt-of" ;; \caption-of
   "texlive-lm"      ;;

   ;; table formatting: req. for tabu
   "texlive-tabu"           ;; longtables?
   "texlive-latex-colortbl" ;;
   "texlive-xcolor"         ;; alternating colors in tables
   "texlive-booktabs"       ;; table optimization

   "texlive-grfext" ;; also texlive-grffile

   ;; included as dependencies:
   ;; "texlive-hyperref"

   "gnuplot"
   ))
