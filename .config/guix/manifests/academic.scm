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
   "texlive-times" "texlive-helvetic" "texlive-courier"))
