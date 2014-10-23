(TeX-add-style-hook "Review-Notes"
 (lambda ()
    (LaTeX-add-labels
     "sec:artif-intell"
     "sec:problem-solving")
    (TeX-run-style-hooks
     "graphicx"
     "xcolor"
     "geometry"
     "margin=10mm"
     "amssymb"
     "amsmath"
     ""
     "latex2e"
     "art10"
     "article"
     "twocolumn")))

