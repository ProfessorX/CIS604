(TeX-add-style-hook
 "_region_"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "twocolumn")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "margin=10mm")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "amsmath"
    "amssymb"
    "geometry"
    "xcolor"
    "graphicx")
   (LaTeX-add-labels
    "sec:artif-intell"
    "sec:problem-solving"
    "sec:uninformed-search"
    "sec:informed-search"
    "eq:1"
    "sec:constr-satisf-probl"
    "sec:adversarial-search")))
