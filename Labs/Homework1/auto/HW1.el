(TeX-add-style-hook
 "HW1"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "12pt")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art12"
    "amsmath"
    "amssymb"
    "gensymb"
    "cite"
    "algorithmic"
    "ntheorem"
    "array"
    "xcolor"
    "graphicx"
    "url"
    "hyperref")
   (LaTeX-add-labels
    "sec:agents-search"
    "sec:graph-theory")
   (LaTeX-add-environments
    "Law")))

