(TeX-add-style-hook
 "Review-Notes"
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
    "sec:adversarial-search"
    "sec:uncert-knowl-reas"
    "sec:expectimax-utilities"
    "sec:mark-decis-proc"
    "sec:mark-decis-proc-1"
    "sec:probability"
    "sec:markov-models"
    "sec:hmms-part-filt"
    "sec:application-hmms"
    "sec:bayes-nets:-repr"
    "sec:bayes-nets:-indep"
    "sec:learning"
    "sec:reinf-learn-1"
    "sec:reinf-learn-2")))

