(TeX-add-style-hook "sigproc-sp"
 (lambda ()
    (LaTeX-add-bibliographies
     "sigproc")
    (LaTeX-add-environments
     "theorem")
    (TeX-run-style-hooks
     "latex2e"
     "acm_proc_article-sp10"
     "acm_proc_article-sp"
     "")))

