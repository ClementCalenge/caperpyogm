(TeX-add-style-hook
 "caperpyogm"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "a4paper")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("hyperref" "colorlinks=true" "linkcolor=blue" "urlcolor=blue" "citecolor=blue")))
   (add-to-list 'LaTeX-verbatim-environments-local "VerbatimOut")
   (add-to-list 'LaTeX-verbatim-environments-local "SaveVerbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "LVerbatim*")
   (add-to-list 'LaTeX-verbatim-environments-local "LVerbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "BVerbatim*")
   (add-to-list 'LaTeX-verbatim-environments-local "BVerbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "Verbatim*")
   (add-to-list 'LaTeX-verbatim-environments-local "Verbatim")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "Verb*")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "Verb")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "fancyvrb"
    "color"
    "url"
    "amsfonts"
    "epsfig"
    "hyperref"
    "longtable"
    "ucs"
    "savesym"
    "amsmath"
    "rotating"
    "appendix")
   (TeX-add-symbols
    '("INT" 1))
   (LaTeX-add-labels
    "sec:notations-1"
    "sec:notat-model-descr"
    "sec:model-description"
    "sec:model-description-1"
    "eq:eqy"
    "eq:detect"
    "eq:eqN"
    "eq:eqlambda"
    "sec:fitmc"
    "sec:conv-model-fit"
    "sec:goodness-fit"
    "sec:test-goodness-fit"
    "sec:grid-model"
    "sec:model-description-2"
    "eq:eqp"
    "sec:model-fit"
    "sec:conv-model-fit-grid"
    "sec:goodness-fit-grid"
    "sec:estim-numb-males"
    "eq:ug"
    "sec:other-models-detect"
    "sec:list-altern-models"
    "sec:use-cross-validation"
    "sec:k-fold-cv-study"
    "sec:implementation-r"
    "sec:discussion"
    "sec:lek--281"
    "sec:sens-our-model"
    "sec:crit-citel-citeb"
    "sec:unacc-vari-detect"
    "sec:simul-whole-proc"
    "sec:simul-detect-proc"
    "sec:effect-double-counts"
    "sec:simul-whole-proc-double"
    "sec:simul-detect-proc-double"
    "sec:history-program")
   (LaTeX-add-bibitems
    "Barker2017"
    "Link2018"
    "Martin2011a"
    "Merkle2019"
    "Robert2010a"
    "Vehtari2017")
   (LaTeX-add-environments
    "Default")
   (LaTeX-add-lengths
    "defaultparindent"))
 :latex)

