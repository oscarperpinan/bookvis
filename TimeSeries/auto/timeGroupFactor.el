(TeX-add-style-hook "timeGroupFactor"
 (lambda ()
    (LaTeX-add-index-entries
     "splom@\\texttt{splom}"
     "panel.link.splom@\\texttt{panel.link.splom}"
     "trellis.focus@\\texttt{trellis.focus}"
     "Packages!hexbin@\\texttt{hexbin}"
     "panel.hexbinplot@\\texttt{panel.hexbinplot}"
     "panel.loess@\\texttt{panel.loess}"
     "diag.panel.splom@\\texttt{diag.panel.splom}"
     "current.panel.limits@\\texttt{current.panel.limits}"
     "Panel function"
     "reshape@\\texttt{reshape}"
     "hexbinplot@\\texttt{hexbinplot}"
     "useOuterStrips@\\texttt{useOuterStrips}"
     "panel.rug@\\texttt{panel.rug}"
     "Packages!latticeExtra@\\texttt{latticeExtra}")
    (LaTeX-add-labels
     "sec-1"
     "SEC:groupVariable"
     "fig:aranjuezSplom"
     "sec-1-1"
     "SEC:hexbin"
     "fig:aranjuezSplomHexbin"
     "fig:aranjuezHexbin"
     "fig:aranjuezGGhexbin"
     "sec-2"
     "SEC:conditionVariable"
     "fig:aranjuezFacetGrid"
     "fig:aranjuezOuterStrips")))

