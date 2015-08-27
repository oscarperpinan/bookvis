(TeX-add-style-hook "dataTime"
 (lambda ()
    (LaTeX-add-index-entries
     "Data!SIAR"
     "Data!Meteorological
  variables"
     "Packages!zoo@\\texttt{zoo}"
     "read.zoo@\\texttt{read.zoo}"
     "zoo@\\texttt{zoo}"
     "Data!Solar radiation"
     "Data!Unemployment"
     "as.yearmon@\\texttt{as.yearmon}"
     "apply@\\texttt{apply}"
     "Data!World Bank"
     "Data!CO2@$CO_2$"
     "Data!GNI"
     "reshape@\\texttt{reshape}")
    (LaTeX-add-labels
     "sec-1"
     "SEC:dataTime"
     "sec-1-1"
     "fig:SIAR_map"
     "sec-1-1-1"
     "sec-1-1-2"
     "sec-1-2"
     "sec-1-3")))

