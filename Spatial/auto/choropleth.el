(TeX-add-style-hook "choropleth"
 (lambda ()
    (LaTeX-add-index-entries
     "Packages!maps@\\texttt{maps}"
     "Packages!maptools@\\texttt{maptools}"
     "Packages!rgeos@\\texttt{rgeos}"
     "Packages!sp@\\texttt{sp}"
     "Packages!latticeExtra@\\texttt{latticeExtra}"
     "Packages!colorspace@\\texttt{colorspace}"
     "INE"
     "readShapePoly@\\texttt{readShapePoly}"
     "Encoding@\\texttt{Encoding}"
     "unionSpatialPolygons@\\texttt{unionSpatialPolygons}"
     "rainbow_hcl@\\texttt{rainbow\\_hcl}"
     "hcl@\\texttt{hcl}"
     "sequential_hcl@\\texttt{sequential\\_hcl}"
     "Reduce@\\texttt{Reduce}"
     "spplot@\\texttt{spplot}"
     "textGrob@\\texttt{textGrob}"
     "packGrob\\texttt{packGrob}"
     "Packages!grid\\texttt{grid}")
    (LaTeX-add-labels
     "sec-1"
     "sec:multiChoropleth"
     "sec-1-1"
     "sec-1-2"
     "sec:map"
     "fig:whichMax"
     "fig:pcMax"
     "sec-1-3"
     "fig:mapLegends")))

