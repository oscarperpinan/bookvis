## para PDFs
pdfcrop <- function(x){
  cmd <- paste('/usr/bin/pdfcrop', x, x)
  system(cmd)
}

setwd('~/temp/figs/')
pdfs <- dir(pattern='.pdf')
lapply(pdfs, pdfcrop)


## para PNGs, JPEGs
pngcrop <- function(x){
  cmd <- paste('convert', x, '-trim', x)
  system(cmd)
  }

pngs <- dir(pattern='.png')
lapply(pngs, pngcrop)
