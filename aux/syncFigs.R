## Copio imágenes de dropbox a working copy de github
dirWeb <- '~/R/spacetimeVis/images/'
figsWeb <- dir(dirWeb, pattern="png$|pdf$|svg$")

dirDropbox <- '~/Dropbox/chapman/book/figs/'
figsDropbox <- dir(dirDropbox, pattern="png$|pdf$|svg$")

idxMatch <- match(figsWeb, figsDropbox, nomatch=0)

filesToCopy <- figsDropbox[idxMatch]
## figsWeb[which(idxMatch==0)]


logCopy <- file.copy(from = paste0(dirDropbox, filesToCopy),
                     to = dirWeb, overwrite=TRUE)
filesToCopy[logCopy] ##exito
filesToCopy[!logCopy]

## ¿He olvidado copiar alguna figura nueva?
figsDropbox[!(figsDropbox %in% figsWeb)]

## mogrify -format png -path thumbs -thumbnail 400x400 *.pdf


## Convierto PDFs a PNGs para thumbnails en web
convertCMD <- function(x, res=72, small=TRUE) {
  name <- strsplit(x, '\\.')[[1]][1]
  if (small) name <- paste(name, 'low', sep='_')
  cmd <- paste('convert', x, '-density', res, '-format png', paste0(name, '.png'))
  system(cmd)
  }
               
pdfWeb <- dir(dirWeb, pattern='pdf$')

old <- setwd(dirWeb)
lapply(pdfWeb, convertCMD)

system('convert airMadrid.png -resize 25% airMadrid_small.png')
system('convert hillShading.png -resize 25% hillShading_small.png')
system('convert airMadrid_krige.png -resize 10% airMadrid_krige_small.png')
system('convert popLandClass.png -resize 25% popLandClass_small.png')

setwd(old)
