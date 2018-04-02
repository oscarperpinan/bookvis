old <- setwd('/tmp/')

files <- dir(pattern='.pdf')

## Cuidado. Algunos gráficos (los que tienen muchos puntos
## (xyplotSISav.pdf) se hacen exageradamente grandes después de
## hacer este paso)
lapply(files, function(f){
  origFile <- strsplit(f, '\\.')[[1]][1]
  destfile <- paste(origFile, 'Fonts.pdf', sep='')
  embedFonts(f, outfile=destfile, options='-dPDFSETTINGS=/prepress')
  })

##Compruebo resultado
filesFont <- dir(pattern='Fonts.pdf')
lapply(filesFont, function(f){
  cmd <- paste('pdffonts', f)
  print(f)
  system(cmd)
  })

setwd(old)
