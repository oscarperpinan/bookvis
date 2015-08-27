setwd('~/Dropbox/chapman/book/')

library(hexbin)
prov=28 ##madrid
est=3 ##aranjuez
start='01/01/2004'
end='31/12/2011'
URL = paste("http://www.marm.es/siar/exportador.asp?T=DD&P=", 
        prov, "&E=", est, "&I=", start, "&F=", end, sep = "")

aranjuez <- read.zoo(URL, index.column = 1,
                     tz = "UTC", format = "%d/%m/%Y", 
                     header = TRUE, skip = 1, fill = TRUE,
                     dec = ",", as.is = TRUE)

aranjuezClean <- aranjuez[, c(1, 2, 4, 6, 7, 11, 13, 16, 17, 18)]

names(aranjuezClean) <- c('TempAvg', 'TempMax', 'TempMin',
                          'HumidAvg', 'HumidMax',
                          'WindAvg', 'WindMax',
                          'Rain', 'Radiation', 'ET')

aranjuezClean <- within(as.data.frame(aranjuezClean),{
  TempMin[TempMin>40] <- NA
  HumidMax[HumidMax>100] <- NA
  WindAvg[WindAvg>10] <- NA
  WindMax[WindMax>10] <- NA
})




xyplot(aranjuez, layout=c(1, ncol(aranjuez)), strip=FALSE,
       scales=list(y=list(cex=0.6, rot=0)),
       panel=function(x, y, ...){
         panel.xblocks(x, Year, col = c("lightgray", "white"),
                       border = "darkgray")
         panel.xblocks(x, y<mean(y, na.rm=TRUE), col = "indianred1",
                       height=unit(0.05, 'npc'))
         panel.xyplot(x, y, ...)
         panel.text(x[1], min(y, na.rm=TRUE),
                    names(aranjuez)[panel.number()],
                    cex=0.7, pos=2,...)
       })
  

splom(as.data.frame(aranjuez),
           panel=panel.hexbinplot, xlab='',
           colramp=BTC,##function(n)BTC(n, beg=250, end=5),
           diag.panel = function(x, ...){
             yrng <- current.panel.limits()$ylim
             d <- density(x, na.rm=TRUE)
             d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y))
             panel.lines(d)
             diag.panel.splom(x, ...)
           },
           lower.panel = function(x, y, ...){
             panel.hexbinplot(x, y, ...)
             panel.loess(x, y, ..., col = 'red')
           },
           pscale=0, varname.cex=0.7
           )

### seleccionar
trellis.focus('panel', 1, 1)
idx <- panel.link.splom(pch=13, cex=0.6, col='green')
aranjuez[idx,]




## reshape
aranjuezRshp <- reshape(as.data.frame(aranjuez), direction='long',
                        varying=list(names(aranjuez)[1:3]),
                        v.names='Temperature',
                        times=names(aranjuez)[1:3],
                        timevar='Statistic')

hexbinplot(Radiation~Temperature|Statistic, data=aranjuezRshp,
           layout=c(1, 3), colramp=BTC, 
           panel=function(x, y, xlim,...){
             panel.hexbinplot(x, y, ...)
             panel.loess(x, y, ..., col = 'red')
           }
           )

## water <- reshape(df, direction='long',
##                varying=list(names(df)[c(4, 6, 7)]),
##                v.names='Water',
##                times=names(df)[c(4, 6, 7)],
##                timevar='Statistic')

## hexbinplot(Radiacion~Water|Statistic, data=water,
##            layout=c(1, 3), colramp=BTC, 
##            panel=function(x, y, ...){
##              panel.hexbinplot(x, y, ...)
##              panel.loess(x, y, ..., col = 'red')
##            }
##            )


## añadir leyenda: It is possible but (as far as I know) it’s not
## easy. The hexbin package includes the function hexlegendGrob which
## produces a grob with the legend. With it, you can draw the plot and
## after use update to include the legend. Take a look at the end of
## the code of hexbinplot.formula to see how to get it. The problem is
## that you have to provide the value of maxcnt to hexlegendGrob as an
## argument.  ATENCION: creo que no se puede (o no tiene sentido)
## porque cada panel es independiente y las celdas de uno no tienen
## relación con el resto.

## comb <- expand.grid(i=1:ncol(aranjuez), j=1:ncol(aranjuez))
## mainDiag <- with(comb, i==j)
## comb <- comb[!mainDiag,]

## cnt <- apply(comb, 1, FUN=function(idx){
##   x=aranjuez[,idx[1]]
##   y=aranjuez[,idx[2]]
##   h <- hexbin(x, y)
##   print(idx)
##   print(max(h@count))
##   max(h@count)
## })

## pp <- update(p,
##              legend = lattice:::updateList(p$legend,
##                list(right =
##                     list(fun = hexlegendGrob,
##                          args =
##                          list(maxcnt = maxcnt,
##                                   ## trans = trans,
##                                   ## inv = inv,
##                                   legend = 1)))))
##                                   ## ...)))))



################################################################################
## VARIAS ESTACIONES
################################################################################
SIAR <- read.csv('http://solar.r-forge.r-project.org/data/SIAR.csv')
table(SIAR$Provincia)

prov=31 ##navarra
navarraSIAR <- subset(SIAR, Provincia=='Navarra')
start='01/01/2011'
end='31/12/2011'

navarra <- lapply(navarraSIAR$N_Estacion, FUN=function(i){
  URL = paste("http://www.marm.es/siar/exportador.asp?T=DD&P=", 
    prov, "&E=", i, "&I=", start, "&F=", end, sep = "")
  dat <- try(read.zoo(URL, index.column = 1,
                      tz = "UTC", format = "%d/%m/%Y", 
                      header = TRUE, skip = 1, fill = TRUE,
                      dec = ",", as.is = TRUE))
  if (class(dat)=='try-error') NULL else dat$Radiacion
})

names(navarra) <- make.names(abbreviate(navarraSIAR$Estacion))

err <- sapply(navarra, is.null)
navarra <- do.call(cbind, navarra[!err])

save(navarra, file='data/navarra.RData')

avRad <- zoo(rowMeans(navarra, na.rm=1), index(navarra))

p <- xyplot(navarra - avRad, superpose=TRUE, auto.key=FALSE,
            lwd=0.3, alpha=0.3, col='black') 
p

library(gridSVG)
## grobs in the graphical output
grobs <- grid.ls()
## only interested in some of them
nms <- grobs$name[grobs$type == "grobListing"]
idxNames <- grep('lines', nms)
IDs <- nms[idxNames]

for (id in unique(IDs)){
  ## extract information from the data
  ## according to the ID value
  i <- strsplit(id, '\\.')
  i <- sapply(i, function(x)as.numeric(x[5]))
  dat <- round(mean(navarra[,i]), 2)
  ## Information to be attached to each polygon
  info <- paste(names(navarra)[i], paste(dat, collapse=','),
                sep=':')
  g <- grid.get(id)
  ## attach SVG attributes
  grid.garnish(id,
               onmouseover=paste("showTooltip(evt,'", info, "')"),
               onmouseout="hideTooltip(evt)")
}



grid.script(filename="tooltip.js")

gridToSVG('figs/xyplot.svg')


horizonplot(navarra, layout=c(1, ncol(navarra)),
            origin=mean, colorkey=TRUE)

horizonplot(navarra-avRad, layout=c(1, ncol(navarra)),
            origin=0, scales=list(y=list(relation='same')),
                        colorkey=TRUE)

horizonplot(navarra-avRad, layout=c(1, ncol(navarra)),
            origin=0, colorkey=TRUE)

