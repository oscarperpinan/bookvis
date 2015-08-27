
prov=28 ##madrid
est=3 ##aranjuez
start='01/01/2004'
end='31/12/2011'

URL = paste("http://www.marm.es/siar/exportador.asp?T=DD&P=", 
  prov, "&E=", est, "&I=", start, "&F=", end, sep = "")

library(zoo)

aranjuez <- read.zoo(URL, index.column = 1,
                     format = "%d/%m/%Y", 
                     header = TRUE, skip = 1, fill = TRUE,
                     dec = ",", as.is = TRUE)

summary(aranjuez)

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

aranjuez <- zoo(aranjuezClean, index(aranjuez))

summary(aranjuez)

pdf(file="aranjuez.pdf")
xyplot(aranjuez, layout=c(1, ncol(aranjuez)))
dev.off()

pdf(file="aranjuez.xblocks.pdf")
Year <- function(x)format(x, "%Y")

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

dev.off()

splom(as.data.frame(aranjuez))

pdf(file="aranjuez.splom.pdf")
library(hexbin)

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

dev.off()

trellis.focus('panel', 1, 1)
idx <- panel.link.splom(pch=13, cex=0.6, col='green')
aranjuez[idx,]

aranjuezRshp <- reshape(as.data.frame(aranjuez), direction='long',
                        varying=list(names(aranjuez)[1:3]),
                        v.names='Temperature',
                        times=names(aranjuez)[1:3],
                        timevar='Statistic')

head(aranjuezRshp)

pdf(file="aranjuez.hexbinplot.pdf")
hexbinplot(Radiation~Temperature|Statistic, data=aranjuezRshp,
           layout=c(1, 3), colramp=BTC, 
           panel=function(x, y, xlim,...){
             panel.hexbinplot(x, y, ...)
             panel.loess(x, y, ..., col = 'red')
           }
           )
dev.off()
