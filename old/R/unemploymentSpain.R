##http://hci.stanford.edu/jheer/files/zoo/ex/time/stack.html
##http://www.ine.es/jaxiBD/menu.do?L=0&divi=EPA&his=3&type=db
setwd('~/Dropbox/chapman/book/')
unemploy <- read.csv('data/paroFormacion_INE.csv')

unemploy <- read.zoo('data/paroFormacion_INE.csv', sep=',', header=TRUE,
                     FUN=function(x){
                       x <- as.character(x)
                       ii <- strsplit(x, 'T')
                       yy <- sapply(ii, function(x)as.numeric(x[1]))
                       terms <- c('I', 'II', 'III', 'IV')
                       term <- sapply(ii, function(x)which(x[2] == terms))
                       yq <- as.yearqtr(paste(yy, term, sep='-'))
                     }
                     )

unemploy <- unemploy[,-1] ##drop Total

xyplot(unemploy, superpose=TRUE,
       panel=panel.flow, prepanel=prepanel.default.flow,
       origin='themeRiver', scales=list(y=list(draw=FALSE)),
       par.settings=custom.theme.2, auto.key=FALSE)

xyplot(unemploy, superpose=TRUE,
       panel=panel.flow, prepanel=prepanel.default.flow,
       origin=0,
       par.settings=custom.theme.2, auto.key=FALSE)


## http://www.bls.gov/webapps/legacy/cpsatab14.htm
library(colorspace)
source('R/themeRiver.R')
unemployUSA <- read.csv('data/unemployUSA.csv')
nms <- unemployUSA$Series.ID
annuals <- 14 + 13*(0:12) ##columns of annual summaries
unemployUSA <- as.data.frame(t(unemployUSA[,-c(1, annuals)]))
names(unemployUSA) <- nms

Sys.setlocale("LC_TIME", 'C')
idx <- as.yearmon(row.names(unemployUSA), format='%b.%Y')
unemployUSA <- zoo(unemployUSA, idx)
isNA <- apply(is.na(unemployUSA), 1, any)
unemployUSA <- unemployUSA[!isNA,]

xyplot(unemployUSA, superpose=TRUE,
       panel=panel.flow, prepanel=prepanel.default.flow,
       origin='themeRiver', scales=list(y=list(draw=FALSE)),
       par.settings=custom.theme.2(fill=rainbow_hcl(18, c = 60, l = 75)), auto.key=FALSE)



