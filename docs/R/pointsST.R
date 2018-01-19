##################################################################
## Initial configuration
##################################################################
## Clone or download the repository and set the working directory
## with setwd to the folder where the repository is located.

library(lattice)
library(latticeExtra)

myTheme <- custom.theme.2(pch=19, cex=0.7,
                          region = rev(brewer.pal(9, 'YlOrRd')),
                          symbol = brewer.pal(n=8, name = "Dark2"))
myTheme$strip.background$col = 'transparent'
myTheme$strip.shingle$col = 'transparent'
myTheme$strip.border$col = 'transparent'

xscale.components.custom <- function(...){
    ans <- xscale.components.default(...)
    ans$top = FALSE
    ans}
yscale.components.custom <- function(...){
    ans <- yscale.components.default(...)
      ans$right = FALSE
    ans}
myArgs <- list(as.table=TRUE,
               between=list(x=0.5, y=0.2),
               xscale.components = xscale.components.custom,
               yscale.components = yscale.components.custom)
defaultArgs <- lattice.options()$default.args

lattice.options(default.theme = myTheme,
                default.args = modifyList(defaultArgs, myArgs))

##################################################################
## Data and spatial information
##################################################################

library(sp)
  
## Spatial location of stations
airStations <- read.csv2('data/airStations.csv')
## rownames are used as the ID of the Spatial object
rownames(airStations) <- substring(airStations$Codigo, 7)
coordinates(airStations) <- ~ long + lat
proj4string(airStations) <- CRS("+proj=longlat +ellps=WGS84")
## Measurements data
airQuality <- read.csv2('data/airQuality.csv')
## Only interested in NO2 
NO2 <- airQuality[airQuality$codParam == 8, ]

library(zoo)
library(reshape2)
library(spacetime)
  
NO2$time <- as.Date(with(NO2,
                         ISOdate(year, month, day)))
NO2wide <- dcast(NO2[, c('codEst', 'dat', 'time')],
                 time ~ codEst,
                 value.var = "dat")
NO2zoo <- zoo(NO2wide[,-1], NO2wide$time)

dats <- data.frame(vals = as.vector(t(NO2zoo)))
NO2st <- STFDF(sp = airStations,
               time = index(NO2zoo),
               data = dats)

##################################################################
## Graphics with spacetime
##################################################################

airPal <- colorRampPalette(c('springgreen1', 'sienna3', 'gray5'))(5)
  
stplot(NO2st[, 1:12],
       cuts = 5,
       col.regions = airPal,
       main = '',
       edge.col = 'black')

stplot(NO2st, mode='xt',
       col.regions = colorRampPalette(airPal)(15),
       scales = list(x = list(rot = 45)),
       ylab='', xlab='', main = '')

stplot(NO2st, mode = 'ts', xlab = '',
       lwd = 0.1, col = 'black', alpha = 0.6,
       auto.key = FALSE)
