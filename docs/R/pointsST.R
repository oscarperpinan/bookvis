##################################################################
## Initial configuration
##################################################################
## Clone or download the repository and set the working directory
## with setwd to the folder where the repository is located.

library(lattice)
library(latticeExtra)

Sys.setlocale("LC_TIME", 'C')

source('configLattice.R')

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
