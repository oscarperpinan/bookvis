##################################################################
## Initial configuration
##################################################################
## Clone or download the repository and set the working directory
## with setwd to the folder where the repository is located.

Sys.setlocale("LC_TIME", 'C')

##################################################################
## CMSAF Data
##################################################################

library(raster)
library(zoo)
library(rasterVis)

SISdm <- brick('data/SISgal')

timeIndex <- seq(as.Date('2011-01-01'), by = 'day', length = 365)
SISdm <- setZ(SISdm, timeIndex)
names(SISdm) <- format(timeIndex, '%a_%Y%m%d')

##################################################################
## Levelplot
##################################################################

levelplot(SISdm, layers=1:12, panel=panel.levelplot.raster)

SISmm <- zApply(SISdm, by=as.yearmon, fun='mean')

levelplot(SISmm, panel=panel.levelplot.raster)

##################################################################
## Exploratory graphics
##################################################################

histogram(SISdm, FUN=as.yearmon)

bwplot(SISdm, FUN=as.yearmon)

splom(SISmm, xlab='', plot.loess=TRUE)

##################################################################
## Space-time and time series plots
##################################################################

hovmoller(SISdm)

xyplot(SISdm, digits=1, col='black', lwd=0.2, alpha=0.6)

horizonplot(SISdm, digits = 1,
            col.regions = rev(brewer.pal(n = 6, 'PuOr')),
            xlab = '', ylab = 'Latitude')

library(mapview)

cubeView(SISdm)
