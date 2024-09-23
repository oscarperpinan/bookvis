##################################################################
## Initial configuration
##################################################################
## Clone or download the repository and set the working directory
## with setwd to the folder where the repository is located.

Sys.setlocale("LC_TIME", "C")

library("raster")
library("zoo")
library("rasterVis")

SISdm <- brick("data/SpatioTime/SISgal")

timeIndex <- seq(as.Date("2011-01-01"), by = "day", length = 365)
SISdm <- setZ(SISdm, timeIndex)
names(SISdm) <- format(timeIndex, "%a_%Y%m%d")

SISdmt <- rast(SISdm)
time(SISdmt) <- timeIndex

##################################################################
## Levelplot
##################################################################

levelplot(SISdm, layers = 1:12, panel = panel.levelplot.raster)

SISmm <- zApply(SISdm, by = as.yearmon, fun = 'mean')

levelplot(SISmm, panel = panel.levelplot.raster)

##################################################################
## Exploratory graphics
##################################################################

histogram(SISdm, FUN = as.yearmon)

bwplot(SISdm, FUN = as.yearmon)

splom(SISmm, xlab = '', plot.loess = TRUE)

##################################################################
## Space-time and time series plots
##################################################################

hovmoller(SISdm)

xyplot(SISdm, auto.key = list(space = 'right'))

library("RColorBrewer")

horizonplot(SISdm, digits = 1,
            col.regions = rev(brewer.pal(n = 6, 'PuOr')),
            xlab = '', ylab = 'Latitude')

library("cubeview")

## cubeview has problems if the Raster*
## is not stored in memory
SISdm <- readAll(SISdm)

cubeview(SISdm)
