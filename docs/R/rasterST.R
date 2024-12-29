##################################################################
## Initial configuration
##################################################################
## Clone or download the repository and set the working directory
## with setwd to the folder where the repository is located.

Sys.setlocale("LC_TIME", "C")

library("raster")
library("terra")

library("zoo")

library("RColorBrewer")
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

horizonplot(SISdm, digits = 1,
            col.regions = rev(brewer.pal(n = 6, 'PuOr')),
            xlab = '', ylab = 'Latitude')

library("cubeview")

## cubeview has problems if the Raster*
## is not stored in memory
SISdm <- readAll(SISdm)

cubeview(SISdm)

##################################################################
## Data
##################################################################

library("raster")
library("rasterVis")

cft <- brick("data/SpatioTime/cft_20130417_0000.nc")
## set projection
projLCC2d <- "+proj=lcc +lon_0=-14.1 +lat_0=34.823 +lat_1=43 +lat_2=43 +x_0=536402.3 +y_0=-18558.61 +units=km +ellps=WGS84"
projection(cft) <- projLCC2d
##set time index
timeIndex <- seq(as.POSIXct("2013-04-17 01:00:00", tz = "UTC"), length = 96, by = "hour")
cft <- setZ(cft, timeIndex)
names(cft) <- format(timeIndex, "D%d_H%H")

##################################################################
## Spatial context: administrative boundaries
##################################################################

library("rnaturalearth")
library("sf")
library("sp")

world <- ne_countries(scale = "medium")
## Project the extent of the cft raster to longitude-latitude, because
## rnaturalearth works with it.
cftLL <- projectExtent(cft, crs(world))
## Crop...
boundaries <- st_crop(world, cftLL)
## ... and project to the projection of the cft object
boundaries <- st_transform(boundaries, crs(cft))
## Finally, convert to a Spatial* object
boundaries <- as(boundaries, "Spatial")

##################################################################
## Producing frames and movie
##################################################################

library("RColorBrewer")
library("latticeExtra")

cloudTheme <- rasterTheme(region = brewer.pal(n = 9, 'Blues'))

tmp <- tempdir()
trellis.device(png, file = paste0(tmp, "/Rplot%02d.png"),
               res = 300, width = 1500, height = 1500)
levelplot(cft, layout = c(1, 1),
          par.settings = cloudTheme,
          scales=list(draw=FALSE)) +
  layer(sp.lines(boundaries, lwd = 0.6))
dev.off()

old <- setwd(tmp)
## Create a movie with ffmpeg ...  
system2("ffmpeg",
        c("-r 6", ## with 6 frames per second
          "-i Rplot%02d.png", ## using the previous files
          "-b:v 300k", ## with a bitrate of 300kbs
          "output.mp4")
        )
file.remove(dir(pattern = "Rplot"))
file.copy("output.mp4", paste0(old, "/figs/SpatioTime/cft.mp4"), overwrite = TRUE)
setwd(old)

##################################################################
## Static image
##################################################################

levelplot(cft,
          layers = 25:48, ## Layers to display (second day)
          layout = c(6, 4), ## Layout of 6 columns and 4 rows
          par.settings = cloudTheme,
          scales=list(draw=FALSE),
          names.attr = paste0(sprintf("%02d", 1:24), "h"),
          panel = panel.levelplot.raster) +
  layer(sp.lines(boundaries, lwd = 0.6))

library("rgl")

clear3d()

pal <- colorRampPalette(brewer.pal(n = 9, "Blues"))

N <- nlayers(cft)

ids <- lapply(seq_len(N),
              FUN = function(i)
                  plot3D(cft[[i]],
                         maxpixels = 1e3,
                         col = pal,
                         adjust = FALSE, ## Disable automatic scaling of xy axes.
                         zfac = 200)) ## Common z scale for all graphics

library("manipulateWidget")

rglwidget() %>%
  playwidget(start = 0, stop = N, 
             subsetControl(1, subsets = ids))
