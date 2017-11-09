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

hovmoller(SISdm, par.settings=BTCTheme())

xyplot(SISdm, digits=1, col='black', lwd=0.2, alpha=0.6)

horizonplot(SISdm, digits = 1,
            col.regions = rev(brewer.pal(n = 6, 'PuOr')),
            xlab = '', ylab = 'Latitude')

##################################################################
## Animation
##################################################################

##################################################################
## Data
##################################################################

cft <- brick('data/cft_20130417_0000.nc')
## use memory instead of file
cft[] <- getValues(cft)
## set projection
projLCC2d <- "+proj=lcc +lon_0=-14.1 +lat_0=34.823 +lat_1=43 +lat_2=43 +x_0=536402.3 +y_0=-18558.61 +units=km +ellps=WGS84"
projection(cft) <- projLCC2d
##set time index
timeIndex <- seq(as.POSIXct('2013-04-17 01:00:00', tz = 'UTC'), length = 96, by = 'hour')
cft <- setZ(cft, timeIndex)
names(cft) <- format(timeIndex, 'D%d_H%H')

##################################################################
## Spatial context: administrative boundaries
##################################################################

library(maptools)
library(rgdal)
library(maps)
library(mapdata)


projLL <- CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
cftLL <- projectExtent(cft, projLL)
cftExt <- as.vector(bbox(cftLL))
boundaries <- map('worldHires',
                  xlim = cftExt[c(1,3)], ylim = cftExt[c(2,4)],
                  plot = FALSE)
boundaries <- map2SpatialLines(boundaries, proj4string = projLL)
boundaries <- spTransform(boundaries, CRS(projLCC2d))

##################################################################
## Producing frames and movie
##################################################################

cloudTheme <- rasterTheme(region = brewer.pal(n = 9, 'Blues'))

tmp <- tempdir()
trellis.device(png, file = paste0(tmp, '/Rplot%02d.png'),
               res = 300, width = 1500, height = 1500)
levelplot(cft, layout = c(1, 1), par.settings = cloudTheme) +
    layer(sp.lines(boundaries, lwd = 0.6))
dev.off()

old <- setwd(tmp)
## Create a movie with ffmpeg ...  
system2('ffmpeg',
        c('-r 6', ## with 6 frames per second
          '-i Rplot%02d.png', ## using the previous files
          '-b:v 300k', ## with a bitrate of 300kbs
          'output.mp4')
        )
file.remove(dir(pattern = 'Rplot'))
file.copy('output.mp4', paste0(old, '/figs/cft.mp4'), overwrite = TRUE)
setwd(old)

##################################################################
## Static image
##################################################################

levelplot(cft, layers = 25:48, layout = c(6, 4),
          par.settings = cloudTheme,
          names.attr = paste0(sprintf('%02d', 1:24), 'h'),
          panel = panel.levelplot.raster) +
    layer(sp.lines(boundaries, lwd = 0.6))

library(rgl)
library(magick)

lat <- seq(-90,90, len=100)*pi/180
long <- seq(-180, 180, len=100)*pi/180

r <- 6378.1 # radius of Earth in km
x <- outer(long, lat, FUN=function(x, y) r*cos(y)*cos(x))
y <- outer(long, lat, FUN=function(x, y) r*cos(y)*sin(x))
z <- outer(long, lat, FUN=function(x, y) r*sin(y))

open3d()
bg3d('black')

## 'http://eoimages.gsfc.nasa.gov/images/imagerecords/79000/79765/dnb_land_ocean_ice.2012.3600x1800_geo.tif'
earth <- surface3d(-x, -z, y,
                   texture="nightLightsHR.png", ## max 8192x8192; sólo PNG!
                   specular="black", col='white')
                 ## normal_x=-x, normal_y=-z, normal_z=y)

writeWebGL('nightLights', width=1000)

library(XML)

geocode <- function(city){
  urlOSM <- paste('http://nominatim.openstreetmap.org/search?city=',
                  city, '&format=xml', sep='')
  xmlOSM <- xmlParse(urlOSM)
  cityOSM <- getNodeSet(xmlOSM, '//place')[[1]] ##sólo cojo el primer resultado
  lon <- xmlGetAttr(cityOSM, 'lon')
  lat <- xmlGetAttr(cityOSM, 'lat')
  as.numeric(c(lon, lat))
  }

cities <- c('Madrid', 'Tokyo', 'Sidney', 'Sao Paulo', 'New York')
points <- t(sapply(cities, geocode))
row.names(points) <- NULL

library(geosphere)

zoomIn <- seq(.3, .1, length=100)
zoomOut <- seq(.1, .3, length=100)

route <- data.frame(lon=points[1,1], lat=points[1,2], zoom=zoomIn, name=cities[1], action='arrive')

for (i in 1:(nrow(points)-1)){
  p1 <- points[i,]
  p2 <- points[i+1,]

  routePart <- gcIntermediate(p1, p2, n=100)
  routePart <- data.frame(routePart)
  routePart$zoom <- .3
  routePart$name <- ''
  routePart$action <- 'travel'

  departure <- data.frame(lon=p1[1], lat=p1[2],  zoom=zoomOut, name=cities[i], action='depart')
  arrival <- data.frame(lon=p2[1], lat=p2[2], zoom=zoomIn, name=cities[i+1], action='arrive')
  routePart <- rbind(departure, routePart, arrival)
  route <- rbind(route, routePart)
  }
route <- rbind(route, data.frame(lon=points[i+1,1], lat=points[i+1,2],
                                 zoom=zoomOut, name=cities[i+1], action='depart'))

travel <- function(tt){
  point <- route[tt,]
  rgl.viewpoint(theta=-90+point$lon, phi=point$lat, zoom=point$zoom)
}

movie3d(travel, duration=nrow(route), startTime=1, fps=1, type='mp4', clean=FALSE)
