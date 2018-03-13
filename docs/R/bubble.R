##################################################################
## Initial configuration
##################################################################
## Clone or download the repository and set the working directory
## with setwd to the folder where the repository is located.
 
library(lattice)
library(ggplot2)
## latticeExtra must be loaded after ggplot2 to prevent masking of its
## `layer` function.
library(latticeExtra)

source('configLattice.R')
##################################################################

##################################################################
## Proportional symbol with spplot
##################################################################

library(sp)
library(rgdal)

NO2sp <- readOGR(dsn = 'data/', layer = 'NO2sp')

airPal <- colorRampPalette(c('springgreen1', 'sienna3', 'gray5'))(5)
  
spplot(NO2sp["mean"],
       col.regions = airPal, ## Palette
       cex = sqrt(1:5), ## Size of circles
       edge.col = 'black', ## Color of border
       scales = list(draw = TRUE), ## Draw scales
       key.space = 'right') ## Put legend on the right

##################################################################
## Proportional symbol with ggplot
##################################################################

library(sf)

NO2sf <- st_read(dsn = 'data', layer = 'NO2sp')
## Create a categorical variable
NO2sf$Mean <- cut(NO2sf$mean, 5)

ggplot(data = NO2sf) + 
    geom_sf(aes(size = Mean, fill = Mean),
            pch = 21, col = 'black') +
    scale_fill_manual(values = airPal) +
    theme_bw()

##################################################################
## Optimal classification and sizes to improve discrimination
##################################################################

library(classInt)
## The number of classes is chosen between the Sturges and the
## Scott rules.
nClasses <- 5
intervals <- classIntervals(NO2sp$mean, n = nClasses, style = 'fisher')
## Number of classes is not always the same as the proposed number
nClasses <- length(intervals$brks) - 1

op <- options(digits = 4)
tab <- print(intervals)
options(op)

## Complete Dent set of circle radii (mm)
dent <- c(0.64, 1.14, 1.65, 2.79, 4.32, 6.22, 9.65, 12.95, 15.11)
## Subset for our dataset
dentAQ <- dent[seq_len(nClasses)]
## Link Size and Class: findCols returns the class number of each
## point; cex is the vector of sizes for each data point
idx <- findCols(intervals)
cexNO2 <- dentAQ[idx]

## spplot version
NO2sp$classNO2 <- factor(names(tab)[idx])  

## Definition of an improved key with title and background
NO2key <- list(x = 0.98, y = 0.02, corner = c(1, 0),
               title = expression(NO[2]~~(paste(mu, plain(g))/m^3)),
               cex.title = .75, cex = 0.7,
               background = 'gray92')

pNO2 <- spplot(NO2sp["classNO2"],
               col.regions = airPal,
               cex = dentAQ,
               edge.col = 'black',
               scales = list(draw = TRUE),
               key.space = NO2key)
pNO2

## ggplot2 version
NO2sf$classNO2 <- factor(names(tab)[idx])  

ggplot(data = NO2sf) +
    geom_sf(aes(size = classNO2, fill = classNO2),
            pch = 21, col = 'black') +
    scale_fill_manual(values = airPal) +
    scale_size_manual(values = dentAQ * 2)  +
    xlab("") + ylab("") + theme_bw()

##################################################################
## Spatial context with underlying layers and labels
##################################################################

##################################################################
## Static image
##################################################################

## Bounding box of data
madridBox <- bbox(NO2sp)
## Extend the limits to get a slightly larger map
madridBox <- t(apply(madridBox, 1,
                   extendrange, f = 0.05))

library(ggmap)

madridGG <- get_map(c(madridBox),
                    maptype = 'watercolor',
                    source = 'stamen')

## ggmap with ggplot
NO2df <- as.data.frame(NO2sp)

ggmap(madridGG) +
    geom_point(data = NO2df,
                aes(coords.x1, coords.x2, 
                    size = classNO2,
                    fill = classNO2),
               pch = 21, col = 'black') +
    scale_fill_manual(values = airPal) +
    scale_size_manual(values = dentAQ*2)

## ggmap with spplot
## Project the data into the web mercator projection
NO2merc <- spTransform(NO2sp, CRS("+init=epsg:3857"))

## sp.layout definition
stamen <- list(panel.ggmap, ## Function that displays the object
               madridGG, ## Object to be displayed
               first = TRUE) ## This layout item will be drawn before
                             ## the object displayed by spplot

spplot(NO2merc["classNO2"],
       col.regions = airPal,
       cex = dentAQ,
       edge.col = 'black',
       sp.layout = stamen,
       scales = list(draw = TRUE),
       key.space = NO2key)

##################################################################
## Vector data
##################################################################

##################################################################
## rgdal and spplot
##################################################################

library(rgdal)

## nomecalles http://www.madrid.org/nomecalles/Callejero_madrid.icm
## Form at http://www.madrid.org/nomecalles/DescargaBDTCorte.icm

## Madrid districts
unzip('Distritos de Madrid.zip')
distritosMadrid <- readOGR('Distritos de Madrid/200001331.shp',
                           p4s = '+proj=utm +zone=30')
distritosMadrid <- spTransform(distritosMadrid,
                               CRS = CRS("+proj=longlat +ellps=WGS84"))

## Madrid streets
unzip('Callejero_ Ejes de viales.zip')
streets <- readOGR('Callejero_ Ejes de viales/call2011.shp',
                   p4s = '+proj=utm +zone=30')
streetsMadrid <- streets[streets$CMUN=='079',]
streetsMadrid <- spTransform(streetsMadrid,
                             CRS = CRS("+proj=longlat +ellps=WGS84"))

## Lists using the structure accepted by sp.layout, with the polygons,
## lines, and points, and their graphical parameters
spDistricts <- list('sp.polygons', distritosMadrid,
                    fill = 'gray97', lwd = 0.3)
spStreets <- list('sp.lines', streetsMadrid,
                  lwd = 0.05)
spNames <- list(sp.pointLabel, NO2sp,
                labels = substring(NO2sp$codEst, 7),
                cex = 0.6, fontfamily = 'Palatino')

## spplot with sp.layout version
spplot(NO2sp["classNO2"],
       col.regions = airPal,
       cex = dentAQ,
       edge.col = 'black',
       alpha = 0.8,
       ## Boundaries and labels overlaid
       sp.layout = list(spDistricts, spStreets, spNames),
       scales = list(draw = TRUE),
       key.space = NO2key)

## lattice with layer version
pNO2 +
    ## Labels *over* the original figure
    layer(sp.pointLabel(NO2sp,
                        labels = substring(NO2sp$codEst, 7),
                        cex = 0.8, fontfamily = 'Palatino')
          ) +
    ## Polygons and lines *below* (layer_) the figure
    layer_(
    {
        sp.polygons(distritosMadrid,
                    fill = 'gray97',
                    lwd = 0.3)
        sp.lines(streetsMadrid,
                 lwd = 0.05)
    })

##################################################################
## sf and ggplot
##################################################################

library(sf)

## Madrid districts
distritosMadridSF <- st_read(dsn = 'Distritos de Madrid/',
                           layer = '200001331')
distritosMadridSF <- st_transform(distritosMadridSF,
                               crs = "+proj=longlat +ellps=WGS84")

## Madrid streets
streetsSF <- st_read(dsn = 'Callejero_ Ejes de viales/',
                           layer = 'call2011',
                           crs = '+proj=longlat +ellps=WGS84')

streetsMadridSF <- streetsSF[streetsSF$CMUN=='079',]
streetsMadridSF <- st_transform(streetsMadridSF,
                              crs = "+proj=longlat +ellps=WGS84")

ggplot()+
    ## Layers are drawn sequentially, so the NO2sf layer must be in
    ## the last place to be on top
    geom_sf(data = streetsMadridSF,
            size = 0.05,
            color = 'lightgray') +
    geom_sf(data = distritosMadridSF,
            fill = 'lightgray',
            alpha = 0.2,
            size = 0.3,
            color = 'black') +
    geom_sf(data = NO2sf,
            aes(size = classNO2,
                fill = classNO2),
            pch = 21, col = 'black') + 
    scale_fill_manual(values = airPal) +
    scale_size_manual(values = dentAQ * 2) +
    theme_bw()

##################################################################
## Spatial interpolation
##################################################################

library(gstat)

## Sample 10^5 points locations within the bounding box of NO2sp using
## regular sampling
airGrid <- spsample(NO2sp, type = 'regular', n = 1e5)
## Convert the SpatialPoints object into a SpatialGrid object
gridded(airGrid) <- TRUE
## Compute the IDW interpolation
airKrige <- krige(mean ~ 1, NO2sp, airGrid)

spplot(airKrige["var1.pred"], ## Variable interpolated
       col.regions = colorRampPalette(airPal)) +
    layer({ ## Overlay boundaries and points
        sp.polygons(distritosMadrid, fill = 'transparent', lwd = 0.3)
        sp.lines(streetsMadrid, lwd = 0.07)
        sp.points(NO2sp, pch = 21, alpha = 0.8, fill = 'gray50', col = 'black')
    })

##################################################################
## Interactive graphics
##################################################################

##################################################################
## mapView
##################################################################

library(mapview)

pal <- colorRampPalette(c('springgreen1', 'sienna3', 'gray5'))(100)

mapview(NO2sp,
        zcol = "mean", ## Variable to display
        cex = "mean", ## Use this variable for the circle sizes
        col.regions = pal,
        label = NO2sp$Nombre,
        legend = TRUE)

##################################################################
## Tooltips with images and graphs
##################################################################

img <- paste('images/', NO2sp$codEst, '.jpg', sep = '')

mapview(NO2sp,
        zcol = "mean",
        cex = "mean",
        col.regions = pal, 
        label = NO2sp$Nombre,
        popup = popupImage(img, src = "local"),
        map.type = "Esri.WorldImagery",
        legend = TRUE)

## Read the time series
airQuality <- read.csv2('data/airQuality.csv')
## We need only NO2 data (codParam 8)
NO2 <- subset(airQuality, codParam == 8)
## Time index in a new column
NO2$tt <- with(NO2,
               as.Date(paste(year, month, day, sep = '-')))
## Stations code
stations <- unique(NO2$codEst)
## Loop to create a scatterplot for each station.
pList <- lapply(stations, function(i)
    xyplot(dat ~ tt, data = NO2,
           subset = (codEst == i),
           type = 'l',
           xlab = '', ylab = '')
    )

mapview(NO2sp,
        zcol = "mean",
        cex = "mean",
        col.regions = pal, 
        label = NO2sp$Nombre,
        popup = popupGraph(pList),
        map.type = "Esri.WorldImagery",
        legend = TRUE)

##################################################################
## Synchronise multiple graphics  
##################################################################

## Map of the average value
mapMean <- mapview(NO2sp, zcol = "mean", cex = "mean",
                   col.regions = pal, legend = TRUE,
                   map.types = "OpenStreetMap.Mapnik",
                   label = NO2sp$Nombre)
## Map of the median
mapMedian <- mapview(NO2sp, zcol = "median", cex = "median",
                     col.regions = pal, legend = TRUE,
                     map.type = "Stamen.Watercolor",
                     label = NO2sp$Nombre)
## Map of the standard deviation
mapSD <- mapview(NO2sp, zcol = "sd", cex = "sd",
                 col.regions = pal, legend = TRUE,
                 map.type = "Esri.WorldImagery",
                 label = NO2sp$Nombre)
## All together
sync(mapMean, mapMedian, mapSD, ncol = 3)

##################################################################
## GeoJSON and OpenStreepMap
##################################################################

library(rgdal)
writeOGR(NO2sp, 'data/NO2.geojson', 'NO2sp', driver = 'GeoJSON')

##################################################################
## Keyhole Markup Language
##################################################################

library(rgdal)
writeOGR(NO2sp, dsn = 'NO2_mean.kml', layer = 'mean', driver = 'KML')

library(plotKML)
plotKML(NO2sp["mean"], points_names = NO2sp$codEst)

##################################################################
## 3D visualization
##################################################################

library(rgl)

## rgl does not understand Spatial* objects
NO2df <- as.data.frame(NO2sp)

## Color of each point according to its class
airPal <- colorRampPalette(c('springgreen1', 'sienna3', 'gray5'))(5)
colorClasses <- airPal[NO2df$classNO2]

plot3d(x = NO2df$coords.x1, 
       y = NO2df$coords.x2,
       z = NO2df$alt, 
       xlab = 'Longitude', 
       ylab = 'Latitude', 
       zlab = 'Altitude', 
       type = 's', 
       col = colorClasses,
       radius = NO2df$mean/10)

##################################################################
## gridSVG
##################################################################

library(gridSVG)

print(pNO2 +
      layer_(sp.polygons(distritosMadrid,
                         fill = 'gray97',
                         lwd = 0.3)))

NO2df <- as.data.frame(NO2sp)

tooltips <- sapply(seq_len(nrow(NO2df)), function(i)
{
    codEst <- NO2df[i, "codEst"]
    ## Information to be attached to each line
    stats <- paste(c('Mean', 'Median', 'SD'),
                   signif(NO2df[i, c('mean', 'median', 'sd')], 4),
                   sep = ' = ', collapse = '<br />')
    ## Station photograph 
    imageURL <- paste('images/', codEst, '.jpg', sep = '')
    imageInfo <- paste("<img src=", imageURL,
                       " width = '100' height = '100' />", sep = '')
    ## Text to be included in the tooltip
    nameStation <- paste('<b>', 
                         as.character(NO2df[i, "Nombre"]),
                         '</b>', sep = '')
    info <- paste(nameStation, stats, sep = '<br />')
    ## Tooltip includes the image and the text
    paste(imageInfo, info, sep = '<br />')
})
grid.garnish('points.panel',
             title = tooltips,
             grep = TRUE,
             group = FALSE)

## Webpage of each station
rootURL <- 'http://www.mambiente.munimadrid.es'
urlList <- sapply(seq_len(nrow(NO2df)), function(i){
    codEst <- NO2df[i, "codEst"]
    codURL <- as.numeric(substr(codEst, 7, 8))
    stationURL <- paste(rootURL,
                        '/opencms/opencms/calaire/contenidos/estaciones/estacion',
                        codURL, '.html', sep = '')
})

grid.hyperlink('points.panel', urlList, grep = TRUE, group = FALSE)

## Add jQuery and jQuery UI scripts
grid.script(file = 'http://code.jquery.com/jquery-1.8.3.js')
grid.script(file = 'http://code.jquery.com/ui/1.9.2/jquery-ui.js')
## Simple JavaScript code to initialize the tooltip
grid.script(file = 'js/myTooltip.js')
## Produce the SVG graphic: the results of grid.garnish,
## grid.hyperlink and grid.script are converted to SVG code
grid.export('figs/airMadrid.svg')

htmlBegin <- '<!DOCTYPE html>
<html>
<head>
<title>Tooltips with jQuery and gridSVG</title>
<link rel="stylesheet" type="text/css" href="http://code.jquery.com/ui/1.9.2/themes/smoothness/jquery-ui.css" />
<meta charset="utf-8">
</head>
<body>'

htmlEnd <- '</body> </html>'

svgText <- paste(readLines('figs/airMadrid.svg'), collapse = '\n')

writeLines(paste(htmlBegin, svgText, htmlEnd, sep = '\n'),
           'airMadrid.html')
