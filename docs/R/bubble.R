##################################################################
## Initial configuration
##################################################################
## Clone or download the repository and set the working directory
## with setwd to the folder where the repository is located.
 
library("lattice")
library("ggplot2")
## latticeExtra must be loaded after ggplot2 to prevent masking of its
## `layer` function.
library("latticeExtra")

source("configLattice.R")
##################################################################

library("sf")

NO2sf <- st_read(dsn = "data/Spatial/", layer = "NO2sp")

##################################################################
## Proportional symbol with spplot
##################################################################

library("sp")

NO2sp <- as(NO2sf, "Spatial")

airPal <- colorRampPalette(c("springgreen1", "sienna3", "gray5"))(5)

##################################################################
## Proportional symbol with ggplot2 and spplot
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

library(sp)

NO2sp <- as(NO2sf, "Spatial")

spplot(NO2sp["mean"],
       col.regions = airPal, ## Palette
       cex = sqrt(1:5), ## Size of circles
       edge.col = "black", ## Color of border
       scales = list(draw = TRUE), ## Draw scales
       key.space = "right") ## Put legend on the right

##################################################################
## Proportional symbol with ggplot
##################################################################

## Create a categorical variable
NO2sf$Mean <- cut(NO2sf$mean, 5)

ggplot(data = NO2sf) + 
    geom_sf(aes(size = Mean, fill = Mean),
            pch = 21, col = "black") +
    scale_fill_manual(values = airPal) +
    theme_bw()

##################################################################
## Optimal classification and sizes to improve discrimination
##################################################################

library("classInt")
## The number of classes is chosen between the Sturges and the
## Scott rules.
nClasses <- 5
intervals <- classIntervals(NO2sp$mean, n = nClasses, style = "fisher")
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
NO2key <- list(x = 0.99, y = 0.01, corner = c(1, 0),
               title = expression(NO[2]~~(paste(mu, plain(g))/m^3)),
               cex.title = 0.8, cex = 1,
               background = "gray92")

pNO2 <- spplot(NO2sp["classNO2"],
               col.regions = airPal,
               cex = dentAQ * 0.8,
               edge.col = "black",
               scales = list(draw = TRUE),
               key.space = NO2key)
pNO2

## ggplot2 version
NO2sf$classNO2 <- factor(names(tab)[idx])  

ggplot(data = NO2sf) +
    geom_sf(aes(size = classNO2, fill = classNO2),
            pch = 21, col = "black") +
    scale_fill_manual(values = airPal) +
    scale_size_manual(values = dentAQ * 2)  +
    xlab("") + ylab("") + theme_bw()

##################################################################
## Spatial context with underlying layers and labels
##################################################################

##################################################################
## OpenStreetMap
##################################################################

library("osmdata")

madridBox <- st_bbox(NO2sf)

qosm <- opq(madridBox) %>%
  add_osm_feature(key = "highway",
                  value = "residential")

qsf <- osmdata_sf(qosm)

library(ggrepel)

ggplot()+
  ## Layers are drawn sequentially, so the NO2sf layer must be in
  ## the last place to be on top
  geom_sf(data = qsf$osm_lines["name"],
          size = 0.3,
          color = "lightgray") +
  geom_sf(data = NO2sf,
          aes(size = classNO2,
              fill = classNO2),
          pch = 21, col = "black") +
  ## Labels for each point, with position according to the circle size
  ## and the rest of labels
  geom_text_repel(data = NO2sf,
                  aes(label = substring(codEst, 7),
                      geometry = geometry,
                      point.size = classNO2),
                  stat = "sf_coordinates") + 
  scale_fill_manual(values = airPal) +
  scale_size_manual(values = dentAQ * 2) +
  labs(x = NULL, y = NULL) + theme_bw()

qsp <- osmdata_sp(qosm)

qspLines <- list("sp.lines", qsp$osm_lines["name"],
                 lwd = 0.1)

spplot(NO2sp["classNO2"],
       col.regions = airPal,
       cex = dentAQ,
       edge.col = "black",
       alpha = 0.8,
       sp.layout = qspLines,
       scales = list(draw = TRUE),
       key.space = NO2key)

##################################################################
## Shapefiles
##################################################################

## Madrid districts
unzip("data/Spatial/distr2022.zip", exdir = tempdir())

distritosMadridSF <- st_read(dsn = tempdir(),
                             layer = "dist2022")
## Filter the streets of the Municipality of Madrid
distritosMadridSF <- distritosMadridSF[distritosMadridSF$CMUN == "079",]
## Assign the geographical reference
distritosMadridSF <- st_transform(distritosMadridSF,
                                  crs = "WGS84")

## Madrid streets
unzip("data/Spatial/call2022.zip", exdir = tempdir())

streetsMadridSF <- st_read(dsn = tempdir(),
                           layer = "Gdie_g_calles")                           
streetsMadridSF <- streetsMadridSF[streetsMadridSF$CDMUNI == "079",]
streetsMadridSF <- st_transform(streetsMadridSF,
                                crs = "WGS84")

ggplot()+
  geom_sf(data = streetsMadridSF,
          size = 0.1,
          color = "darkgray") +
  geom_sf(data = distritosMadridSF,
          fill = "lightgray",
          alpha = 0.2,
          size = 0.15,
          color = "black") +
  geom_sf(data = NO2sf,
          aes(size = classNO2,
              fill = classNO2),
          pch = 21, col = "black") + 
  geom_text_repel(data = NO2sf,
                  aes(label = substring(codEst, 7),
                      geometry = geometry,
                      point.size = classNO2),
                  size = 2.5,
                  stat = "sf_coordinates") + 
  scale_fill_manual(values = airPal) +
  scale_size_manual(values = dentAQ * 2) +
  labs(x = NULL, y = NULL) + theme_bw()

distritosMadridSP <- as(distritosMadridSF, "Spatial")
streetsMadridSP <- as(streetsMadridSF, "Spatial")
## Lists using the structure accepted by sp.layout, with the polygons,
## lines, and points, and their graphical parameters
spDistricts <- list("sp.polygons", distritosMadridSP,
                    fill = "gray97", lwd = 0.3)
spStreets <- list("sp.lines", streetsMadridSP,
                  lwd = 0.05)

## spplot with sp.layout version
spplot(NO2sp["classNO2"],
       col.regions = airPal,
       cex = dentAQ,
       edge.col = "black",
       alpha = 0.8,
       sp.layout = list(spDistricts, spStreets),
       scales = list(draw = TRUE),
       key.space = NO2key)

## lattice with layer version
pNO2 +
    ## Polygons and lines *below* (layer_) the figure
    layer_(
    {
        sp.polygons(distritosMadridSP,
                    fill = "gray97",
                    lwd = 0.3)
        sp.lines(streetsMadridSP,
                 lwd = 0.05)
    })

##################################################################
## Spatial interpolation
##################################################################

library("gstat")

## Sample 10^5 points locations within the bounding box of NO2sp using
## regular sampling
airGrid <- spsample(NO2sp, type = "regular", n = 1e5)
## Convert the SpatialPoints object into a SpatialGrid object
gridded(airGrid) <- TRUE
## Compute the IDW interpolation
airKrige <- krige(mean ~ 1, NO2sp, airGrid)

spplot(airKrige["var1.pred"], ## Variable interpolated
       col.regions = colorRampPalette(airPal)) +
  layer({ ## Overlay boundaries and points
    sp.polygons(distritosMadridSP,
                fill = "transparent",
                lwd = 0.3)
    sp.lines(streetsMadridSP,
             lwd = 0.07)
    sp.points(NO2sp,
              pch = 21,
              alpha = 0.8,
              fill = "gray50",
              col = "black")
  })

##################################################################
## Interactive graphics
##################################################################

##################################################################
## mapView
##################################################################

library("mapview")

pal <- colorRampPalette(c("springgreen1", "sienna3", "gray5"))

mapview(NO2sp,
        zcol = "mean", ## Variable to display
        cex = "mean", ## Use this variable for the circle sizes
        col.regions = pal,
        label = NO2sp$Nombre,
        legend = TRUE)

##################################################################
## Tooltips with images and graphs
##################################################################

library("leafpop")

img <- paste("images/Spatial/", NO2sp$codEst, ".jpg", sep = "")

mapview(NO2sp,
        zcol = "mean",
        cex = "mean",
        col.regions = pal, 
        label = NO2sp$Nombre,
        popup = popupImage(img, src = "local", embed = TRUE),
        map.type = "Esri.WorldImagery",
        legend = TRUE)

## Read the time series
airQuality <- read.csv2("data/Spatial/airQuality.csv")
## We need only NO2 data (codParam 8)
NO2 <- subset(airQuality, codParam == 8)
## Time index in a new column
NO2$tt <- with(NO2,
               as.Date(paste(year, month, day, sep = "-")))
## Stations code
stations <- unique(NO2$codEst)
## Loop to create a scatterplot for each station.
pList <- lapply(stations,
                function(i)
                    xyplot(dat ~ tt, data = NO2,
                           subset = (codEst == i),
                           type = "l",
                           xlab = "", ylab = "")
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

library("leafsync")

## Map of the average value
mapMean <- mapview(NO2sp, zcol = "mean", cex = "mean",
                   col.regions = pal, legend = TRUE,
                   map.types = "OpenStreetMap.Mapnik",
                   label = NO2sp$Nombre)

## Map of the median
mapMedian <- mapview(NO2sp, zcol = "median", cex = "median",
                     col.regions = pal, legend = TRUE,
                     #map.type = "NASAGIBS.ViirsEarthAtNight",
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

st_write(NO2sf,
         dsn = "data/Spatial/NO2.geojson",
         layer = "NO2sp",
         driver = "GeoJSON")

##################################################################
## Keyhole Markup Language
##################################################################

st_write(NO2sf,
         dsn = "data/Spatial/NO2_mean.kml",
         layer = "mean",
         driver = "KML")

##################################################################
## 3D visualization
##################################################################

library("rgl")

## rgl does not understand Spatial* objects
NO2df <- as.data.frame(NO2sp)

## Color of each point according to its class
airPal <- colorRampPalette(c("springgreen1", "sienna3", "gray5"))(5)
colorClasses <- airPal[NO2df$classNO2]

plot3d(x = NO2df$coords.x1, 
       y = NO2df$coords.x2,
       z = NO2df$alt, 
       xlab = "Longitude", 
       ylab = "Latitude", 
       zlab = "Altitude", 
       type = "s", 
       col = colorClasses,
       radius = NO2df$mean/10)
