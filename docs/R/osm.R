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

library("RColorBrewer")
library("colorspace")

library("raster")
library("terra")

library("rasterVis")
library("tidyterra")

##################################################################
## Retrieving data from OpenStreetMap
##################################################################

library("osmdata")
library("sf")
library("sp")


## Bounding box
xmin <- -8.1
ymin <- 43.62
xmax <- -8
ymax <- 43.7 
cedeiraBB <- c(xmin = xmin, ymin = ymin,
               xmax = xmax, ymax = ymax) 

## Overpass query
cedeiraOPQ <- opq(cedeiraBB)

streetsOSM <- add_osm_feature(cedeiraOPQ,
                              key = "highway",
                              value = "residential")

streetsSP <- osmdata_sp(streetsOSM)

print(streetsSP)

spFromOSM <- function(source, key, value, type = 'lines')
{
    osm <- add_osm_feature(source, key, value)
    spdata <- osmdata_sp(osm)
    switch(type,
           lines = spdata$osm_lines,
           points = spdata$osm_points)
}

sfFromOSM <- function(bb, key, value, type = 'lines')
{
  source <- opq(bb)
  osm <- add_osm_feature(source, key, value)
  sfdata <- osmdata_sf(osm)
  switch(type,
         lines = st_crop(sfdata$osm_lines, bb),
         points = sfdata$osm_points)
  
}

streetsSP <- spFromOSM(cedeiraOPQ,
                       key = "highway", value = "residential")
primarySP <- spFromOSM(cedeiraOPQ,
                       key = "highway", value = "primary")
secondarySP <- spFromOSM(cedeiraOPQ,
                         key = "highway", value = "secondary")
tertiarySP <- spFromOSM(cedeiraOPQ,
                        key = "highway", value = "tertiary")
unclassifiedSP <- spFromOSM(cedeiraOPQ,
                            key = "highway", value = "unclassified")
footwaySP <- spFromOSM(cedeiraOPQ,
                       key = "highway", value = "footway")
stepsSP <- spFromOSM(cedeiraOPQ,
                     key = "highway", value = "steps")

streetsSF <- sfFromOSM(cedeiraBB,
                       key = "highway", value = "residential")
primarySF <- sfFromOSM(cedeiraBB,
                       key = "highway", value = "primary")
secondarySF <- sfFromOSM(cedeiraBB,
                         key = "highway", value = "secondary")
tertiarySF <- sfFromOSM(cedeiraBB,
                        key = "highway", value = "tertiary")
unclassifiedSF <- sfFromOSM(cedeiraBB,
                            key = "highway", value = "unclassified")
footwaySF <- sfFromOSM(cedeiraBB,
                       key = "highway", value = "footway")
stepsSF <- sfFromOSM(cedeiraBB,
                     key = "highway", value = "steps")

citySP <- spFromOSM(cedeiraOPQ, key = "place",
                    value = "town", type = "points")

placesHsp <- spFromOSM(cedeiraOPQ, key = "place",
                       value = "hamlet", type = "points")
placesHsp <- subset(placesHsp, as.numeric(population) > 30)

citySF <- sfFromOSM(cedeiraBB, key = "place",
                    value = "town", type = "points")

placesHsf <- sfFromOSM(cedeiraBB, key = "place",
                       value = "hamlet", type = "points")
placesHsf <- subset(placesHsf, as.numeric(population) > 30)

##################################################################
## Hill Shading
##################################################################

library("raster")
projCedeira <- projection(citySP)

demCedeira <- raster('data/Spatial/demCedeira')
projection(demCedeira) <- projCedeira

## Crop the DEM using the bounding box of the OSM data
OSMextent <- extent(extendrange(c(xmin, xmax)),
                    extendrange(c(ymin, ymax)))
demCedeira <- crop(demCedeira, OSMextent)

## Discard values below sea level
demCedeira[demCedeira <= 0] <- NA

slope <- terrain(demCedeira, 'slope')
aspect <- terrain(demCedeira, 'aspect')
hsCedeira <- hillShade(slope = slope, aspect = aspect,
                       angle = 20, direction = 30)

library("terra")

demCedeiraT <- rast(demCedeira)
hsCedeiraT <- rast(hsCedeira)

##################################################################
## Overlaying layers of information
##################################################################

## The background color of the panel is set to blue to represent the
## sea
hsTheme <- GrTheme(panel.background = list(col = "skyblue3"))

hsLattice <- levelplot(hsCedeira, maxpixels = ncell(hsCedeira),
                       par.settings = hsTheme,
                       margin = FALSE, colorkey = FALSE,
                       xlab = "", ylab = "")

greyPal <- rev(brewer.pal(n = 9, name = "Greys"))

ggplot() +
  geom_spatraster(data = hsCedeiraT,
                  show.legend = FALSE) +
  scale_fill_gradientn(colours = greyPal,
                       na.value = "skyblue3") +
  theme_bw()

## Build a vector of greys based on the Color Brewer pal
greyRamp <- colorRampPalette(greyPal)
greys <- greyRamp(255) ##255 colors, 0 to 254

## Classify the values of the raster in 255 classes
idx <- classify(hsCedeiraT, 255, ## 255 cuts
                include.lowest = TRUE) 
idx <- as.vector(idx)
## Map these classes to the vector of colors
palGreys <- greys[idx + 1] ## 1:255 for indexing

hsGGplot <- geom_spatraster(data = hsCedeiraT,
                  fill = palGreys,
                  ## Plot every cell of the raster
                  maxcell = Inf)

## DEM with terrain colors and semitransparency
terrainPal <- terrain_hcl(n = 15)

## Lattice version
terrainTheme <- rasterTheme(region = terrainPal, 
                            regions = list(alpha = 0.6))

demLattice <- levelplot(demCedeira, maxpixels = ncell(demCedeira),
                        par.settings = terrainTheme,
                        margin = FALSE, colorkey = FALSE)

print(demLattice)

## ggplot version
demGGplot <- geom_spatraster(data = demCedeiraT,
                             alpha = 0.6,
                             show.legend = FALSE)
terrainScale <- scale_fill_gradientn(colours = terrainPal,
                                     na.value = "skyblue3")

print(ggplot() + demGGplot + terrainScale)

## Auxiliary function to display the roads.

## A thicker black line in the background and a
## thinner one with an appropiate color.

## sp version
sp.road <- function(line, lwd = 6, blwd = 7,
                    col = "indianred1", bcol = "black") {
  sp.lines(line, lwd = blwd, col = bcol)
  sp.lines(line, lwd = lwd, col = col)
}

## sf version
sf_road <- function(line, lwd = 1, blwd = 1.1,
                    col = "indianred1", bcol = "black") {
  list(
    geom_sf(data = line, linewidth = blwd, col = bcol), 
    geom_sf(data = line, linewidth = lwd, col = col)
  )
}

##Auxiliary function to display the towns and villages. 

## sp version
sp.places <- function(places, point.size= 0.4, text.size = 0.8) {
  sp.points(places, pch = 19, col = "black",
            cex = point.size, alpha = 0.8)
  sp.text(coordinates(places), places$name,
          pos = 3,
          fontfamily = "Palatino", 
          cex = text.size, col = "black")
}

## sf version
sf_places <- function(places, text_size, point_size, vjust = -1)
{
  list(
    geom_sf(data = places, size = point_size), 
    geom_sf_text(aes(label = name), data = places,
                 size = text_size, vjust = vjust)
  )
}

## Hill shade and DEM overlaid
hsLattice +
  demLattice +
  ## Roads and places
  layer({
    ## Street and roads
    sp.road(streetsSP, lwd = 1, blwd = 1, col = "white")
    sp.road(unclassifiedSP, lwd = 2, blwd = 2, col = "white")
    sp.road(footwaySP, lwd = 2, blwd = 2, col = "white")
    sp.road(stepsSP, lwd = 2, blwd = 2, col = "white")
    sp.road(tertiarySP, lwd = 4, blwd = 4, col = "palegreen")
    sp.road(secondarySP, lwd = 6, blwd = 6, col = "midnightblue")
    sp.road(primarySP, lwd = 7, blwd = 8, col = "indianred1")
    ## Places except Cedeira town
    sp.places(placesHsp, point.size = 0.4, text.size = 0.8)
    ## Cedeira town
    sp.places(citySP, point.size = 1.2, text.size = 1.5)
  })

ggplot() +
  hsGGplot +
  demGGplot + terrainScale +
  ## Street and roads
  sf_road(streetsSF, lwd = .4, blwd = .5, col = "white") +
  sf_road(unclassifiedSF, lwd = .4, blwd = .5, col = "white") +
  sf_road(footwaySF, lwd = .4, blwd = .5, col = "white") +
  sf_road(stepsSF, lwd = .4, blwd = .5, col = "white") +
  sf_road(tertiarySF, lwd = .8, blwd = .9, col = "palegreen") +
  sf_road(secondarySF, lwd = .9, blwd = 1, col = "midnightblue") +
  sf_road(primarySF, lwd = 1.1, blwd = 1.2, col = "indianred1") +
  ## Places
  sf_places(placesHsf, point_size = 1, text_size = 3) +
  sf_places(citySF, point_size = 3, text_size = 5) +
  theme_bw() + xlab("") + ylab("")
