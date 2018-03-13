##################################################################
## Initial configuration
##################################################################
## Clone or download the repository and set the working directory
## with setwd to the folder where the repository is located.

##################################################################
## Retrieving data from OpenStreetMap
##################################################################

library('osmdata')

## Bounding box
ymax <- 43.7031
ymin <- 43.6181
xmax <- -8.0224
xmin <- -8.0808
## Overpass query
cedeira <- opq(c(xmin, ymin, xmax, ymax))

streetsOSM <- add_osm_feature(cedeira,
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

streets <- spFromOSM(cedeira, key = "highway", value = "residential")
primary <- spFromOSM(cedeira, key = "highway", value = "primary")
secondary <- spFromOSM(cedeira, key = "highway", value = "secondary")
tertiary <- spFromOSM(cedeira, key = "highway", value = "tertiary")
unclassified <- spFromOSM(cedeira, key = "highway", value = "unclassified")
footway <- spFromOSM(cedeira, key = "highway", value = "footway")
steps <- spFromOSM(cedeira, key = "highway", value = "steps")

city <- spFromOSM(cedeira, key = "place", value = "town", type = "points")
places <- spFromOSM(cedeira, key = "place", value = "hamlet", type = "points")

nms <- strsplit(as.character(places$name), split = ' \\(')
places$name <- sapply(nms, function(x) x[1])

##################################################################
## Hill Shading
##################################################################

library(raster)
library(rasterVis)

projCedeira <- projection(city)

demCedeira <- raster('data/demCedeira')
projection(demCedeira) <- projCedeira
## Discard values below sea level
demCedeira[demCedeira <= 0] <- NA

slope <- terrain(demCedeira, 'slope')
aspect <- terrain(demCedeira, 'aspect')
hsCedeira <- hillShade(slope = slope, aspect = aspect,
                       angle = 20, direction = 30)

##################################################################
## Overlaying layers of information
##################################################################

## The background color of the panel is set to blue to represent the sea
hsTheme <- GrTheme(panel.background = list(col = 'skyblue3'))

library(colorspace)
## DEM with terrain colors and semitransparency
terrainTheme <- rasterTheme(region = terrain_hcl(n = 15), 
                            regions = list(alpha = 0.6))

##Auxiliary function to display the roads. A thicker black line in
##the background and a thinner one with an appropiate color.
sp.road <- function(line, lwd = 5, blwd = 7,
                    col = 'indianred1', bcol = 'black'){
    sp.lines(line, lwd = blwd, col = bcol)
    sp.lines(line, lwd = lwd, col = col)
}

library(maptools)

## Hill shade and DEM overlaid
levelplot(hsCedeira, maxpixels = ncell(hsCedeira),
          par.settings = hsTheme, margin = FALSE, colorkey = FALSE) +
    levelplot(demCedeira, maxpixels = ncell(demCedeira),
              par.settings = terrainTheme) +
    ## Roads and places
    layer({
        ## Street and roads
        sp.road(streets, lwd = 1, blwd = 2, col = 'white')
        sp.road(unclassified, lwd = 2, blwd = 2, col = 'white')
        sp.road(footway, lwd = 2, blwd = 2, col = 'white')
        sp.road(steps, lwd = 2, blwd = 2, col = 'white')
        sp.road(tertiary, lwd = 3, blwd = 4, col = 'palegreen')
        sp.road(secondary, lwd = 4, blwd = 6, col = 'midnightblue')
        sp.road(primary, col = 'indianred1')
        ## Places except Cedeira town
        sp.points(places, pch = 19, col = 'black', cex = 0.4, alpha = 0.8)
        sp.pointLabel(places, labels = places$name,
                      fontfamily = 'Palatino', 
                      cex = 0.6, col = 'black')
        ## Cedeira town
        sp.points(city, pch = 18, col = 'black', cex = 1)
        sp.pointLabel(city, labels = 'Cedeira',
                      fontfamily = 'Palatino', 
                      cex = 1, col = 'black')
    })
