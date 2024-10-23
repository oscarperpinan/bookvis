##################################################################
## Initial configuration
##################################################################
## Clone or download the repository and set the working directory
## with setwd to the folder where the repository is located.

##################################################################
## Physical maps
##################################################################

library("lattice")
library("ggplot2")
library("latticeExtra")

library("terra")
library("sp")
library("sf")

library("rnaturalearth")
library("rnaturalearthhires")
library("geodata")

library("rasterVis")
library("tidyterra")

library("colorspace")
library("RColorBrewer")

library("ggrepel")

##################################################################
## Retrieving data from DIVA-GIS, GADM and Natural Earth Data
##################################################################

     brazilAdm <- ne_states(country = "brazil")

     brazilDEM <- elevation_30s("BRA", mask = FALSE,
                                path = tempdir())

     worldRiv <- ne_download(type = "rivers_lake_centerlines",
                             category = "physical", 
                             scale = 10)

     ## only those features labeled as "River" are needed
     worldRiv <- worldRiv[worldRiv$featurecla == "River",]

     ## Ensure the CRS of brazilDEM matches the CRS of worldRiv
     crs(brazilDEM) <- crs(worldRiv)

     ## and intersect it with worldRiv to extract brazilian rivers
     ## from the world database
     brazilRiv <- st_crop(worldRiv, brazilDEM)

##################################################################
## Labels
##################################################################

## Locations of labels of each polygon
centroids <- brazilAdm[ , c("longitude", "latitude")]
## Extract the data
centroids <- st_drop_geometry(centroids)
## Location of the "Brazil" label (average of the centroids)
xyBrazil <- apply(centroids, 2, mean)

admNames <- strsplit(as.character(brazilAdm$name), ' ')
  
admNames <- sapply(admNames,
                   FUN = function(s){
                       sep = if (length(s)>2) '\n' else  ' '
                       paste(s, collapse = sep)
                   })

##################################################################
## Overlaying layers of information
##################################################################

terrainTheme <- rasterTheme(region = terrain_hcl(15),
                            panel.background = list(col = "lightskyblue1"))

altPlot <- levelplot(brazilDEM, par.settings = terrainTheme,
                     maxpixels = 1e6, panel = panel.levelplot.raster,
                     margin = FALSE, colorkey = FALSE)

print(altPlot)

ggplot() +
  geom_spatraster(data = brazilDEM,
                  show.legend = FALSE) +
  scale_fill_whitebox_c("high_relief",
                        na.value = "aquamarine") +
  theme_bw()

brazilRivsp <- as_Spatial(brazilRiv)

brazilAdmsp <- as_Spatial(brazilAdm)

## lattice version
altPlot + layer({
  ## Rivers
  sp.lines(brazilRivsp, col = 'darkblue', lwd = 0.2)
  ## Administrative boundaries
  sp.polygons(brazilAdmsp, col = 'black', lwd = 0.2)
  ## Centroids of administrative boundaries ...
  panel.points(centroids, col = 'black')
  ## ... with their labels
  panel.text(centroids, labels = admNames, pos = 3,
             cex = 0.7, fontfamily = 'Palatino', lineheight=.8)
  ## Country name
  panel.text(xyBrazil[1], xyBrazil[2], label = 'B R A Z I L',
             cex = 1.5, fontfamily = 'Palatino', fontface = 2)
  })

## ggplot2 version
ggplot() +
  geom_spatraster(data = brazilDEM,
                  show.legend = FALSE) +
  scale_fill_whitebox_c("high_relief",
                        na.value = "aquamarine") +
  geom_sf(data = brazilAdm,
          col = "black",
          linewidth = 0.15,
          fill = "transparent") +
  geom_sf(data = brazilRiv,
          col = "darkblue",
          linewidth = 0.1) +
  geom_point(data = centroids,
                     aes(x = longitude,
                         y = latitude)) +
  geom_text_repel(data = centroids,
                          aes(x = longitude,
                              y = latitude,
                              label = admNames),
                  family = "Palatino") +
  geom_text(aes(xyBrazil[1], xyBrazil[2],
                label = 'B R A Z I L'),
            size = 7,
            family = 'Palatino') +
  theme_bw()
