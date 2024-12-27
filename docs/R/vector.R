##################################################################
## Initial configuration
##################################################################
## Clone or download the repository and set the working directory
## with setwd to the folder where the repository is located.

library("raster")
library("rasterVis")
library("RColorBrewer")

## Local vector direction 
wDir <- raster('data/Spatial/wDir')/180*pi
## Local vector magnitude
wSpeed <- raster('data/Spatial/wSpeed')
## Vector field encoded in a RasterStack with two layers, magnitude
## and direction
windField <- stack(wSpeed, wDir)
names(windField) <- c('magnitude', 'direction')

##################################################################
## Arrow plot
##################################################################

vectorTheme <- BTCTheme(regions = list(alpha = 0.7))

vectorplot(windField,
           isField = TRUE, ##RasterStack is a vector field
           aspX = 5, aspY = 5, ##Multipliers to adjust the relation
                               ##between slope/aspect and
                               ##horizontal/vertical displacements in
                               ##the figure.
           scaleSlope = FALSE, ## Slope values are *not* scaled
           par.settings = vectorTheme, 
           colorkey = FALSE,
           scales = list(draw = FALSE))

##################################################################
## Streamlines
##################################################################

myTheme <- streamTheme(
    region = rev(brewer.pal(n = 4, "Greys")),
    symbol = rev(brewer.pal(n = 9, "Blues")))

streamplot(windField, isField = TRUE,
           par.settings = myTheme,
           droplet = list(pc = 12), ## Amount of droplets, percentage of cells
           streamlet = list(L = 5, ## Length of the streamlet
                            h = 5), ## Calculation step
           scales = list(draw = FALSE),
           panel = panel.levelplot.raster)
