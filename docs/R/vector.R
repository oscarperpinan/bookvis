##################################################################
## Initial configuration
##################################################################
## Clone or download the repository and set the working directory
## with setwd to the folder where the repository is located.

##################################################################
## Vector fields
##################################################################

library(raster)
library(rasterVis)

wDir <- raster('data/wDir')/180*pi
wSpeed <- raster('data/wSpeed')
windField <- stack(wSpeed, wDir)
names(windField) <- c('magnitude', 'direction')

##################################################################
## Arrow plot
##################################################################

vectorplot(windField, isField = TRUE,
           par.settings = BTCTheme(),
           colorkey = FALSE,
           scales = list(draw = FALSE))

##################################################################
## Streamlines
##################################################################

myTheme <- streamTheme(region = rev(brewer.pal(n = 4, name = 'Greys')),
                       symbol = BTC(n = 9, beg = 20))
streamplot(windField, isField = TRUE,
           par.settings = myTheme,
           droplet = list(pc = 12),
           streamlet = list(L = 5, h = 5),
           scales = list(draw = FALSE),
           panel = panel.levelplot.raster)
