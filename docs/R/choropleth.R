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
## Read data
##################################################################

## sp approach
library(sp)
library(rgdal)

spMapVotes <- readOGR("data/spMapVotes.shp", 
                      p4s = "+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs")

## sf approach
library(sf)

sfMapVotes <- st_read("data/spMapVotes.shp")
st_crs(sfMapVotes) <- 25830

##################################################################
## Province Boundaries
##################################################################

## sp
provinces <- readOGR("data/spain_provinces.shp",
                     p4s = "+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs")

provinceLines <- list("sp.polygons", provinces, lwd = 0.1)

## sf
sfProvs <- st_read("data/spain_provinces.shp")
st_crs(sfProvs) <- 25830

##################################################################
## Quantitative variable
##################################################################

library(RColorBrewer)

## Number of intervals (colors)
N <- 6
## Sequential palette
quantPal <- brewer.pal(n = N, "Oranges")

## Number of cuts
ucN <- 1000
## Palette created with interpolation
ucQuantPal <- colorRampPalette(quantPal)(ucN)

## The polygons boundaries are not displayed thanks to col = 'transparent' 
spplot(spMapVotes["pcMax"],
       col.regions = ucQuantPal,
       cuts = ucN,
       ## Do not draw municipality boundaries
       col = 'transparent',
       ## Overlay province boundaries
       sp.layout = provinceLines)

##################################################################
## Data classification
##################################################################

ggplot(as.data.frame(spMapVotes),
       aes(pcMax,
           fill = whichMax,
           colour = whichMax)) +
    geom_density(alpha = 0.1) +
    theme_bw()

library(classInt)

## Compute intervals with the same number of elements
intQuant <- classIntervals(spMapVotes$pcMax,
                           n = N, style = "quantile")
## Compute intervals with the natural breaks algorithm
intFisher <- classIntervals(spMapVotes$pcMax,
                            n = N, style = "fisher")

plot(intQuant, pal = quantPal, main = "")

plot(intFisher, pal = quantPal, main = "")

## spplot solution

## Add a new categorical variable with cut, using the computed breaks
spMapVotes$pcMaxInt <- cut(spMapVotes$pcMax,
                            breaks = intFisher$brks)

spplot(spMapVotes["pcMaxInt"],
       col = 'transparent',
       col.regions = quantPal,
       sp.layout = provinceLines)

## sf and geom_sf
sfMapVotes$pcMaxInt <- cut(sfMapVotes$pcMax,
                           breaks = intFisher$brks)

ggplot(sfMapVotes) +
    ## Display the pcMaxInt variable...
    geom_sf(aes(fill = pcMaxInt),
            ## without drawing municipality boundaries
            color = "transparent") +
    scale_fill_brewer(palette = "Oranges") +
    ## And overlay provinces boundaries
    geom_sf(data = sfProvs,
            fill = 'transparent',
            ## but do not include them in the legend
            show.legend = FALSE) +
    theme_bw()

##################################################################
## Qualitative variable
##################################################################

classes <- levels(factor(spMapVotes$whichMax))
nClasses <- length(classes)

qualPal <- brewer.pal(nClasses, "Dark2")

## spplot solution
spplot(spMapVotes["whichMax"],
       col.regions = qualPal,
       col = 'transparent',
       sp.layout = provinceLines)

## geom_sf solution
ggplot(sfMapVotes) +
    geom_sf(aes(fill = whichMax),
            color = "transparent") +
    scale_fill_brewer(palette = 'Dark2') +
    geom_sf(data = sfProvs,
            fill = 'transparent',
            show.legend = FALSE) +
    theme_bw()

##################################################################
## Small multiples
##################################################################

## spplot version
spplot(spMapVotes, "pcMaxInt",
       ## Formula to define the faceting: a panel for each level of
       ## whichMax
       formula = pcMaxInt ~ xlabelpoint + ylabelpoint | whichMax,
       col.regions = quantPal,
       col = 'transparent',
       sp.layout = provinceLines)

## ggplot2 version
ggplot(sfMapVotes) +
    geom_sf(aes(fill = pcMaxInt),
            color = "transparent") +
    ## Define the faceting using two rows
    facet_wrap(~whichMax, nrow = 2) +
    scale_fill_brewer(palette = "Oranges") +
    geom_sf(data = sfProvs,
            fill = 'transparent',
            size = 0.1,
            show.legend = FALSE) +
    theme_bw()

##################################################################
## Bivariate map
##################################################################

## Number of intervals.
N <- 3
## Loop to create a bidimensional palette
multiPal <- sapply(1:nClasses, function(i)
{
    colorAlpha <- adjustcolor(qualPal[i], alpha = 0.4)
    colorRampPalette(c(qualPal[i], colorAlpha), alpha = TRUE)(N)
})

## Define the intervals
intFisher <- classIntervals(spMapVotes$pcMax,
                            n = N, style = "fisher")
## ... and create a categorical variable with them
spMapVotes$pcMaxInt <- cut(spMapVotes$pcMax,
                            breaks = intFisher$brks)

pList <- lapply(1:nClasses, function(i){
    ## Only those polygons corresponding to a level are selected
    mapClass <- subset(spMapVotes,
                       whichMax == classes[i])
    ## Palette
    pal <- multiPal[, i]
    ## Produce the graphic
    pClass <- spplot(mapClass, "pcMaxInt",
                     col.regions = pal,
                     col = 'transparent',
		     colorkey = FALSE)
})
names(pList) <- classes
p <- Reduce('+', pList)

op <- options(digits = 4)
tabFisher <- print(intFisher)
intervals <- names(tabFisher)
options(op)

library(grid)

legend <- layer(
{
    ## Position of the legend
    x0 <- 1000000
    y0 <- 4200000
    ## Width of the legend 
    w <- 120000
    ## Height of the legend
    h <- 100000
    ## Colors
    grid.raster(multiPal, interpolate = FALSE,
                      x = unit(x0, "native"),
                      y = unit(y0, "native"),
                width = unit(w, "native"),
                height = unit(h, "native"))
    ## x-axis (qualitative variable)
    grid.text(classes,
              x = unit(seq(x0 - w * (nClasses -1)/(2*nClasses),
                           x0 + w * (nClasses -1)/(2*nClasses),
                           length = nClasses),
                       "native"),
              y = unit(y0 + h/2, "native"),
              just = "bottom",
              rot = 10,
              gp = gpar(fontsize = 4))
    ## y-axis (quantitative variable)
    Ni <- length(intervals)
    grid.text(intervals,
              x = unit(x0 + w/2, "native"),
              y = unit(seq(y0 - h * (Ni -1)/(2*Ni),
                           y0 + h * (Ni -1)/(2*Ni),
                           length = Ni),
                       "native"),
              just = "left",
              gp = gpar(fontsize = 6))
})

## Main plot
p + legend

##################################################################
## Interactive Graphics
##################################################################

library(mapview)

sfMapVotes0 <- st_read("data/spMapVotes0.shp")
st_crs(sfMapVotes0) <- 25830

## Quantitative variable, pcMax
mapView(sfMapVotes0,
        zcol = "pcMax", ## Choose the variable to display
        legend = TRUE,
        col.regions = quantPal)

## Qualitative variable, whichMax
mapView(sfMapVotes0,
        zcol = "whichMax",
        legend = TRUE,
        col.regions = qualPal)
