##################################################################
## Initial configuration
##################################################################
## Clone or download the repository and set the working directory
## with setwd to the folder where the repository is located.
  
library(lattice)
library(ggplot2)
library(latticeExtra)
  
myTheme <- custom.theme.2(pch = 19, cex = 0.7,
                          region = rev(brewer.pal(9, 'YlOrRd')),
                          symbol = brewer.pal(n = 8, name = "Dark2"))
myTheme$strip.background$col='transparent'
myTheme$strip.shingle$col='transparent'
myTheme$strip.border$col='transparent'

xscale.components.custom <- function(...)
{
    ans <- xscale.components.default(...)
    ans$top = FALSE
    ans
}

yscale.components.custom <- function(...)
{
    ans <- yscale.components.default(...)
    ans$right = FALSE
    ans
}

myArgs <- list(as.table = TRUE,
               between = list(x = 0.5, y = 0.2),
               xscale.components = xscale.components.custom,
               yscale.components = yscale.components.custom)

defaultArgs <- lattice.options()$default.args


lattice.options(default.theme = myTheme,
                default.args = modifyList(defaultArgs, myArgs))

##################################################################
## Choropleth maps
##################################################################

library(sp)
library(maptools)

spMapVotes <- readShapePoly(fn = "data/spMapVotes", 
                        proj4string = CRS("+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs"))

provinces <- readShapePoly(fn="data/spain_provinces",
                        proj4string = CRS("+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs"))

provinceLines <- list("sp.polygons", provinces, lwd = 0.1)

library(RColorBrewer)

## Number of intervals (colors)
N <- 6
## Sequential palette
quantPal <- brewer.pal(n = N, "Oranges")

ucN <- 1000
ucQuantPal <- colorRampPalette(quantPal)(ucN)

## The polygons boundaries are not displayed thanks to col = 'transparent' 
spplot(spMapVotes["pcMax"],
       col.regions = ucQuantPal,
       cuts = ucN,
       col = 'transparent',
       sp.layout = provinceLines)

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

## Add a new categorical variable with cut, using the computed breaks
spMapVotes$pcMaxInt <- cut(spMapVotes$pcMax,
                            breaks = intFisher$brks)

spplot(spMapVotes["pcMaxInt"],
       col = 'transparent',
       col.regions = quantPal,
       sp.layout = provinceLines)

classes <- levels(factor(spMapVotes$whichMax))
nClasses <- length(classes)

qualPal <- brewer.pal(nClasses, "Dark2")

spplot(spMapVotes[1:100, "whichMax"],
       col.regions = qualPal,
       col = 'transparent',
       sp.layout = provinceLines)

pList <- lapply(1:nClasses, function(i){
    ## Only those polygons corresponding to a level are selected
    mapClass <- subset(spMapVotes, 
                       whichMax == classes[i])
    ## Use natural breaks classification
    mapClass$pcMaxInt <- cut(mapClass$pcMax,
                             breaks = intFisher$brks)
    ## Produce the graphic
    pClass <- spplot(mapClass, "pcMaxInt",
                     col.regions = quantPal,
                     col='transparent',
		     sp.layout = provinceLines)
})
names(pList) <- classes

do.call(c, pList)

library(mapview)

mapView(spMapVotes0, zcol = "whichMax",
        legend = TRUE,
        col.regions = quantPal)

mapView(spMapVotes0, zcol = "pcMax",
        legend = TRUE,
        col.regions = qualPal)
