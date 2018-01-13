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
library(rgdal)

spMapVotes <- readOGR("data/spMapVotes.shp", 
                      p4s = "+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs")

provinces <- readOGR("data/spain_provinces.shp",
                     p4s = "+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs")

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

spplot(spMapVotes["whichMax"],
       col.regions = qualPal,
       col = 'transparent',
       sp.layout = provinceLines)

pList <- lapply(1:nClasses, function(i){
    ## Only those polygons corresponding to a level are selected
    mapClass <- subset(spMapVotes,
                       whichMax == classes[i])
    ## Produce the graphic
    pClass <- spplot(mapClass, "pcMaxInt",
                     col.regions = quantPal,
                     col='transparent',
		     colorkey = FALSE,
		     sp.layout = provinceLines)
})
names(pList) <- classes

do.call(c, pList)

##################################################################
## Categorical and quantitative variables combined in a multivariate choropleth map
##################################################################

multiPal <- sapply(1:nClasses, function(i)
{
    colorAlpha <- adjustcolor(qualPal[i], alpha = 0.4)
    colorRampPalette(c(qualPal[i], colorAlpha), alpha = TRUE)(N)
})

pList <- lapply(1:nClasses, function(i){
    ## Only those polygons corresponding to a level are selected
    mapClass <- subset(spMapVotes,
                       whichMax == classes[i])
    ## Palette
    pal <- multiPal[, i]
    ## Produce the graphic
    pClass <- spplot(mapClass, "pcMaxInt",
                     col.regions = pal,
                     col='transparent',
		     colorkey = FALSE)
})
names(pList) <- classes
p <- Reduce('+', pList)

op <- options(digits = 4)
tabFisher <- print(intFisher)
intervals <- names(tabFisher)
options(op)

blibrary(grid)

legend <- layer(
{
    x0 <- 1000000
    y0 <- 4500000
    w <- 100000
    grid.raster(multiPal, interpolate = FALSE,
                      x = unit(x0, "native"),
                      y = unit(y0, "native"),
                width = unit(w, "native"))
    grid.text(classes,
              x = unit(seq(x0 - w/2,
                           x0 + w/2,
                           length = nClasses),
                       "native"),
              y = unit(y0 + w/2, "native"),
              gp = gpar(fontsize = 4))
    grid.text(intervals,
              x = unit(x0 + w/2, "native"),
              y = unit(seq(y0 - w/2,
                           y0 + w/2,
                           length = length(intervals)),
                       "native"),
              gp = gpar(fontsize = 4))
})

## Main plot
p + legend + 
    ## Provinces boundaries
    layer(sp.polygons(peninsulaLines, lwd = 0.1)) +
    layer(grid.rect(x = bbIslands[1,1], y = bbIslands[2,1],
                    width = diff(bbIslands[1,]),
                    height = diff(bbIslands[2,]),
                    default.units = 'native', just = c('left', 'bottom'),
                    gp = gpar(lwd = 0.5, fill = 'transparent')))

library(sf)

nc <- st_read("data/spMapVotes.shp")
st_crs(nc) <- 25830

ggplot(nc) +
    geom_sf(aes(fill = pcMax),
            color = "transparent") + 
    scale_fill_brewer(palette = "Oranges")

ggplot(nc) +
    geom_sf(aes(fill = whichMax),
            color = "transparent") +
    scale_fill_brewer(palette = "Dark2")

library(mapview)

spMapVotes0 <- readShapePoly(fn = "data/spMapVotes0", 
                        proj4string = CRS("+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs"))

mapView(spMapVotes0, zcol = "whichMax",
        legend = TRUE,
        col.regions = qualPal)

mapView(spMapVotes0, zcol = "pcMax",
        legend = TRUE,
        col.regions = quantPal)
