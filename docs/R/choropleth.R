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

library("RColorBrewer")

source("configLattice.R")
##################################################################

library("sp")

library("sf")

##################################################################
## Read data
##################################################################

sfMapVotes <- st_read("data/Spatial/sfMapVotes.shp")

sfMapVotes$whichMax <- factor(sfMapVotes$whichMax)
sfMapVotes$PROV <- factor(sfMapVotes$PROV)

summary(sfMapVotes)

##################################################################
## Province Boundaries
##################################################################

sfProvs <- st_read("data/Spatial/spain_provinces.shp",
                   crs = 25830)

##################################################################
## Quantitative variable
##################################################################

## Number of intervals (colors)
N <- 6
## Sequential palette
quantPal <- brewer.pal(n = N, "Oranges")

ggplot(sfMapVotes) +
  ## Display the pcMax variable...
  geom_sf(aes(fill = pcMax),
          ## without drawing municipality boundaries
          color = "transparent") +
  scale_fill_gradientn(colours = quantPal) +
  ## And overlay provinces boundaries
  geom_sf(data = sfProvs,
          fill = 'transparent',
          ## but do not include them in the legend
          show.legend = FALSE) +
  theme_bw()

spMapVotes <- as(sfMapVotes, "Spatial")
spProvs <- as(sfProvs, "Spatial")

## Number of cuts
ucN <- 1000
## Palette created with interpolation
ucQuantPal <- colorRampPalette(quantPal)(ucN)

## Province boundaries
provinceLines <- list("sp.polygons",
                      spProvs,
                      lwd = 0.1,
                      # draw the lines after the data
                      first = FALSE)

## Main plot
spplot(spMapVotes["pcMax"],
       col.regions = ucQuantPal,
       cuts = ucN,
       ## Do not draw municipality boundaries
       col = "transparent",
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

library("classInt")

## Compute intervals with the same number of elements
intQuant <- classIntervals(sfMapVotes$pcMax,
                           n = N, style = "quantile")
## Compute intervals with the natural breaks algorithm
intFisher <- classIntervals(sfMapVotes$pcMax,
                            n = N, style = "fisher")

plot(intQuant, pal = quantPal, main = "")

plot(intFisher, pal = quantPal, main = "")

## spplot solution

## Add a new categorical variable with cut, using the computed breaks
spMapVotes$pcMaxInt <- cut(spMapVotes$pcMax,
                           breaks = intFisher$brks,
                           include.lowest = TRUE)

spplot(spMapVotes["pcMaxInt"],
       col = "transparent",
       col.regions = quantPal,
       sp.layout = provinceLines)

## sf and geom_sf
sfMapVotes$pcMaxInt <- cut(sfMapVotes$pcMax,
                           breaks = intFisher$brks,
                           include.lowest = TRUE)

ggplot(sfMapVotes) +
  geom_sf(aes(fill = pcMaxInt),
          color = "transparent") +
  scale_fill_brewer(palette = "Oranges") +
  geom_sf(data = sfProvs,
          fill = "transparent",
          show.legend = FALSE) +
  theme_bw()

##################################################################
## Qualitative variable
##################################################################

classes <- levels(spMapVotes$whichMax)
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
  scale_fill_brewer(palette = "Dark2") +
  geom_sf(data = sfProvs,
          fill = "transparent",
          show.legend = FALSE) +
  theme_bw()

##################################################################
## Small multiples
##################################################################

ggplot(sfMapVotes) +
  geom_sf(aes(fill = pcMaxInt),
          color = "transparent") +
  ## Define the faceting using two rows
  facet_wrap(~whichMax, nrow = 2) +
  scale_fill_brewer(palette = "Oranges") +
  geom_sf(data = sfProvs,
          fill = "transparent",
          size = 0.1,
          show.legend = FALSE) +
  theme_bw()

##################################################################
## Bivariate map
##################################################################

## PP and Cs -> Right
## PSOE and UP -> Left
levels(sfMapVotes$whichMax) <-
  c("ABS", "Right", "OTH", "Right", "Left", "Left")

## Number of steps.
Nint <- 4
## ABS - Greys, Right - Blues, OTH - Greens, Left - Reds
multiPal <- lapply(c("Greys", "Blues", "Greens", "Reds"),
                   function(pal) brewer.pal(Nint, pal))
multiPal <- do.call(rbind, multiPal)

library("biscale")


sfClass <- bi_class(sfMapVotes,
               x = whichMax,
               y = pcMax,
               style = "fisher",
               dim = 4)

bipal <- c(multiPal)

nms <- outer(1:4, 1:4, paste, sep = "-")
names(bipal) <- c(nms)

bilegend <- bi_legend(pal = bipal,
                      dim = 4,
                      xlab = "ABS-Right-OTH-Left",
                      ylab = "% of votes ",
                      size = 8)

bimap <- ggplot() +
  geom_sf(data = sfClass,
          aes(fill = bi_class),
          color = "white",
          size = 0.1,
          show.legend = FALSE) +
  bi_scale_fill(pal = bipal, dim = 4) +
  bi_theme()

library("cowplot")

ggdraw() +
  draw_plot(bimap, 0, 0, 1, 1) +
  draw_plot(bilegend, 0.05, 0.1,
            width = 0.2, height = 0.2)

## Define the intervals
intFisher <- classIntervals(spMapVotes$pcMax,
                            n = Nint, style = "fisher")
## ... and create a categorical variable with them
spMapVotes$pcMaxInt <- cut(spMapVotes$pcMax,
                           breaks = intFisher$brks)

levels(spMapVotes$whichMax) <-
  c("ABS", "Right", "OTH", "Right", "Left", "Left")

classes <- levels(spMapVotes$whichMax)
nClasses <- length(classes)

pList <- lapply(1:nClasses, function(i)
{
  ## Only those polygons corresponding to a level are selected
  mapClass <- subset(spMapVotes,
                     whichMax == classes[i])
  ## Palette
  pal <- multiPal[i, ]
  ## Produce the graphic
  pClass <- spplot(mapClass, "pcMaxInt",
                   col.regions = pal,
                   col = "transparent",
                   colorkey = FALSE)
})
names(pList) <- classes
p <- Reduce("+", pList)

op <- options(digits = 4)
tabFisher <- print(intFisher)
intervals <- names(tabFisher)
options(op)

library("grid")

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
    ## x-axis (quantitative variable)
    Ni <- length(intervals)
    grid.text(intervals,                      
              y = unit(y0 - 1.25 * h/2, "native"),
              x = unit(seq(x0 - w * (Ni -1)/(2*Ni),
                           x0 + w * (Ni -1)/(2*Ni),
                           length = Ni),
                       "native"),
              just = "top",
              rot = 45, 
              gp = gpar(fontsize = 6))
    ## y-axis (qualitative variable)
    grid.text(classes,
              y = unit(seq(y0 + h * (nClasses -1)/(2*nClasses),
                           y0 - h * (nClasses -1)/(2*nClasses),
                           length = nClasses),
                       "native"),
              x = unit(x0 + w/2, "native"),
              just = "left",
              gp = gpar(fontsize = 6))
})

## Main plot
p + legend

##################################################################
## Interactive Graphics
##################################################################

library("mapview")

sfMapVotes0 <- st_read("data/Spatial/sfMapVotes0.shp",
                       crs = 25830)

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
