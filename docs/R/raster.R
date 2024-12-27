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
## Quantitative data
##################################################################

library("raster")
library("terra")
library("sf")

library("rnaturalearth")
library("geodata")

library("viridisLite")

library("rasterVis")

SISavr <- raster("data/Spatial/SISav.nc")

SISavt <- rast("data/Spatial/SISav.nc")

levelplot(SISavr)

boundarySF <- ne_countries(country = "spain", scale = 50)
## Crop to the limits of the raster object
boundarySF <- st_crop(boundarySF,
                      xmin = xmin(SISavt), ymin = ymin(SISavt),
                      xmax = xmax(SISavt), ymax = ymax(SISavt))

##ggplot2 version
gplot(SISavt) +
  geom_sf(data = boundarySF, fill = "transparent")

## lattice version
## Convert the sf object to sp
boundarySP <- as(boundarySF, "Spatial")

## Display the data ...
levelplot(SISavt) +
  ## ... and overlay the SpatialLines object
  layer(sp.lines(boundarySP,
                 lwd = 0.5))

##################################################################
## Hill shading
##################################################################

DEM <- elevation_30s("ESP", path = tempdir())

slope <- terrain(DEM, "slope", unit = "radians")
aspect <- terrain(DEM, "aspect", unit = "radians")
hs <- shade(slope = slope, aspect = aspect,
            angle = 60, direction = 45)

DEMr <- raster(DEM)
sloper <- terrain(DEMr, "slope")
aspectr <- terrain(DEMr, "aspect")
hsr <- hillShade(slope = sloper, aspect = aspectr,
                 angle = 60, direction = 45)

## hillShade theme: gray colors and semitransparency
hsTheme <- GrTheme(regions = list(alpha = 0.5))

levelplot(SISavt,
          par.settings = YlOrRdTheme,
          margin = FALSE, colorkey = FALSE) +
  ## Overlay the hill shade raster
  levelplot(hs, par.settings = hsTheme, maxpixels = 1e6) +
  ## and the countries boundaries
  layer(sp.lines(boundarySP, lwd = 0.5))

##################################################################
## Diverging palettes
##################################################################

meanRad <- global(SISavt, "mean")
meanRad <- as.numeric(meanRad)
SISavt <- SISavt - meanRad

meanRad <- cellStats(SISavr, "mean")
SISavr <- SISavr - meanRad

xyplot(layer ~ y, data = SISavt,
       groups = cut(x, 5),
       par.settings = rasterTheme(symbol = magma(n = 5,
                                                 begin = 0, end = 0.9,
                                                 direction = -1)),
       xlab = "Latitude", ylab = "Solar radiation (scaled)",  
       auto.key = list(space = "right",
                       title = "Longitude",
                       cex.title = 1.3))

divPal <- brewer.pal(n = 9, "PuOr")
divPal[5] <- "#FFFFFF"

showPal <- function(pal)
{
    N <- length(pal)
    image(1:N, 1, as.matrix(1:N), col = pal,
          xlab = "", ylab = "",
          xaxt = "n", yaxt = "n",
          bty = "n")
}

showPal(divPal)

divTheme <- rasterTheme(region = divPal)

levelplot(SISavt, contour = TRUE, par.settings = divTheme)

rng <- range(SISavt[])
## Number of desired intervals
nInt <- 15
## Increment corresponding to the range and nInt
inc0 <- diff(rng)/nInt
## Number of intervals from the negative extreme to zero
n0 <- floor(abs(rng[1])/inc0)
## Update the increment adding 1/2 to position zero in the center of an interval
inc <- abs(rng[1])/(n0 + 1/2)
## Number of intervals from zero to the positive extreme
n1 <- ceiling((rng[2]/inc - 1/2) + 1)
## Collection of breaks
breaks <- seq(rng[1], by = inc, length= n0 + 1 + n1)

## Midpoints computed with the median of each interval
idx <- findInterval(SISavt[], breaks, rightmost.closed = TRUE)
mids <- tapply(SISavt[], idx, median)
## Maximum of the absolute value both limits
mx <- max(abs(breaks))

break2pal <- function(x, mx, pal){
    ## x = mx gives y = 1
    ## x = 0 gives y = 0.5
    y <- 1/2*(x/mx + 1)
    rgb(pal(y), maxColorValue = 255)
}

## Interpolating function that maps colors with [0, 1]
## rgb(divRamp(0.5), maxColorValue=255) gives "#FFFFFF" (white)
divRamp <- colorRamp(divPal)
## Diverging palette where white is associated with the interval
## containing the zero
pal <- break2pal(mids, mx, divRamp)
showPal(pal)

levelplot(SISavt,
          par.settings = rasterTheme(region = pal),
          at = breaks, contour = TRUE)

divTheme <- rasterTheme(regions = list(col = pal))

levelplot(SISavt,
          par.settings = divTheme,
          at = breaks,
          contour = TRUE)

cl <- classIntervals(SISavt[], style = "kmeans")
breaks <- cl$brks

## Repeat the procedure previously exposed, using the 'breaks' vector
## computed with classIntervals
idx <- findInterval(SISavt[], breaks, rightmost.closed = TRUE)
mids <- tapply(SISavt[], idx, median)

mx <- max(abs(breaks))
pal <- break2pal(mids, mx, divRamp)

## Modify the vector of colors in the 'divTheme' object
divTheme$regions$col <- pal

levelplot(SISavt,
          par.settings = divTheme,
          at = breaks,
          contour = TRUE)

##################################################################
## Categorical data
##################################################################

## raster
myExtR <- extent(65, 135, 5, 55)

popR <- raster("data/Spatial/875430rgb-167772161.0.FLOAT.TIFF")
popR <- crop(popR, myExtR)
popR[popR==99999] <- NA

landClassR <- raster("data/Spatial/241243rgb-167772161.0.TIFF")
landClassR <- crop(landClassR, myExtR)

## terra
myExtT <- ext(65, 135, 5, 55)

popT <- rast("data/Spatial/875430rgb-167772161.0.FLOAT.TIFF")
names(popT) <- "population"
popT <- crop(popT, myExtT)
popT[popT==99999] <- NA

landClassT <- rast("data/Spatial/241243rgb-167772161.0.TIFF")
names(landClassT) <- "landClass"
landClassT <- crop(landClassT, myExtT)

landClassR[landClassR %in% c(0, 254)] <- NA
## Only four groups are needed:
## Forests: 1:5
## Shrublands, etc: 6:11
## Agricultural/Urban: 12:14
## Snow: 15:16
landClassR <- cut(landClassR, c(0, 5, 11, 14, 16))
## Add a Raster Attribute Table and define the raster as categorical data
landClassR <- ratify(landClassR)
## Configure the RAT: first create a RAT data.frame using the
## levels method; second, set the values for each class (to be
## used by levelplot); third, assign this RAT to the raster
## using again levels
rat <- levels(landClassR)[[1]]
rat$classes <- c("Forest", "Land", "Urban", "Snow")
levels(landClassR) <- rat

landClassT[landClassT %in% c(0, 254)] <- NA
landClassT <- classify(landClassT, c(0, 5, 11, 14, 16))

rat <- levels(landClassT)[[1]]
names(rat) <- c("ID", "classes")
rat$classes <- c("Forest", "Land", "Urban", "Snow")
levels(landClassT) <- rat

qualPal <- c("palegreen4", # Forest
             "lightgoldenrod", # Land
             "indianred4", # Urban
             "snow3")      # Snow

qualTheme <- rasterTheme(region = qualPal,
                         panel.background = list(col = "lightskyblue1")
                         )

levelplot(landClassT, maxpixels = 3.5e5,
          par.settings = qualTheme)

pPop <- levelplot(popT, zscaleLog = 10,
                  par.settings = BTCTheme,
                  maxpixels = 3.5e5)
pPop

## Join the RasterLayer objects to create a RasterStack object.
s <- stack(popR, landClassR)
names(s) <- c("pop", "landClass")

## Join the SpatRaster objects to create a multilayer object.
st <- c(popT, landClassT)
names(st) <- c("pop", "landClass")

densityplot(~log10(pop), ## Represent the population
            groups = landClass, ## grouping by land classes
            data = s,
            ## Do not plot points below the curves
            plot.points = FALSE)

##################################################################
## Bivariate legend
##################################################################

classes <- rat$classes
nClasses <- length(classes)

logPopAt <- c(0, 0.5, 1.85, 4)

nIntervals <- length(logPopAt) - 1

multiPal <- sapply(1:nClasses, function(i)
{
    colorAlpha <- adjustcolor(qualPal[i], alpha = 0.4)
    colorRampPalette(c(qualPal[i],
                       colorAlpha),
                     alpha = TRUE)(nIntervals)
})

pList <- lapply(1:nClasses, function(i){
    landSub <- landClassR
    ## Those cells from a different land class are set to NA...
    landSub[!(landClassR == i)] <- NA
    ## ... and the resulting raster masks the population raster
    popSub <- mask(popR, landSub)
    ## Palette
    pal <- multiPal[, i]

    pClass <- levelplot(log10(popSub),
                        at = logPopAt,
                        maxpixels = 3.5e5,
                        col.regions = pal,
                        colorkey = FALSE,
                        margin = FALSE)
})

p <- Reduce('+', pList)

library("grid")

legend <- layer(
{
    ## Center of the legend (rectangle)
    x0 <- 125
    y0 <- 22
    ## Width and height of the legend
    w <- 10
    h <- w / nClasses * nIntervals
    ## Legend
    grid.raster(multiPal, interpolate = FALSE,
                      x = unit(x0, "native"),
                      y = unit(y0, "native"),
                width = unit(w, "native"))
    ## Axes of the legend
    ## x-axis (qualitative variable)
    grid.text(classes,
              x = unit(seq(x0 - w * (nClasses -1)/(2*nClasses),
                           x0 + w * (nClasses -1)/(2*nClasses),
                           length = nClasses),
                       "native"),
              y = unit(y0 + h/2, "native"),
              just = "bottom",
              rot = 10,
              gp = gpar(fontsize = 6))
    ## y-axis (quantitative variable)
    yLabs <- paste0("[",
                    paste(logPopAt[-nIntervals],
                          logPopAt[-1], sep = ","),
                    "]")
    grid.text(yLabs,
              x = unit(x0 + w/2, "native"),
              y = unit(seq(y0 - h * (nIntervals -1)/(2*nIntervals),
                           y0 + h * (nIntervals -1)/(2*nIntervals),
                           length = nIntervals),
                       "native"),
              just = "left",
              gp = gpar(fontsize = 6))

})

p + legend

##################################################################
## 3D visualization
##################################################################

plot3D(DEMr, maxpixels = 5e4)

library("rgl")

writeSTL("docs/images/rgl/DEM.stl")

##################################################################
## mapview
##################################################################

library("mapview")

mvSIS <- mapview(SISavr, legend = TRUE)

SIAR <- read.csv("data/Spatial/SIAR.csv")

spSIAR <- SpatialPointsDataFrame(coords = SIAR[, c("lon", "lat")], 
                                 data = SIAR,
                                 proj4str = CRS(projection(SISavr)))

sfSIAR <- st_as_sf(SIAR,
                   coords = c("lon", "lat"),
                   crs = crs(SISavt))

mvSIAR <- mapview(sfSIAR,
                  label = sfSIAR$Estacion)

mvSIS + mvSIAR
