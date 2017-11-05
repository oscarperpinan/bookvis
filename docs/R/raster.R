  ##################################################################
  ## Initial configuration
  ##################################################################
  ## Clone or download the repository and set the working directory
  ## with setwd to the folder where the repository is located.

  ##################################################################
  ## Raster maps
  ##################################################################

  ##################################################################
  ## Quantitative data
  ##################################################################

  library(raster)
  library(rasterVis)
  SISav <- raster('data/SISav')
  levelplot(SISav)

  library(maps)
  library(mapdata)
  library(maptools)
  
  ext <- as.vector(extent(SISav))
  boundaries <- map('worldHires',
                    xlim=ext[1:2], ylim=ext[3:4],
                    plot=FALSE)
  boundaries <- map2SpatialLines(boundaries,
                                 proj4string=CRS(projection(SISav)))

  levelplot(SISav) + layer(sp.lines(boundaries, lwd=0.5))

library(mapview)

mapview(SISav, legend = TRUE)

  SIAR <- read.csv("data/SIAR.csv")

  spSIAR <- SpatialPointsDataFrame(SIAR[, c(6, 7)],
                                   SIAR[, -c(6, 7)],
                                   proj4str = CRS(projection(SISav)))

mapview(SISav, legend = TRUE) + mapview(spSIAR, zcol = 'Estacion')

##################################################################
## Hill shading
##################################################################

  old <- setwd(tempdir())
  download.file('http://biogeo.ucdavis.edu/data/diva/msk_alt/ESP_msk_alt.zip', 'ESP_msk_alt.zip')
  unzip('ESP_msk_alt.zip', exdir='.')
  
  DEM <- raster('ESP_msk_alt')

  slope <- terrain(DEM, 'slope')
  aspect <- terrain(DEM, 'aspect')
  hs <- hillShade(slope=slope, aspect=aspect,
                  angle=20, direction=30)

  setwd(old)

  ## hillShade theme: gray colors and semitransparency
  hsTheme <- modifyList(GrTheme(), list(regions=list(alpha=0.6)))
  
  levelplot(SISav, panel=panel.levelplot.raster,
            margin=FALSE, colorkey=FALSE) +
      levelplot(hs, par.settings=hsTheme, maxpixels=1e6) +
      layer(sp.lines(boundaries, lwd=0.5))

##################################################################
## Excursus: 3D visualization
##################################################################

  plot3D(DEM, maxpixels=5e4)

writeSTL('figs/DEM.stl')

  ##################################################################
  ## Diverging palettes
  ##################################################################

meanRad <- cellStats(SISav, 'mean')
SISav <- SISav - meanRad

  xyplot(layer ~ y, data = SISav,
         groups=cut(x, 5),
         par.settings=rasterTheme(symbol=plinrain(n=5, end=200)),
         xlab = 'Latitude', ylab = 'Solar radiation (scaled)',  
         auto.key=list(space='right', title='Longitude', cex.title=1.3))

  divPal <- brewer.pal(n=9, 'PuOr')
  divPal[5] <- "#FFFFFF"
  
  showPal <- function(pal, labs=pal, cex=0.6, ...){
    barplot(rep(1, length(pal)), col=pal,
            names.arg=labs, cex.names=cex,
            axes=FALSE, ...)
  }
  
  showPal(divPal)

  divTheme <- rasterTheme(region=divPal)
  
  levelplot(SISav, contour=TRUE, par.settings=divTheme)

  rng <- range(SISav[])
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
  breaks <- seq(rng[1], by=inc, length= n0 + 1 + n1)

  ## Midpoints computed with the median of each interval
  idx <- findInterval(SISav[], breaks, rightmost.closed=TRUE)
  mids <- tapply(SISav[], idx, median)
  ## Maximum of the absolute value both limits
  mx <- max(abs(breaks))
  mids

  break2pal <- function(x, mx, pal){
    ## x = mx gives y = 1
    ## x = 0 gives y = 0.5
    y <- 1/2*(x/mx + 1)
    rgb(pal(y), maxColorValue=255)
  }
  
  ## Interpolating function that maps colors with [0, 1]
  ## rgb(divRamp(0.5), maxColorValue=255) gives "#FFFFFF" (white)
  divRamp <- colorRamp(divPal)
  ## Diverging palette where white is associated with the interval
  ## containing the zero
  pal <- break2pal(mids, mx, divRamp)
  showPal(pal, round(mids, 1))

  levelplot(SISav, par.settings=rasterTheme(region=pal),
            at=breaks, contour=TRUE)

  divTheme <- rasterTheme()
  
  divTheme$regions$col <- pal
  levelplot(SISav, par.settings=divTheme, at=breaks, contour=TRUE)

  library(classInt)
  
  cl <- classIntervals(SISav[],
                       ## n=15, style='equal')
                       ## style='hclust')
                       ## style='sd')
                       style='kmeans')
                       ## style='quantile')
  cl
  breaks <- cl$brks

  idx <- findInterval(SISav[], breaks, rightmost.closed=TRUE)
  mids <- tapply(SISav[], idx, median)
  mids
  mx <- max(abs(breaks))
  pal <- break2pal(mids, mx, divRamp)
  divTheme$regions$col <- pal
  levelplot(SISav, par.settings=divTheme, at=breaks, contour=TRUE)

  ##################################################################
  ## Categorical data
  ##################################################################

  library(raster)
  ## China and India  
  ext <- extent(65, 135, 5, 55)
  
  pop <- raster('875430rgb-167772161.0.FLOAT.TIFF')
  pop <- crop(pop, ext)
  pop[pop==99999] <- NA
  
  landClass <- raster('241243rgb-167772161.0.TIFF')
  landClass <- crop(landClass, ext)

  landClass[landClass %in% c(0, 254)] <- NA
  ## Only four groups are needed:
  ## Forests: 1:5
  ## Shrublands, etc: 6:11
  ## Agricultural/Urban: 12:14
  ## Snow: 15:16
  landClass <- cut(landClass, c(0, 5, 11, 14, 16))
  ## Add a Raster Attribute Table and define the raster as categorical data
  landClass <- ratify(landClass)
  ## Configure the RAT: first create a RAT data.frame using the
  ## levels method; second, set the values for each class (to be
  ## used by levelplot); third, assign this RAT to the raster
  ## using again levels
  rat <- levels(landClass)[[1]]
  rat$classes <- c('Forest', 'Land', 'Urban', 'Snow')
  levels(landClass) <- rat

  library(rasterVis)
  
  pal <- c('palegreen4', # Forest
           'lightgoldenrod', # Land
           'indianred4', # Urban
           'snow3')      # Snow
  
  catTheme <- modifyList(rasterTheme(),
                         list(panel.background = list(col='lightskyblue1'),
                              regions = list(col= pal)))
  
  levelplot(landClass, maxpixels=3.5e5, par.settings=catTheme,
            panel=panel.levelplot.raster)

  pPop <- levelplot(pop, zscaleLog=10, par.settings=BTCTheme,
                    maxpixels=3.5e5, panel=panel.levelplot.raster)
  pPop

  s <- stack(pop, landClass)
  names(s) <- c('pop', 'landClass')
  histogram(~log10(pop)|landClass, data=s,
            scales=list(relation='free'))

##################################################################
## Multivariate legend
##################################################################

  library(colorspace)
  ## at for each sub-levelplot is obtained from the global levelplot
  at <- pPop$legend$bottom$args$key$at
  classes <- rat$classes
  nClasses <- length(classes)
  
  pList <- lapply(1:nClasses, function(i){
    landSub <- landClass
    ## Those cells from a different land class are set to NA...
    landSub[!(landClass==i)] <- NA
    ## ... and the resulting raster masks the population raster
    popSub <- mask(pop, landSub)
    ## The HCL color wheel is divided in nClasses
    step <- 360/nClasses
    ## and a sequential palette is constructed with a hue from one of
    ## the color wheel parts
    cols <- rev(sequential_hcl(16, h = (30 + step*(i-1))%%360))
  
    pClass <- levelplot(popSub, zscaleLog=10, at=at,
                        maxpixels=3.5e5,
                        ## labels only needed in the last legend
                        colorkey=(if (i==nClasses) TRUE else list(labels=list(labels=rep('', 17)))),
                        col.regions=cols, margin=FALSE)
  })

  p <- Reduce('+', pList)
  ## Function to add a title to a legend
  addTitle <- function(legend, title){
    titleGrob <- textGrob(title, gp=gpar(fontsize=8), hjust=0.5, vjust=1)
    ## retrieve the legend from the trellis object
    legendGrob <- eval(as.call(c(as.symbol(legend$fun), legend$args)))
    ## Layout of the legend WITH the title
    ly <- grid.layout(ncol=1, nrow=2,
                      widths=unit(0.9, 'grobwidth', data=legendGrob))
    ## Create a frame to host the original legend and the title
    fg <- frameGrob(ly, name=paste('legendTitle', title, sep='_'))
    ## Add the grobs to the frame
    pg <- packGrob(fg, titleGrob, row=2)
    pg <- packGrob(pg, legendGrob, row=1)
    }
  
  ## Access each trellis object from pList...
  for (i in seq_len(nClasses)){
    ## extract the legend (automatically created by spplot)...
    lg <- pList[[i]]$legend$right
    ## ... and add the addTitle function to the legend component of each trellis object
    pList[[i]]$legend$right <- list(fun='addTitle',
                                    args=list(legend=lg, title=classes[i]))
  }
  
  ## List of legends
  legendList <- lapply(pList, function(x){
    lg <- x$legend$right
    clKey <- eval(as.call(c(as.symbol(lg$fun), lg$args)))
    clKey
  })
  
  ## Function to pack the list of legends in a unique legend
  ## Adapted from latticeExtra::: mergedTrellisLegendGrob
  packLegend <- function(legendList){
    N <- length(legendList)
    ly <- grid.layout(nrow = 1,  ncol = N)
    g <- frameGrob(layout = ly, name = "mergedLegend")
    for (i in 1:N) g <- packGrob(g, legendList[[i]], col = i)
    g
  }
  
  ## The legend of p will include all the legends
  p$legend$right <- list(fun = 'packLegend',  args = list(legendList = legendList))
  
  
  p
