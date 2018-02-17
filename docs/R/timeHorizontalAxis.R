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
## Time graph of variables with different scales
##################################################################

library(zoo)
load('data/aranjuez.RData')

## The layout argument arranges panels in rows
xyplot(aranjuez, layout = c(1, ncol(aranjuez)))

autoplot(aranjuez) + facet_free()

##################################################################
## Annotations to enhance the time graph
##################################################################

## lattice version

library(grid)
library(latticeExtra)
  
## Auxiliary function to extract the year value of a POSIXct time
## index
Year <- function(x)format(x, "%Y")
  
xyplot(aranjuez, layout = c(1, ncol(aranjuez)), strip = FALSE,
       scales = list(y = list(cex = 0.6, rot = 0)),
       panel = function(x, y, ...){
           ## Alternation of years
           panel.xblocks(x, Year,
                         col = c("lightgray", "white"),
                         border = "darkgray")
           ## Values under the average highlighted with red regions
           panel.xblocks(x, y<mean(y, na.rm = TRUE),
                         col = "indianred1",
                         height = unit(0.1, 'npc'))
           ## Time series
           panel.lines(x, y, col = 'royalblue4', lwd = 0.5, ...)
           ## Label of each time series
           panel.text(x[1], min(y, na.rm = TRUE),
                      names(aranjuez)[panel.number()],
                      cex = 0.6, adj = c(0, 0), srt = 90, ...)
           ## Triangles to point the maxima and minima 
           idxMax <- which.max(y)
           panel.points(x[idxMax], y[idxMax],
                        col = 'black', fill = 'lightblue', pch = 24)
           idxMin <- which.min(y)
           panel.points(x[idxMin], y[idxMin],
                        col = 'black', fill = 'lightblue', pch = 25)
       })

## ggplot2 version

timeIdx <- index(aranjuez)
  
aranjuezLong <- fortify(aranjuez, melt = TRUE)

summary(aranjuezLong)

## Values below mean are negative after being centered
scaled <- fortify(scale(aranjuez, scale = FALSE), melt = TRUE)
## The 'scaled' column is the result of the centering.
## The new 'Value' column store the original values.
scaled <- transform(scaled, scaled = Value,
                    Value = aranjuezLong$Value)
underIdx <- which(scaled$scaled <= 0)
## 'under' is the subset of values below the average
under <- scaled[underIdx,]

library(xts)
ep <- endpoints(timeIdx, on = 'years')
ep <- ep[-1]
N <- length(ep)
## 'tsp' is start and 'tep' is the end of each band. One of each two
## years are selected.
tep <- timeIdx[ep[seq(1, N, 2)] + 1]
tsp <- timeIdx[ep[seq(2, N, 2)]]

minIdx <- timeIdx[apply(aranjuez, 2, which.min)]
minVals <- apply(aranjuez, 2, min, na.rm = TRUE)
mins <- data.frame(Index = minIdx,
                   Value = minVals,
                   Series = names(aranjuez))

maxIdx <- timeIdx[apply(aranjuez, 2, which.max)]
maxVals <- apply(aranjuez, 2, max, na.rm = TRUE)
maxs <- data.frame(Index = maxIdx,
                   Value = maxVals,
                   Series = names(aranjuez))

ggplot(data = aranjuezLong, aes(Index, Value)) +
    ## Time series of each variable
    geom_line(colour = "royalblue4", lwd = 0.5) +
    ## Year bands
    annotate('rect',
             xmin = tsp, xmax = tep,
             ymin = -Inf, ymax = Inf,
              alpha = 0.4) + 
    ## Values below average
    geom_rug(data = under,
             sides = 'b', col = 'indianred1') +
    ## Minima
    geom_point(data = mins, pch = 25) +
    ## Maxima
    geom_point(data = maxs, pch = 24) +
    ## Axis labels and theme definition
    labs(x = 'Time', y = NULL) +
    theme_bw() +
    ## Each series is displayed in a different panel with an
    ## independent y scale
    facet_free()

##################################################################
## Time series of variables with the same scale
##################################################################

load('data/navarra.RData')

avRad <- zoo(rowMeans(navarra, na.rm = 1), index(navarra))
pNavarra <- xyplot(navarra - avRad,
                   superpose = TRUE, auto.key = FALSE,
                   lwd = 0.5, alpha = 0.3, col = 'midnightblue') 
pNavarra

##################################################################
## Aspect ratio and rate of change
##################################################################

xyplot(navarra - avRad,
       aspect = 'xy', cut = list(n = 3, overlap = 0.1),
       strip = FALSE,
       superpose = TRUE, auto.key = FALSE,
       lwd = 0.5, alpha = 0.3, col = 'midnightblue')

##################################################################
## The horizon graph
##################################################################

library(latticeExtra)
  
horizonplot(navarra - avRad,
            layout = c(1, ncol(navarra)),
            origin = 0, ## Deviations in each panel are calculated
                        ## from this value
            colorkey = TRUE)

##################################################################
## Time graph of the differences between a time series and a reference
##################################################################

Ta <- aranjuez$TempAvg
timeIndex <- index(aranjuez)
longTa <- ave(Ta, format(timeIndex, '%j'))
diffTa <- (Ta - longTa)

xyplot(cbind(Ta, longTa, diffTa),
       col = c('darkgray', 'red', 'midnightblue'),
       superpose = TRUE, auto.key = list(space = 'right'),
       screens = c(rep('Average Temperature', 2), 'Differences'))

years <- unique(format(timeIndex, '%Y'))
  
horizonplot(diffTa, cut = list(n = 8, overlap = 0),
            colorkey = TRUE, layout = c(1, 8),
            scales = list(draw = FALSE, y = list(relation = 'same')),
            origin = 0, strip.left = FALSE) +
    layer(grid.text(years[panel.number()], x  =  0, y  =  0.1, 
                    gp = gpar(cex = 0.8),
                    just = "left"))

year <- function(x)as.numeric(format(x, '%Y'))
day <- function(x)as.numeric(format(x, '%d'))
month <- function(x)as.numeric(format(x, '%m'))

myTheme <- modifyList(custom.theme(region = brewer.pal(9, 'RdBu')),
                      list(
                          strip.background = list(col = 'gray'),
                          panel.background = list(col = 'gray')))

maxZ <- max(abs(diffTa))

levelplot(diffTa ~ day(timeIndex) * year(timeIndex) | factor(month(timeIndex)),
          at = pretty(c(-maxZ, maxZ), n = 8),
          colorkey = list(height = 0.3),
          layout = c(1, 12), strip = FALSE, strip.left = TRUE,
          xlab = 'Day', ylab = 'Month', 
          par.settings = myTheme)

df <- data.frame(Vals = diffTa,
                 Day = day(timeIndex),
                 Year = year(timeIndex),
                 Month = month(timeIndex))

library(scales) 
## The packages scales is needed for the pretty_breaks function.

ggplot(data = df,
       aes(fill = Vals,
           x = Day,
           y = Year)) +
    facet_wrap(~ Month, ncol = 1, strip.position = 'left') +
    scale_y_continuous(breaks = pretty_breaks()) + 
    scale_fill_distiller(palette = 'RdBu', direction = 1) + 
    geom_raster() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

##################################################################
## Stacked graphs
##################################################################

load('data/unemployUSA.RData')

xyplot(unemployUSA,
       superpose = TRUE,
       par.settings = custom.theme,
       auto.key = list(space = 'right'))

library(scales) ## scale_x_yearmon needs scales::pretty_breaks
autoplot(unemployUSA, facets = NULL, geom = 'area') +
    geom_area(aes(fill = Series)) +
    scale_x_yearmon()

panel.flow <- function(x, y, groups, origin, ...)
{
    dat <- data.frame(x = x, y = y, groups = groups)
    nVars <- nlevels(groups)
    groupLevels <- levels(groups)
    
    ## From long to wide
    yWide <- unstack(dat, y~groups)
    ## Where are the maxima of each variable located? We will use
    ## them to position labels.
    idxMaxes <- apply(yWide, 2, which.max)
    
    ##Origin calculated following Havr.eHetzler.ea2002
    if (origin=='themeRiver') origin =  -1/2*rowSums(yWide)
    else origin = 0 
    yWide <- cbind(origin = origin, yWide)
    ## Cumulative sums to define the polygon
    yCumSum <- t(apply(yWide, 1, cumsum))
    Y <- as.data.frame(sapply(seq_len(nVars),
                              function(iCol)c(yCumSum[,iCol+1],
                                              rev(yCumSum[,iCol]))))
    names(Y) <- levels(groups)
    ## Back to long format, since xyplot works that way
    y <- stack(Y)$values
    
    ## Similar but easier for x
    xWide <- unstack(dat, x~groups)
    x <- rep(c(xWide[,1], rev(xWide[,1])), nVars)
    ## Groups repeated twice (upper and lower limits of the polygon)
    groups <- rep(groups, each = 2)
    
    ## Graphical parameters
    superpose.polygon <- trellis.par.get("superpose.polygon")
    col = superpose.polygon$col
    border = superpose.polygon$border 
    lwd = superpose.polygon$lwd 
    
    ## Draw polygons
    for (i in seq_len(nVars)){
        xi <- x[groups==groupLevels[i]]
        yi <- y[groups==groupLevels[i]]
        panel.polygon(xi, yi, border = border,
                      lwd = lwd, col = col[i])
    }
    
    ## Print labels
    for (i in seq_len(nVars)){
        xi <- x[groups==groupLevels[i]]
        yi <- y[groups==groupLevels[i]]
        N <- length(xi)/2
        ## Height available for the label
        h <- unit(yi[idxMaxes[i]], 'native') -
            unit(yi[idxMaxes[i] + 2*(N-idxMaxes[i]) +1], 'native')
        ##...converted to "char" units
        hChar <- convertHeight(h, 'char', TRUE)
        ## If there is enough space and we are not at the first or
        ## last variable, then the label is printed inside the polygon.
        if((hChar >= 1) && !(i %in% c(1, nVars))){
            grid.text(groupLevels[i],
                      xi[idxMaxes[i]],
                      (yi[idxMaxes[i]] +
                       yi[idxMaxes[i] + 2*(N-idxMaxes[i]) +1])/2,
                      gp = gpar(col = 'white', alpha = 0.7, cex = 0.7),
                      default.units = 'native')
        } else {
            ## Elsewhere, the label is printed outside
            
            grid.text(groupLevels[i],
                      xi[N],
                      (yi[N] + yi[N+1])/2,
                      gp = gpar(col = col[i], cex = 0.7),
                      just = 'left', default.units = 'native')
        }          
    }
}

prepanel.flow <- function(x, y, groups, origin,...)
{
    dat <- data.frame(x = x, y = y, groups = groups)
    nVars <- nlevels(groups)
    groupLevels <- levels(groups)
    yWide <- unstack(dat, y~groups)
    if (origin=='themeRiver') origin =  -1/2*rowSums(yWide)
    else origin = 0
    yWide <- cbind(origin = origin, yWide)
    yCumSum <- t(apply(yWide, 1, cumsum))
    
    list(xlim = range(x),
         ylim = c(min(yCumSum[,1]), max(yCumSum[,nVars+1])),
         dx = diff(x),
         dy = diff(c(yCumSum[,-1])))
}

library(colorspace)
## We will use a qualitative palette from colorspace
nCols <- ncol(unemployUSA)
pal <- rainbow_hcl(nCols, c = 70, l = 75, start = 30, end = 300)
myTheme <- custom.theme(fill = pal, lwd = 0.2)

sep2008 <- as.numeric(as.yearmon('2008-09'))

xyplot(unemployUSA, superpose = TRUE, auto.key = FALSE,
       panel = panel.flow, prepanel = prepanel.flow,
       origin = 'themeRiver', scales = list(y = list(draw = FALSE)),
       par.settings = myTheme) +
    layer(panel.abline(v = sep2008, col = 'gray', lwd = 0.7))

##################################################################
## Panel and prepanel functions to implement the ThemeRiver with =xyplot=
##################################################################

library(dygraphs)

dyTemp <- dygraph(aranjuez[, c("TempMin", "TempAvg", "TempMax")],
                  main = "Temperature in Aranjuez",
                  ylab = "ºC")

dyTemp

dyTemp %>%
    dyHighlight(highlightSeriesBackgroundAlpha = 0.2,
                highlightSeriesOpts = list(strokeWidth = 2))

dygraph(aranjuez[, c("TempMin", "TempAvg", "TempMax")],
        main = "Temperature in Aranjuez",
        ylab = "ºC") %>%
    dySeries(c("TempMin", "TempAvg", "TempMax"),
             label = "Temperature")

library(highcharter)
library(xts)

aranjuezXTS <- as.xts(aranjuez)

highchart() %>%
    hc_add_series(name = 'TempMax',
                      aranjuezXTS[, "TempMax"]) %>%
    hc_add_series(name = 'TempMin',
                      aranjuezXTS[, "TempMin"]) %>%
    hc_add_series(name = 'TempAvg',
                      aranjuezXTS[, "TempAvg"])

aranjuezDF <- fortify(aranjuez[,
                               c("TempMax",
                                 "TempAvg",
                                 "TempMin")],
                      melt = TRUE)

summary(aranjuezDF)

library(plotly)

plot_ly(aranjuezDF) %>%
    add_lines(x = ~ Index,
              y = ~ Value,
              color = ~ Series)

##################################################################
## Interaction with gridSVG
##################################################################

library(gridSVG)
## grobs in the graphical output
pNavarra
grobs <- grid.ls(print = FALSE)
## only interested in some of them
nms <- grobs$name[grobs$type == "grobListing"]
idxNames <- grep('lines', nms)
IDs <- nms[idxNames]

for (id in unique(IDs))
{
    ## extract information from the data
    ## according to the ID value
    i <- strsplit(id, '\\.')
    i <- sapply(i, function(x)as.numeric(x[5]))
    ## Information to be attached to each line: annual mean of daily
    ## radiation and abbreviated name of the station
    dat <- round(mean(navarra[,i], na.rm = TRUE), 2)
    info <- paste(names(navarra)[i], paste(dat, collapse = ','),
                  sep = ': ')
    ## attach SVG attributes
    grid.garnish(id,
                 onmouseover = "highlight(evt)",
                 onmouseout = "hide(evt)",
                 title = info)
}

grid.script(filename="highlight.js")

grid.export('figs/navarraRadiation.svg')

unemployDF <- fortify(unemployUSA, melt = TRUE)

head(unemployDF)

## remotes::install_github("hrbrmstr/streamgraph")
library(streamgraph)

streamgraph(unemployDF,
            key = "Series",
            value = "Value",
            date = "Index") %>%
    sg_axis_x(1, "year", "%Y") %>%
    sg_fill_brewer("Set1")
