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
## Polylines
##################################################################

library(zoo)

load('data/CO2.RData')

## lattice version
xyplot(GNI.capita  ~ CO2.capita, data = CO2data,
       xlab = "Carbon dioxide emissions (metric tons per capita)",
       ylab = "GNI per capita, PPP (current international $)",
       groups = Country.Name, type = 'b')

## ggplot2 version
ggplot(data = CO2data, aes(x = CO2.capita, y = GNI.capita,
                         color = Country.Name)) +
    xlab("Carbon dioxide emissions (metric tons per capita)") +
    ylab("GNI per capita, PPP (current international $)") +
    geom_point() + geom_path() + theme_bw()

##################################################################
## Choosing colors
##################################################################

library(RColorBrewer)

nCountries <- nlevels(CO2data$Country.Name)
pal <- brewer.pal(n = 5, 'Set1')
pal <- rep(pal, length = nCountries)

## Rank of average values of CO2 per capita
CO2mean <- aggregate(CO2.capita ~ Country.Name,
                     data = CO2data, FUN = mean)
palOrdered <- pal[rank(CO2mean$CO2.capita)]

library(reshape2)

CO2capita <- CO2data[, c('Country.Name',
                         'Year',
                         'CO2.capita')]
CO2capita <- dcast(CO2capita, Country.Name ~ Year)

summary(CO2capita)

hCO2 <- hclust(dist(CO2capita[, -1]))

oldpar <- par(mar = c(0, 2, 0, 0) + .1)
plot(hCO2, labels = CO2capita$Country.Name,
     xlab = '', ylab = '', sub = '', main = '')
par(oldpar)

idx <- match(levels(CO2data$Country.Name), 
             CO2capita$Country.Name[hCO2$order])
palOrdered <- pal[idx]

## lattice version
## simpleTheme encapsulates the palette in a new theme for xyplot
myTheme <- simpleTheme(pch = 19, cex = 0.6, col = palOrdered)

pCO2.capita <- xyplot(GNI.capita  ~ CO2.capita,
                      data = CO2data,
                      xlab = "Carbon dioxide emissions (metric tons per capita)",
                      ylab = "GNI per capita, PPP (current international $)",
                      groups = Country.Name,
                      par.settings = myTheme,
                      type = 'b')

## ggplot2 version
gCO2.capita <- ggplot(data = CO2data,
                      aes(x = CO2.capita,
                          y = GNI.capita,
                          color = Country.Name)) +
    geom_point() + geom_path() +
    scale_color_manual(values = palOrdered, guide = FALSE) +
    xlab('CO2 emissions (metric tons per capita)') +
    ylab('GNI per capita, PPP (current international $)') +
    theme_bw()

##################################################################
## Labels to show time information
##################################################################

xyplot(GNI.capita  ~ CO2.capita,
       data = CO2data,
       xlab = "Carbon dioxide emissions (metric tons per capita)",
       ylab = "GNI per capita, PPP (current international $)",
       groups = Country.Name,
       par.settings = myTheme,
       type = 'b',
       panel = function(x, y, ..., subscripts, groups){
           panel.text(x, y, ...,
                      labels = CO2data$Year[subscripts],
                      pos = 2, cex = 0.5, col = 'gray')
           panel.superpose(x, y, subscripts, groups,...)
       })

## lattice version
pCO2.capita <- pCO2.capita +
    glayer_(panel.text(...,
                       labels = CO2data$Year[subscripts],
                         pos = 2, cex = 0.5, col = 'gray'))

## ggplot2 version
gCO2.capita <- gCO2.capita + geom_text(aes(label = Year),
                                       colour = 'gray',
                                       size = 2.5,
                                       hjust = 0, vjust = 0)

##################################################################
## Country names: positioning labels
##################################################################

library(directlabels)

## lattice version
direct.label(pCO2.capita,
             method = 'extreme.grid')

## ggplot2 version
direct.label(gCO2.capita, method = 'extreme.grid')

##################################################################
## A panel for each year
##################################################################

## lattice version
xyplot(GNI.capita  ~ CO2.capita | factor(Year),
       data = CO2data,
       xlab = "Carbon dioxide emissions (metric tons per capita)",
       ylab = "GNI per capita, PPP (current international $)",
       groups = Country.Name, type = 'b',
       auto.key = list(space = 'right'))

## ggplot2 version
ggplot(data = CO2data,
       aes(x = CO2.capita,
           y = GNI.capita,
           colour = Country.Name)) +
    facet_wrap(~ Year) + geom_point(pch = 19) + 
    xlab('CO2 emissions (metric tons per capita)') +
    ylab('GNI per capita, PPP (current international $)') +
    theme_bw()

## lattice version
xyplot(GNI.capita  ~ CO2.capita | factor(Year),
       data = CO2data,
       xlab = "Carbon dioxide emissions (metric tons per capita)",
       ylab = "GNI per capita, PPP (current international $)",
       groups = Country.Name, type = 'b',
       par.settings = myTheme) + 
    glayer(panel.pointLabel(x, y,
                            labels = group.value,
                            col = palOrdered[group.number],
                            cex = 0.7))

##################################################################
## Using variable size to encode an additional variable
##################################################################

library(classInt)
z <- CO2data$CO2.PPP
intervals <- classIntervals(z, n = 4, style = 'fisher')

nInt <- length(intervals$brks) - 1
cex.key <- seq(0.5, 1.8, length = nInt)

idx <- findCols(intervals)
CO2data$cexPoints <- cex.key[idx]

ggplot(data = CO2data,
       aes(x = CO2.capita,
           y = GNI.capita,
           colour = Country.Name)) +
    facet_wrap(~ Year) +
    geom_point(aes(size = cexPoints), pch = 19) +
    xlab('Carbon dioxide emissions (metric tons per capita)') +
    ylab('GNI per capita, PPP (current international $)') +
    theme_bw()

op <- options(digits = 2)
tab <- print(intervals)
options(op)
  
key <- list(space = 'right',
            title = expression(CO[2]/GNI.PPP),
            cex.title = 1,
            ## Labels of the key are the intervals strings
            text = list(labels = names(tab), cex = 0.85),
            ## Points sizes are defined with cex.key
            points = list(col = 'black', pch = 19,
                cex = cex.key, alpha = 0.7))

  
xyplot(GNI.capita ~ CO2.capita|factor(Year), data = CO2data,
       xlab = "Carbon dioxide emissions (metric tons per capita)",
       ylab = "GNI per capita, PPP (current international $)",
       groups = Country.Name, key = key, alpha = 0.7,
       panel  =  panel.superpose,
       panel.groups  =  function(x, y,
           subscripts, group.number, group.value, ...){
           panel.xyplot(x, y,
                        col  =  palOrdered[group.number],
                        cex  =  CO2data$cexPoints[subscripts])
           panel.pointLabel(x, y, labels = group.value,
                            col = palOrdered[group.number],
                            cex = 0.7)
       }
       )

library(plotly)
p <- plot_ly(CO2data,
             x = ~CO2.capita,
             y = ~GNI.capita)

p <- add_markers(p,
                 size = ~CO2.PPP,
                 color = ~Country.Name,
                 text = ~Country.Name, hoverinfo = "text",
                 ids = ~Country.Name,
                 frame = ~Year,
                 showlegend = FALSE)

p <- animation_opts(p,
                    frame = 1000,
                    transition = 800,
                    redraw = FALSE)

p <- animation_slider(p,
                      currentvalue = list(prefix = "Year "))

p

##################################################################
## googleVis
##################################################################

library(googleVis)

pgvis <- gvisMotionChart(CO2data,
                         xvar = 'CO2.capita',
                         yvar = 'GNI.capita',
                         sizevar = 'CO2.PPP',
                         idvar = 'Country.Name',
                         timevar = 'Year')

print(pgvis, 'html', file = 'figs/googleVis.html')

library(gridSVG)
library(grid)

xyplot(GNI.capita ~ CO2.capita,
       data = CO2data,
       xlab = "Carbon dioxide emissions (metric tons per capita)",
       ylab = "GNI per capita, PPP (current international $)",
       subset = Year==2000, groups = Country.Name,
       ## The limits of the graphic are defined
       ## with the entire dataset
       xlim = extendrange(CO2data$CO2.capita),
       ylim = extendrange(CO2data$GNI.capita),
       panel = function(x, y, ..., subscripts, groups) {
           color <- palOrdered[groups[subscripts]]
           radius <- CO2data$CO2.PPP[subscripts]
           ## Size of labels
           cex <- 1.1*sqrt(radius)
           ## Bubbles
           grid.circle(x, y, default.units = "native",
                       r = radius*unit(.25, "inch"),
                       name = trellis.grobname("points", type = "panel"),
                       gp = gpar(col = color,
                               ## Fill color ligther than border
                               fill = adjustcolor(color, alpha = .5),
                               lwd = 2))
           ## Country labels
           grid.text(label = groups[subscripts],
                     x = unit(x, 'native'),
                     ## Labels above each bubble
                     y = unit(y, 'native') + 1.5 * radius *unit(.25, 'inch'),
                     name = trellis.grobname('labels', type = 'panel'),
                     gp = gpar(col = color, cex = cex))
       })

## Duration in seconds of the animation
duration <- 20
  
nCountries <- nlevels(CO2data$Country.Name)
years <- unique(CO2data$Year)
nYears <- length(years)

## Intermediate positions of the bubbles
x_points <- animUnit(unit(CO2data$CO2.capita, 'native'),
                     id = rep(seq_len(nCountries), each = nYears))
y_points <- animUnit(unit(CO2data$GNI.capita, 'native'),
                     id = rep(seq_len(nCountries), each = nYears))
## Intermediate positions of the labels
y_labels <- animUnit(unit(CO2data$GNI.capita, 'native') +
                     1.5 * CO2data$CO2.PPP * unit(.25, 'inch'),
                     id = rep(seq_len(nCountries), each = nYears))
## Intermediate sizes of the bubbles
size <- animUnit(CO2data$CO2.PPP * unit(.25, 'inch'),
                 id = rep(seq_len(nCountries), each = nYears))

grid.animate(trellis.grobname("points", type = "panel", row = 1, col = 1),
             duration = duration,
             x = x_points,
             y = y_points,
             r = size,
             rep = TRUE)

grid.animate(trellis.grobname("labels", type = "panel", row = 1, col = 1),
             duration = duration,
             x = x_points,
             y = y_labels,
             rep = TRUE)

countries <- unique(CO2data$Country.Name)
URL <- paste('http://en.wikipedia.org/wiki/', countries, sep = '')
grid.hyperlink(trellis.grobname('points', type = 'panel', row = 1, col = 1),
               URL, group = FALSE)

visibility <- matrix("hidden", nrow = nYears, ncol = nYears)
diag(visibility) <- "visible"
yearText <- animateGrob(garnishGrob(textGrob(years, .9, .15,
                                             name = "year",
                                             gp = gpar(cex = 2, col = "grey")),
                                    visibility = "hidden"),
                        duration = 20,
                        visibility = visibility,
                        rep = TRUE)
grid.draw(yearText)

grid.export("figs/bubbles.svg")
