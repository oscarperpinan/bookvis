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
## Scatterplot matrix: time as a grouping variable 
##################################################################

library(zoo)

load('data/aranjuez.RData')
aranjuezDF <- as.data.frame(aranjuez)
aranjuezDF$Month <- format(index(aranjuez), '%m')

## Red-Blue palette with black added (12 colors)
colors <- c(brewer.pal(n=11, 'RdBu'), '#000000')
## Rearrange according to months (darkest for summer)
colors <- colors[c(6:1, 12:7)]

splom(~ aranjuezDF, 
      groups = aranjuezDF$Month,
      auto.key = list(space = 'right', 
                    title = 'Month', cex.title = 1),
      pscale = 0, varname.cex = 0.7, xlab = '',
      par.settings = custom.theme(symbol = colors,
                                pch = 19),
      cex = 0.3, alpha = 0.1)

trellis.focus('panel', 1, 1)
idx <- panel.link.splom(pch=13, cex=0.6, col='green')
aranjuez[idx,]

library(GGally)

ggpairs(aranjuezDF,
        columns = 1:10, ## Do not include "Month"
        upper = list(continuous = "points"),
        mapping = aes(colour = Month, alpha = 0.1))

##################################################################
## Hexagonal binning
##################################################################

library(hexbin)
  
splom(~as.data.frame(aranjuez),
      panel = panel.hexbinplot,
      diag.panel = function(x, ...){
          yrng <- current.panel.limits()$ylim
          d <- density(x, na.rm = TRUE)
          d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y))
          panel.lines(d)
          diag.panel.splom(x, ...)
      },
      lower.panel = function(x, y, ...){
          panel.hexbinplot(x, y, ...)
          panel.loess(x, y, ..., col = 'red')
      },
      xlab = '',
      pscale = 0, varname.cex = 0.7)

library(reshape2)

aranjuezRshp <- melt(aranjuezDF,
                     measure.vars = c('TempMax',
                                      'TempAvg',
                                      'TempMin'),
                     variable.name = 'Statistic',
                     value.name = 'Temperature')

summary(aranjuezRshp)

hexbinplot(Radiation ~ Temperature | Statistic,
           data = aranjuezRshp,
           layout = c(1, 3)) +
    layer(panel.loess(..., col = 'red'))

ggplot(data = aranjuezRshp,
       aes(Temperature, Radiation)) +
    stat_binhex(ncol = 1) + 
    stat_smooth(se = FALSE, method = 'loess', col = 'red') +
    facet_wrap(~ Statistic, ncol = 1) +
    theme_bw()

##################################################################
## Scatterplot with time as a conditioning variable
##################################################################

ggplot(data = aranjuezRshp, aes(Radiation, Temperature)) +
    facet_grid(Statistic ~ Month) +
    geom_point(col = 'skyblue4', pch = 19, cex = 0.5, alpha = 0.3) +
    geom_rug() +
    stat_smooth(se = FALSE, method = 'loess', col = 'indianred1', lwd = 1.2) +
    theme_bw()

useOuterStrips(xyplot(Temperature ~ Radiation | Month * Statistic,
                      data = aranjuezRshp,
                      between = list(x = 0),
                      col = 'skyblue4', pch = 19,
                      cex = 0.5, alpha = 0.3)) +
    layer({
        panel.rug(..., col.line = 'indianred1', end = 0.05, alpha = 0.6)
        panel.loess(..., col = 'indianred1', lwd = 1.5, alpha = 1)
    })
