setwd('~/Dropbox/chapman/book')
library(zoo)
library(ggplot2)
load("data/aranjuez.RData")
# dates when darker background panels start and end. (en = st + 1yr - 1day)
st <- seq(start(aranjuez), end(aranjuez), by = "2 year")
en <- as.Date(as.yearmon(st) + 1) - 1
# long form of data. Column names of long are Index, Series and Value.
long <- fortify(aranjuez, melt = TRUE)
# subset of long with only points below mean of each Series
below <- subset(na.omit(long), Value < ave(Value, Series))
# subset of long containing only the min and max rows for each Series
nr <- nrow(aranjuez)
long.min <- subset(long, ave(Value, Series, FUN = which.min) == 1:nr)
long.max <- subset(long, ave(Value, Series, FUN = which.max) == 1:nr)
# x is xadjust days to left of xmin. y is vertical midpoint.
xadjust <- 10 ##
nms <- data.frame(Series = names(aranjuez), ##
x = start(aranjuez) - xadjust, ##
y = (long.min$Value + long.max$Value)/2) ##
autoplot(aranjuez, geom = "blank") +
    geom_line(col = "royalblue4", lwd = 0.5) +
    geom_rug(data = below, sides = "b", col="indianred1", cex=.5) +
    geom_point(data = long.min, fill = "lightblue", pch = 24) +
    geom_point(data = long.max, fill = "lightblue", pch = 25) +
    annotate(geom = "rect", xmin = st, xmax = en,
             ymin = -Inf, ymax = Inf, alpha = 0.2) +
    geom_text(data = nms, aes(x, y, label = Series), ##
              size = 2.5, angle = 90, vjust = 0, hjust = 0.5) + ##
    theme(strip.background = element_blank(), strip.text = element_blank()) + ##
    scale_x_date(name = "Time"
                 , expxsand = c(0.01, 0) ##
                 ) +
    facet_free()


##################################################################
library(xts)

timeIdx <- index(aranjuez)

long <- fortify(aranjuez, melt=TRUE)

## Values below mean
scaled <- fortify(scale(aranjuez, scale=FALSE), melt=TRUE)
scaled <- transform(scaled, scaled=Value, Value=long$Value)
underIdx <- which(scaled$scaled <= 0)
under <- scaled[underIdx,]

## Minima and maxima
minIdx <- timeIdx[apply(aranjuez, 2, which.min)]
minVals <- apply(aranjuez, 2, min, na.rm=TRUE)
mins <- data.frame(Index=minIdx,
                   Value=minVals,
                   Series=names(aranjuez))

maxIdx <- timeIdx[apply(aranjuez, 2, which.max)]
maxVals <- apply(aranjuez, 2, max, na.rm=TRUE)
maxs <- data.frame(Index=maxIdx,
                   Value=maxVals,
                   Series=names(aranjuez))

## Year bands
ep <- endpoints(timeIdx, on='years')
N <- length(ep[-1])
tep <- timeIdx[ep]
tsp <- timeIdx[ep[-(N+1)]+1]
cols <- rep_len(c('gray', 'white'), N)

ggplot(data=long, aes(Index, Value), group=Series) +
    geom_line(colour = "royalblue4", lwd = 0.5) +
    annotate(geom='rect', ymin = -Inf, ymax = Inf,
              xmin=tsp, xmax=tep,
              fill = cols, alpha = 0.4) +
    geom_rug(data=under,
             sides='b', col='indianred1') +
    geom_point(data=mins, pch=25) +
    geom_point(data=maxs, pch=24) +
    labs(x='Time', y=NULL) +
    facet_free() +
    theme_bw()
    
