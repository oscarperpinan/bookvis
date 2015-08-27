panel.flow <- function(x, y, groups, origin,...){
  dat <- data.frame(x=x, y=y, groups=groups)
  nVars <- nlevels(groups)
  groupLevels <- levels(groups)

  yy <- unstack(dat, y~groups)
  if (origin=='themeRiver') origin= -1/2*rowSums(yy) else origin=0 ##Havre.Hetzler.ea2002
  yy <- cbind(origin=origin, yy)
  yyy <- t(apply(yy, 1, cumsum))
  Y <- as.data.frame(sapply(seq_len(nVars), function(iCol)c(yyy[,iCol+1], rev(yyy[,iCol]))))
  names(Y) <- levels(groups)
  y <- stack(Y)$values

  xx <- unstack(dat, x~groups)
  x <- rep(c(xx[,1], rev(xx[,1])), nVars)

  groups <- rep(groups, each=2)
  
  superpose.polygon <- trellis.par.get("superpose.polygon")

  col = superpose.polygon$col
  col.line = superpose.polygon$col
  border = superpose.polygon$border 
  lty = superpose.polygon$lty
  lwd = superpose.polygon$lwd 
  alpha = superpose.polygon$alpha

  for (i in seq_len(nVars)){
    xx <- x[groups==groupLevels[i]]
    yy <- y[groups==groupLevels[i]]
    panel.polygon(xx, yy, border="black", lwd=0.6, col=col[i])
    N <- length(xx)/2
    panel.text(xx[N], (yy[N]+yy[N+1])/2, labels=groupLevels[i],
               col=col[i], cex=0.7, ##fontface='bold', 
               pos=4, offset=0.6)
  }
  ## panel.superpose(x, y, ..., groups=groups, panel.groups=panel.polygon,
  ##                 col=col, col.line=col.line, lty=lty, lwd=lwd,
  ##                 border=border, alpha=alpha)
}

prepanel.default.flow <- function(x, y, groups, origin,...){
  dat <- data.frame(x=x, y=y, groups=groups)
  nVars <- nlevels(groups)
  groupLevels <- levels(groups)
  yy <- unstack(dat, y~groups)
  if (origin=='themeRiver') origin= -1/2*rowSums(yy) else origin=0
  yy <- cbind(origin=origin, yy)
  yyy <- t(apply(yy, 1, cumsum))

  list(xlim=c(min(x), max(x)))
       ylim=c(min(yyy[,1]), max(yyy[,nVars+1])),
       dx=diff(x),
       dy=diff(c(yyy[,-1])))
}


idx <- seq(as.Date('2011-01-01'), by='day', length=365)
z <- zoo(data.frame(abc=runif(365), bbbbvvvvvvvb=1, cd=1), idx)

y <- c(abs(runif(365)), rep(1, 365), rep(1, 365))
x <- rep(as.numeric(idx), 3)
groups <- factor(rep(names(z), each=365))

prepanel.default.flow(x, y, groups, origin=0)

xyplot(z, superpose=TRUE, panel=panel.flow, prepanel=prepanel.default.flow,
       origin='themeRiver', scales=list(y=list(draw=FALSE)),
       par.settings=custom.theme.2, auto.key=FALSE)



## i=2
## lag = 4
## iMed <- length(yy)/2
## ii <- 1:(N-lag)
## X <- xx[1:iMed]
## difY <- smooth(yy[1:iMed] - yy[length(yy):(iMed+1)])
## A <- sapply(1:(N-lag), function(i)lag/2*(difY[i] + 4*difY[i+lag/2]+difY[i+lag])) ##simpson rule
## iMaxArea <- which.max(A)
## xMaxArea <- xx[iMaxArea] ## - difX[iMaxArea]/2
## yMaxArea <- yy[iMaxArea] - difY[iMaxArea]/2
## panel.text(xMaxArea, yMaxArea, labels=groupLevels[i])


## difX <- diff(xx[1:iMed])

## N <- length(difY)
## sumDifY <- difY[1:(N-1)] + difY[2:N]
## A <- sumDifY*difX/2 ## area del trapecio
## iMaxArea <- which.max(A)
## xMaxArea <- xx[iMaxArea]
## yMaxArea <- yy[iMaxArea]


