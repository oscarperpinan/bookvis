setwd('~/Dropbox/chapman/book/')
## wbData <- read.csv('data/BancoMundial.csv')



## wbData2 <- reshape(wbData, varying=list(names(wbData)[5:54]),
##                       timevar='Year', v.names='Value',
##                       times=1962:2011,
##                       direction='long')
## wbData2 <- wbData2[, c(1, 4, 5, 6)]
## wbData3 <- reshape(wbData2, 
##                    idvar=c('Country.Name','Year'),
##                    timevar='Indicator.Code', direction='wide')

## euro <- c('Finland', 'France', 'Greece', 'Spain')

## ## xyplot(Value.NY.GNP.PCAP.KN~Value.SE.PRM.TCHR, groups=Country.Name,
## ##        data=wbData3,
## ##        subset=((Country.Name %in% euro) & (Year >1999)),
## ##        panel=function(x, y, groups,...,subscripts){
## ##                    panel.xyplot(x, y, col=groups[subscripts],...)
## ##                    panel.text(x, y, labels=wbData3$Year[subscripts], cex=0.65, pos=1, ...)
## ##                    }
## ##        )
## euroData <- subset(wbData3, subset=((Country.Name %in% euro) & (Year >1999)))
## euroData <- droplevels(euroData)
## xyplot(Value.NY.GNP.PCAP.KN  ~ Value.SE.XPD.PRIM.PC.ZS,
##        groups=Country.Name,
##        data=euroData,
##        panel=panel.superpose,
##        type='b', auto.key=list(space='right'),
##        panel.groups=function(..., groups, subscripts){
##                    panel.xyplot(...)
##                    panel.text(..., labels=euroData$Year[subscripts], pos=1)
##                    })

## pPrim <- xyplot(Value.NY.GNP.PCAP.KN  ~ Value.SE.XPD.PRIM.PC.ZS,
##        groups=Country.Name,
##        data=euroData,
##        type='b')

## pSec <- xyplot(Value.NY.GNP.PCAP.KN  ~ Value.SE.XPD.SECO.PC.ZS,
##        groups=Country.Name,
##        data=euroData,
##        type='b')

## pPrim +
##   glayer(panel.text(..., labels=euroData$Year[subscripts], pos=1)) +
##   glayer(panel.text(x[2], y[2], labels= group.value, pos=2, offset=2))

## pSec +
##   glayer(panel.text(..., labels=euroData$Year[subscripts], pos=1)) +
##   glayer(panel.text(x[2], y[2], labels= group.value, pos=2, offset=2))



CO2 <- read.csv('data/CO2_GNI_BM.csv')
CO2data <- reshape(CO2, varying=list(names(CO2)[5:16]),
                      timevar='Year', v.names='Value',
                      times=2000:2011,
                      direction='long')

CO2data <- CO2data[, c(1, 3, 5, 6)]
CO2data <- reshape(CO2data, 
                   idvar=c('Country.Name','Year'),
                   timevar='Indicator.Name', direction='wide')

names(CO2data)[3:6] <- c('CO2.PPP', 'CO2.capita', 'GNI.PPP', 'GNI.capita')

nCountries <- nlevels(CO2data$Country.Name)
pal <- colorRampPalette(brewer.pal(n=8, name='Dark2'))(nCountries)

pCO2.capita <- xyplot(GNI.capita  ~ CO2.capita,
                      xlab="CO2 emissions (metric tons per capita)",
                      ylab="GNI per capita, PPP (current international $)",
                      groups=Country.Name, data=CO2data,
                      par.settings=custom.theme(pch=19, symbol=pal),
                      type='b')

pCO2.capita +
  glayer_(panel.text(..., labels=CO2data$Year[subscripts], pos=1, cex=0.6, col='gray')) +
  glayer(panel.text(x[9], y[9], labels= group.value,
                    col=pal[group.number],
                    pos=4, offset=0.7, cex=0.7))


pCO2.PPP <- xyplot(GNI.PPP  ~ CO2.PPP,
                   xlab="CO2 emissions (kg per PPP $ of GDP)",
                   ylab="GNI, PPP (current international $)",
                   groups=Country.Name, data=CO2data,
                   par.settings=custom.theme(pch=19, symbol=pal),
                   type='b')

pCO2.PPP +
  glayer_(panel.text(..., labels=CO2data$Year[subscripts], pos=1, cex=0.6, col='gray')) +
  glayer(panel.text(x[9], y[9], labels= group.value,
                    col=pal[group.number],
                    pos=2, offset=0.7, cex=0.7))


pGNI <- xyplot(GNI.capita  ~ GNI.PPP,
               xlab="GNI, PPP (current international $)",
               ylab="GNI per capita, PPP (current international $)",
               groups=Country.Name, data=CO2data,
               par.settings=custom.theme(pch=19, symbol=pal),
               type='b')

pGNI +
  glayer_(panel.text(..., labels=CO2data$Year[subscripts], pos=2, cex=0.6, col='gray')) +
  glayer(panel.text(x[9], y[9], labels= group.value,
                    col=pal[group.number],
                    pos=3, offset=1, cex=0.7))

### probar directlabels, maptools::pointLabel, plotrix::thigmophobe, Hmisc::labcurve
pCO2.capita +
  glayer({
    xyLab <- pointLabel(..., labels=CO2data$Year[subscripts], doPlot=FALSE, cex=0.6)
    print(xyLab)
##    panel.text(xyLab$x, xyLab$y, labels=CO2data$Year[subscripts], pos=2, cex=0.6, col='gray')
         })
