##################################################################
## SIAR
##################################################################

##################################################################
## Daily data of different meteorological variables 
##################################################################

library(zoo)
  
aranjuez <- read.zoo("data/aranjuez.gz",
                     index.column = 3, format = "%d/%m/%Y",
                     fileEncoding = 'UTF-16LE',
                     header = TRUE, fill = TRUE,
                     sep = ';', dec = ",", as.is = TRUE)
aranjuez <- aranjuez[, -c(1:4)]
  
names(aranjuez) <- c('TempAvg', 'TempMax', 'TempMin',
                     'HumidAvg', 'HumidMax',
                     'WindAvg', 'WindMax',
                     'Radiation', 'Rain', 'ET')
  
  
summary(aranjuez)

aranjuezClean <- within(as.data.frame(aranjuez),{
    TempMin[TempMin > 40] <- NA
    HumidMax[HumidMax > 100] <- NA
})

aranjuez <- zoo(aranjuezClean, index(aranjuez))

summary(aranjuez)

save(aranjuez, file = 'data/aranjuez.RData')

##################################################################
## Solar radiation measurements from different locations
##################################################################

library(zoo)

load('data/navarra.RData')

summary(navarra)

##################################################################
## Unemployment in the United States
##################################################################

unemployUSA <- read.csv('data/unemployUSA.csv')
nms <- unemployUSA$Series.ID
##columns of annual summaries
annualCols <- 14 + 13*(0:12)
## Transpose. Remove annual summaries
unemployUSA <- as.data.frame(t(unemployUSA[,-c(1, annualCols)]))
## First 7 characters can be suppressed
names(unemployUSA) <- substring(nms, 7)

summary(unemployUSA)

library(zoo)
  
Sys.setlocale("LC_TIME", 'C')
idx <- as.yearmon(row.names(unemployUSA), format = '%b.%Y')
unemployUSA <- zoo(unemployUSA, idx)

unemployUSA <- unemployUSA[complete.cases(unemployUSA), ]

summary(unemployUSA)

save(unemployUSA, file = 'data/unemployUSA.RData')

##################################################################
## Gross National Income and $CO_2$ emissions
##################################################################

library(WDI)
    
CO2data <- WDI(indicator = c('EN.ATM.CO2E.PC', 'EN.ATM.CO2E.PP.GD',
                           'NY.GNP.MKTP.PP.CD', 'NY.GNP.PCAP.PP.CD'),
               start = 2000, end = 2014,
               country = c('BR', 'CN', 'DE',
                         'ES', 'FI', 'FR',
                         'GR', 'IN', 'NO',
                         'US'))

names(CO2data) <- c('iso2c', 'Country.Name', 'Year',
                    'CO2.capita', 'CO2.PPP',
                    'GNI.PPP', 'GNI.capita')

summary(CO2data)

CO2data <- CO2data[complete.cases(CO2data), ]

CO2data$Country.Name <- factor(CO2data$Country.Name)

summary(CO2data)

save(CO2data, file = 'data/CO2.RData')
