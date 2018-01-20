##################################################################
## Initial configuration
##################################################################
## Clone or download the repository and set the working directory
## with setwd to the folder where the repository is located.

  ##################################################################
  ## Air Quality in Madrid
  ##################################################################

  ## codeStations.csv is extracted from the document
  ## http://www.mambiente.munimadrid.es/opencms/export/sites/default/calaire/Anexos/INTPHORA-DIA.pdf,
  ## table of page 3.
  
  codEstaciones <- read.csv2('data/codeStations.csv')
  codURL <- as.numeric(substr(codEstaciones$Codigo, 7, 8))
  
  ## The information of each measuring station is available at its own webpage, defined by codURL
  URLs <- paste('http://www.mambiente.munimadrid.es/opencms/opencms/calaire/contenidos/estaciones/estacion', codURL, '.html', sep='')

##################################################################
## Data arrangement
##################################################################

  library(XML)
  library(sp)
  
  ## Access each webpage, retrieve tables and extract long/lat data
  coords <- lapply(URLs, function(est){
    tables <- readHTMLTable(est)
    location <- tables[[2]]
    ## Clean the table content and convert to dms format
    ub2dms <- function(x){
      ch <- as.character(x)
      ch <- sub(',', '.', ch) 
      ch <- sub('O', 'W', ch) ## Some stations use "O" instead of "W"
      as.numeric(char2dms(ch, "º", "'", "'' "))
    }
    long <- ub2dms(location[2,1])
    lat <- ub2dms(location[2,2])
    alt <- as.numeric(sub(' m.', '', location[2, 3]))
  
    coords <- data.frame(long=long, lat=lat, alt=alt)
  
    coords
  })
  
  airStations <- cbind(codEstaciones, do.call(rbind, coords))
  
  ## The longitude of "El Pardo" station is wrong (positive instead of negative)
  airStations$long[22] <- -airStations$long[22]
  
  write.csv2(airStations, file='data/airStations.csv')

rawData <- readLines('data/Datos11.txt')
## This loop reads each line and extracts fields as defined by the
## INTPHORA file:
## http://www.mambiente.munimadrid.es/opencms/export/sites/default/calaire/Anexos/INTPHORA-DIA.pdf
datos11 <- lapply(rawData, function(x){
    codEst <- substr(x, 1, 8)
    codParam <- substr(x, 9, 10)
    codTec <- substr(x, 11, 12)
    codPeriod <- substr(x, 13, 14)
    month <- substr(x, 17, 18)
    dat <- substr(x, 19, nchar(x))
    ## "N" used for impossible days (31st April)
    idxN <- gregexpr('N', dat)[[1]]
    if (idxN==-1) idxN <- numeric(0)
    nZeroDays <- length(idxN)
    day <- seq(1, 31-nZeroDays)
    ## Substitute V and N with ";" to split data from different days
    dat <- gsub('[VN]+', ';', dat)
    dat <- as.numeric(strsplit(dat, ';')[[1]])
    ## Only data from valid days
    dat <- dat[day]
    res <- data.frame(codEst, codParam, ##codTec, codPeriod,
                      month, day, year = 2016,
                      dat)
})
datos11 <- do.call(rbind, datos11)

write.csv2(datos11, 'data/airQuality.csv')

##################################################################
## Combine data and spatial locations
##################################################################

  library(sp)
  
  ## Spatial location of stations
  airStations <- read.csv2('data/airStations.csv')
  coordinates(airStations) <- ~ long + lat
  ## Geographical projection
  proj4string(airStations) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

  ## Measurements data
  airQuality <- read.csv2('data/airQuality.csv')
  ## Only interested in NO2 
  NO2 <- airQuality[airQuality$codParam==8, ]

  NO2agg <- aggregate(dat ~ codEst, data=NO2,
                      FUN = function(x) {
                          c(mean=signif(mean(x), 3),
                            median=median(x),
                            sd=signif(sd(x), 3))
                          })
  NO2agg <- do.call(cbind, NO2agg)
  NO2agg <- as.data.frame(NO2agg)

library(rgdal)
library(maptools)
## Link aggregated data with stations to obtain a SpatialPointsDataFrame.
## Codigo and codEst are the stations codes
idxNO2 <- match(airStations$Codigo, NO2agg$codEst)
NO2sp <- spCbind(airStations[, c('Nombre', 'alt')], NO2agg[idxNO2, ])
## Save the result
writeOGR(NO2sp, dsn = 'data/', layer = 'NO2sp',
         driver = 'ESRI Shapefile')

##################################################################
## Photographs of the stations
##################################################################

library(XML)

old <- setwd('images')
for (i in 1:nrow(NO2df))
{
    codEst <- NO2df[i, "codEst"]
    ## Webpage of each station
    codURL <- as.numeric(substr(codEst, 7, 8))
    rootURL <- 'http://www.mambiente.munimadrid.es'
    stationURL <- paste(rootURL,
                        '/opencms/opencms/calaire/contenidos/estaciones/estacion',
                        codURL, '.html', sep='')
    content <- htmlParse(stationURL, encoding='utf8')
    ## Extracted with http://www.selectorgadget.com/
    xPath <- '//*[contains(concat( " ", @class, " " ), concat( " ", "imagen_1", " " ))]'
    imageStation <- getNodeSet(content, xPath)[[1]]
    imageURL <- xmlAttrs(imageStation)[1]
    imageURL <- paste(rootURL, imageURL, sep='')
    download.file(imageURL, destfile=paste(codEst, '.jpg', sep=''))
}
setwd(old)

##################################################################
## Spanish General Elections
##################################################################

dat2016 <- read.csv('data/GeneralSpanishElections2016.csv')

census <- dat2016$Total.censo.electoral
validVotes <- dat2016$Votos.válidos
## Election results per political party and municipality
votesData <- dat2016[, -(1:13)]
## Abstention as an additional party
votesData$ABS <- census - validVotes
## UP is a coalition of several parties
UPcols <- grep("PODEMOS|ECP", names(votesData))
votesData$UP <- rowSums(votesData[, UPcols])
votesData[, UPcols] <- NULL
## Winner party at each municipality
whichMax <- apply(votesData,  1, function(x)names(votesData)[which.max(x)])
## Results of the winner party at each municipality
Max <- apply(votesData, 1, max)
## OTH for everything but PP, PSOE, UP, Cs, and ABS
whichMax[!(whichMax %in% c('PP', 'PSOE', 'UP', 'C.s', 'ABS'))] <- 'OTH'
## Percentage of votes with the electoral census
pcMax <- Max/census * 100

## Province-Municipality code. sprintf formats a number with leading zeros.
PROVMUN <- with(dat2016, paste(sprintf('%02d', Código.de.Provincia),
                               sprintf('%03d', Código.de.Municipio),
                               sep=""))

votes2016 <- data.frame(PROVMUN, whichMax, Max, pcMax)
write.csv(votes2016, 'data/votes2016.csv', row.names=FALSE)

##################################################################
## Administrative boundaries
##################################################################

library(sp)
library(rgdal)

old <- setwd(tempdir())

download.file('ftp://www.ine.es/pcaxis/mapas_completo_municipal.rar',
              'mapas_completo_municipal.rar')
system2('unrar', c('e', 'mapas_completo_municipal.rar'))

spMap <- readOGR("esp_muni_0109.shp",
                 p4s = "+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs")
Encoding(levels(spMap$NOMBRE)) <- "latin1"

setwd(old)

## dissolve repeated polygons
spPols <- unionSpatialPolygons(spMap, spMap$PROVMUN)

votes2016 <- read.csv('data/votes2016.csv',
                        colClasses=c('factor', 'factor', 'numeric', 'numeric'))

## Match polygons and data using ID slot and PROVMUN column
IDs <- sapply(spPols@polygons, function(x)x@ID)
idx <- match(IDs, votes2016$PROVMUN)
  
##Places without information
idxNA <- which(is.na(idx))

##Information to be added to the SpatialPolygons object
dat2add <- votes2016[idx, ]

## SpatialPolygonsDataFrame uses row names to match polygons with data
row.names(dat2add) <- IDs
spMapVotes <- SpatialPolygonsDataFrame(spPols, dat2add)

## Drop those places without information
spMapVotes0 <- spMapVotes[-idxNA, ]

## Save the result
writeOGR(spMapVotes0, dsn = 'data/', layer = 'spMapVotes0',
         drive = 'ESRI Shapefile')

## Extract Canarias islands from the SpatialPolygons object
canarias <-  sapply(spMapVotes0@polygons, function(x)substr(x@ID, 1, 2) %in% c("35",  "38"))
peninsula <- spMapVotes0[!canarias,]
island <- spMapVotes0[canarias,]

## Shift the island extent box to position them at the bottom right corner
dy <- bbox(peninsula)[2,1] - bbox(island)[2,1]
dx <- bbox(peninsula)[1,2] - bbox(island)[1,2]
island2 <- elide(island, shift = c(dx, dy))
bbIslands <- bbox(island2)
proj4string(island2) <- proj4string(peninsula)

## Bind Peninsula (without islands) with shifted islands
spMapVotes <- rbind(peninsula, island2)

## Save the result
writeOGR(spMapVotes, dsn = 'data/', layer = 'spMapVotes',
         drive = 'ESRI Shapefile')

  ##################################################################
  ## CM SAF
  ##################################################################

  library(raster)
  
  tmp <- tempdir()
  unzip('data/SISmm2008_CMSAF.zip', exdir=tmp)
  filesCMSAF <- dir(tmp, pattern='SISmm')
  SISmm <- stack(paste(tmp, filesCMSAF, sep='/'))
  ## CM-SAF data is average daily irradiance (W/m2). Multiply by 24
  ## hours to obtain daily irradiation (Wh/m2)
  SISmm <- SISmm * 24

  ## Monthly irradiation: each month by the corresponding number of days
  daysMonth <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  SISm <- SISmm * daysMonth / 1000 ## kWh/m2
  ## Annual average
  SISav <- sum(SISm)/sum(daysMonth)
  writeRaster(SISav, file='SISav')

 library(raster)
 ## http://neo.sci.gsfc.nasa.gov/Search.html?group=64
 pop <- raster('875430rgb-167772161.0.FLOAT.TIFF')
 ## http://neo.sci.gsfc.nasa.gov/Search.html?group=20
 landClass <- raster('241243rgb-167772161.0.TIFF')
