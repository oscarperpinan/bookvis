##################################################################
## Initial configuration
##################################################################
## Clone or download the repository and set the working directory
## with setwd to the folder where the repository is located.

  ##################################################################
  ## Air Quality in Madrid
  ##################################################################

##################################################################
## Retrieve data
##################################################################

airStations <- read.csv2("data/Spatial/airStations.csv")
head(airStations)

airQuality <- read.csv2("data/Spatial/airQuality.csv")
head(airQuality)

##################################################################
## Combine data and spatial locations
##################################################################

library("sf")
  
## Spatial location of stations
airStations <- st_as_sf(airStations,
                        coords = c("long", "lat"),
                        crs = 4326)

NO2 <- subset(airQuality, codParam == 8)

NO2agg <- aggregate(dat ~ codEst, data = NO2,
                    FUN = function(x) {
                      c(mean = signif(mean(x), 3),
                        median = median(x),
                        sd = signif(sd(x), 3))
                    })
NO2agg <- do.call(cbind, NO2agg)
NO2agg <- as.data.frame(NO2agg)

## Link aggregated data with stations to obtain a sf object
## Code and codEst are the stations codes
idxNO2 <- match(airStations$Code, NO2agg$codEst)
NO2sf <- cbind(airStations[, c("Name", "alt")],
               NO2agg[idxNO2, ])

## Save the result
st_write(NO2sf, dsn = "data/Spatial/", layer = "NO2sf",
         driver = "ESRI Shapefile")

##################################################################
## Spanish General Elections
##################################################################

dat2016 <- read.csv("data/Spatial/GeneralSpanishElections2016.csv")

population <- dat2016$Población
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
whichMax[!(whichMax %in% c("PP", "PSOE", "UP", "C.s", "ABS"))] <- "OTH"
## Percentage of votes with the electoral census
pcMax <- Max/census * 100

## Province-Municipality code. sprintf formats a number with leading zeros.
PROV <- sprintf("%02d", dat2016$Código.de.Provincia)
MUN <- sprintf("%03d", dat2016$Código.de.Municipio)
PROVMUN <- paste0(PROV, MUN)
                          
votes2016 <- data.frame(PROV, MUN, PROVMUN,
                        population, census, validVotes,
                        whichMax, Max, pcMax)

write.csv(votes2016, "data/Spatial/votes2016.csv", row.names = FALSE)

##################################################################
## Administrative boundaries
##################################################################

library("sf")

old <- setwd(tempdir())

download.file("https://www.ine.es/pcaxis/mapas_completo_municipal.zip",
              "mapas_completo_municipal.zip")
unzip("mapas_completo_municipal.zip")

sfMun <- st_read("esp_muni_0109.shp", crs = 25830,
                 stringsAsFactors = TRUE)
sfMun <- subset(sfMun, !is.na(sfMun$PROVMUN))

setwd(old)

votes2016 <- read.csv("data/Spatial/votes2016.csv",
                      colClasses = c("factor", "factor", "factor",
                                     "numeric", "numeric", "numeric",
                                     "factor", "numeric", "numeric"))
## Match polygons and data with the PROVMUN column
idx <- match(sfMun$PROVMUN, votes2016$PROVMUN)
  
##Places without information
idxNA <- which(is.na(idx))

##Information to be added to the sf object
dat2add <- votes2016[idx, c("PROV", "population", "census", "validVotes",
                            "whichMax", "Max", "pcMax")]

## Spatial object with votes data
sfMapVotes <- cbind(sfMun, dat2add)

## Drop those places without information
sfMapVotes0 <- sfMapVotes[-idxNA, ]

## Save the result
st_write(sfMapVotes0, "data/Spatial/sfMapVotes0.shp")

## Extract Canarias islands from the sf object
canarias <-  substr(sfMapVotes0$PROVMUN, 1, 2) %in% c("35",  "38")
peninsula <- sfMapVotes0[!canarias,]
island <- sfMapVotes0[canarias,]

## Shift the island extent box to position them at the bottom right corner
dbbox <- st_bbox(peninsula) - st_bbox(island)
dxy <- dbbox[c("xmax", "ymin")]
island$geometry <- island$geometry + dxy

## Bind Peninsula (without islands) with shifted islands
st_crs(island) <- st_crs(peninsula)
sfMapVotes <- rbind(peninsula, island)

## Save the result
st_write(sfMapVotes, "data/Spatial/sfMapVotes.shp", append = FALSE)

##################################################################
##   GDP and Population
##################################################################

## Population of each province
popSpain <- read.csv("data/SpatioTime/PopSpain.csv")
popSpain2020 <- subset(popSpain, Year == 2020)
popSpain2020$PROV <- substring(popSpain2020$Province, 1, 2)

## GDP of each province
GDPSpain2020 <- read.csv("data/Spatial/GDPSpain2020.csv")
GDPSpain2020$PROV <- substring(GDPSpain2020$Province, 1, 2)

popGDPSpain2020 <- merge(popSpain2020, GDPSpain2020[, c("PROV", "GDP")])

library("sf")

sfProv <- st_read("data/Spatial/spain_provinces_2.shp", crs = 25830,
                 stringsAsFactors = TRUE)

## Merge data with the polygons
sfPopGDPSpain <- merge(sfProv, popGDPSpain2020,
                      by = "PROV")

st_write(sfPopGDPSpain, "data/Spatial/sfPopGDPSpain.shp")

  ##################################################################
  ## CM SAF
  ##################################################################

library("raster")
  
tmp <- tempdir()
unzip("data/Spatial/SISmm2008_CMSAF.zip", exdir = tmp)
filesCMSAF <- dir(tmp, pattern = "SISmm")
SISmm <- stack(paste(tmp, filesCMSAF, sep = "/"))
## CM-SAF data is average daily irradiance (W/m2). Multiply by 24
## hours to obtain daily irradiation (Wh/m2)
SISmm <- SISmm * 24

## Monthly irradiation: each month by the corresponding number of days
daysMonth <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
SISm <- SISmm * daysMonth / 1000 ## kWh/m2
## Annual average
SISav <- sum(SISm)/sum(daysMonth)
writeRaster(SISav, file = "data/Spatial/SISav.nc")

library("raster")
## https://neo.gsfc.nasa.gov/view.php?datasetId=SEDAC_POP
pop <- raster("data/Spatial/875430rgb-167772161.0.FLOAT.TIFF")
## https://neo.gsfc.nasa.gov/view.php?datasetId=MCD12C1_T1
landClass <- raster("data/Spatial/241243rgb-167772161.0.TIFF")
