##################################################################
## Initial configuration
##################################################################
## Clone or download the repository and set the working directory
## with setwd to the folder where the repository is located.

Sys.setlocale("LC_TIME", "C")

##################################################################
## Data and spatial information
##################################################################

## Spatial location of stations
airStations <- read.csv2("data/Spatial/airStations.csv")
## Measurements data
airQuality <- read.csv2("data/Spatial/airQuality.csv")
## Only interested in NO2 
NO2 <- airQuality[airQuality$codParam == 8, ]

##################################################################
## sp and spacetime
##################################################################

library("sp")

airStationsSP <- airStations
## rownames are used as the ID of the Spatial object
rownames(airStationsSP) <- substring(airStationsSP$Code, 7)
coordinates(airStationsSP) <- ~ long + lat
proj4string(airStationsSP) <- CRS("+proj=longlat +ellps=WGS84")

library("zoo")
library("reshape2")
library("spacetime")
  
NO2$time <- as.Date(with(NO2,
                         ISOdate(year, month, day)))

NO2wide <- dcast(NO2[, c('codEst', 'dat', 'time')],
                 time ~ codEst,
                 value.var = "dat")

NO2zoo <- zoo(NO2wide[,-1], NO2wide$time)

dats <- data.frame(vals = as.vector(t(NO2zoo)))

NO2st <- STFDF(sp = airStationsSP,
               time = index(NO2zoo),
               data = dats)

##################################################################
## sf and sftime
##################################################################

library(sftime)

airStationsSF <- st_as_sf(airStations,
                          coords = c("long", "lat"),
                          crs = 4326)

NO2$time <- as.Date(with(NO2,                       
                         ISOdate(year, month, day)))

idx <- match(NO2$codEst, airStationsSF$Code)

NO2sft <- st_sftime(dat = NO2$dat, code = NO2$codEst,
                    geometry = airStationsSF$geometry[idx],
                    time = NO2$time)

##################################################################
## Graphics with spacetime and sftime
##################################################################

airPal <- colorRampPalette(c("springgreen1", "sienna3", "gray5"))(5)
  
stplot(NO2st[, 1:12],
       cuts = 5,
       col.regions = airPal,
       main = "",
       edge.col = "black")

library(ggplot2)

airPal <- colorRampPalette(c("springgreen1", "sienna3", "gray5"))(5)

ggplot(NO2sft) + 
  geom_sf(aes(color = dat)) +
  facet_wrap(~ cut(time, 12)) +
  scale_colour_stepsn(n.breaks = 6, colours = airPal)

stplot(NO2st, mode = "xt",
       col.regions = colorRampPalette(airPal)(15),
       scales = list(x = list(rot = 45)),
       ylab = "", xlab = "", main = "")

ggplot(NO2sft) + 
  geom_tile(aes(x = as.factor(code),
                y = time,
                fill = dat)) +
  scale_fill_stepsn(colours = airPal,
                    n.breaks = 6) +
  xlab("Station Code") + ylab("") +
  guides(x = guide_axis(angle = 45))

stplot(NO2st, mode = "ts",
       xlab = "",
       lwd = 0.1, col = "black", alpha = 0.6,
       auto.key = FALSE)

ggplot(NO2sft) +
  geom_line(aes(x=time, y = dat),
            colour = "black",
            linewidth = 0.25,
            alpha = 0.6) +
  theme_bw() + xlab("")
