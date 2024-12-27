##################################################################
## Initial configuration
##################################################################
## Clone or download the repository and set the working directory
## with setwd to the folder where the repository is located.
Sys.setlocale("LC_TIME", "C")

##################################################################
## Data and spatial information
##################################################################

library("sf")

library("cartogram")

library("gganimate")

covidSpain <- read.csv("data/SpatioTime/covid.csv",
                       na.strings = NULL)
covidSpain$PROV <- sprintf("%02d", covidSpain$PROV)
covidSpain$day <- as.Date(covidSpain$day)

covidSpain <- subset(covidSpain,
                     day < as.Date("2022-03-01") &
                     day >= as.Date("2021-12-01"))

## Population of each province
popSpain <- read.csv("data/SpatioTime/PopSpain.csv")
popSpain$PROV <- substring(popSpain$Province, 1, 2)

popSpain2021 <- subset(popSpain,
                       Year == 2021,
                       select = c(PROV, Population))

covidSpain <- merge(covidSpain, popSpain2021, by = "PROV")

## Number of cases per 100.000 population
covidSpain$cases_rel <- with(covidSpain,
                             num_cases/Population * 1e5)

ggplot(covidSpain) +
  geom_raster(aes(x = day,
                  y = PROV,
                  fill = cases_rel)) +
  scale_fill_distiller(palette = "PuBu", direction = 1) +
  theme_bw()

sfProv <- st_read("data/Spatial/spain_provinces_2.shp",
                  crs = 25830,
                  stringsAsFactors = TRUE)

## Merge data with the polygons
sfCovid <- merge(sfProv, covidSpain,
                      by = "PROV")

ggplot(subset(sfCovid,
              day >= as.Date("2022-01-01") &
              day <= as.Date("2022-01-09"))) +
  geom_sf(aes(fill = cases_rel)) +
  scale_fill_distiller(palette = "PuBu", direction = 1) +
  theme_bw() +
  facet_wrap(~ day, nrow = 3)

ggCovid <- ggplot(sfCovid) +
  geom_sf(aes(fill = cases_rel)) +
  scale_fill_distiller(palette = "PuBu", direction = 1) +
  theme_bw() +
  ggtitle("{format(frame_time, format = '%Y-%m-%d')}") +
  transition_time(time = day)


animate(ggCovid,
        height = 1080, width = 1080,
        res = 150,
        units = "px")

fday <- as.Date("2022-01-01")
lday <- as.Date("2022-02-28")

days <- seq(fday, lday, by = "day")

cartCOVIDList <- lapply(days, function(d)
{
  x <- subset(sfCovid, day == d)
  cartogram_ncont(x,
                  weight = "cases_rel")
})
cartCOVID <- do.call(rbind, cartCOVIDList)

ggplot(subset(cartCOVID,
              day >= as.Date("2022-01-01") &
              day <= as.Date("2022-01-09"))) +
  geom_sf(data = sfProv) +
  geom_sf(aes(fill = cases_rel)) +
  scale_fill_distiller(palette = "PuBu", direction = 1) +
  theme_bw() +
  facet_wrap(~ day, nrow = 3)

ggCartCOVID <- ggplot(cartCOVID) +
  geom_sf(data = sfProv) +
  geom_sf(aes(fill = cases_rel, group = day)) +
  scale_fill_distiller(palette = "PuBu", direction = 1) +
  ggtitle("{format(frame_time)}") + 
  transition_time(day) + 
  theme_bw()


animate(ggCartCOVID,
        height = 1080, width = 1080,
        res = 150,
        units = "px")
