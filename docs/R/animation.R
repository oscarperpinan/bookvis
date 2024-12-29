##################################################################
## Initial configuration
##################################################################
## Clone or download the repository and set the working directory
## with setwd to the folder where the repository is located.

Sys.setlocale("LC_TIME", 'C')

##################################################################
## Time trajectory
##################################################################

library("sf")
library("move2")
library("units") 

library("rnaturalearth")

library("ggplot2")
library("gganimate")

## Movebank data
birds0 <- movebank_download_study(2398637362,
                                 "license-md5"="74263192947ce529c335a0ae72d7ead7")

sf_use_s2(FALSE) ## Needed for st_crop to work
## Natural Earth boundaries
boundaries <- ne_countries(scale = "large")
boundaries <- st_crop(boundaries, birds0)

## Filter the data: speed higher than 2 m/s; remove year 2022 data.
birds <- subset(birds0,
                ground_speed > set_units(2L, "m/s") &
                timestamp >= as.POSIXct("2023-01-01"))

## Add a column with month values
birds$month <- as.numeric(format(mt_time(birds), "%m"))

ggplot() +
    geom_sf(data = boundaries) +
    geom_sf(data = birds,
            aes(color = individual_local_identifier),
            alpha = 0.1) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) + 
    theme_linedraw() +
    facet_wrap(~ month, nrow = 2)

birds$speed <- cut(birds$ground_speed,
                   breaks = c(2, 5, 10, 15, 35))
ggplot() +
    coord_polar(start = 0) +
    geom_histogram(data = birds,
                   aes(x = set_units(heading, "degrees"),
                       fill = speed),
                   breaks = set_units(seq(0, 360, by = 10L), "degrees"),
                   position = position_stack(reverse = TRUE)) +
    scale_x_units(name = NULL,
                  limits = set_units(c(0L, 360), "degrees"),
                  breaks = (0:4) * 90L) +
    ylab("") +
    facet_wrap(~ month, nrow = 2) +
    scale_fill_ordinal("Speed") +
    theme_linedraw()

birdsMarch <- subset(birds,
                      month == 3)
p <- ggplot() +
    geom_sf(data = boundaries) +
    geom_sf(data = birdsMarch,
            aes(colour = individual_local_identifier),
            size = 3) +
    theme_bw() +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("{format(frame_time, format = '%Y-%m-%d %H:%M:%S')}") +
    transition_time(timestamp) +
    shadow_wake(0.8)

animate(p, nframes = 300)

##################################################################
## Fly-by animation
##################################################################

library("rgl")
library("magick") ## needed to import the texture

## Opens the OpenGL device with a black background
open3d()
bg3d("black")

## XYZ coordinates of a sphere
lat <- seq(-90, 90, len = 100) * pi/180
long <- seq(-180, 180, len = 100) * pi/180
r <- 6378.1 # radius of Earth in km
x <- outer(long, lat, FUN = function(x, y) r * cos(y) * cos(x))
y <- outer(long, lat, FUN = function(x, y) r * cos(y) * sin(x))
z <- outer(long, lat, FUN = function(x, y) r * sin(y))

## Read, scale, and convert the image
nightLightsJPG <- image_read("https://eoimages.gsfc.nasa.gov/images/imagerecords/79000/79765/dnb_land_ocean_ice.2012.13500x6750.jpg")
nightLightsJPG <- image_scale(nightLightsJPG, "8192") ## surface3d reads files up to 8192x8192
nightLights <- image_write(nightLightsJPG, tempfile(),
                           format = "png") ## Only the png format is supported
## Display the sphere with the image superimposed
surface3d(-x, -z, y,
          texture = nightLights,
          specular = "black", col = "white")

rglwidget()

cities <- rbind(c("Madrid", "Spain"),
                c("Tokyo", "Japan"),
                c("Sidney", "Australia"),
                c("Sao Paulo", "Brazil"),
                c("New York", "USA"))
cities <- as.data.frame(cities)
names(cities) <- c("city", "country")

library("osmdata")

geocode <- function(x) {
  place <- paste(x, collapse = ", ")
  bb <- getbb(place)
  center <- c(sum(bb[1, ])/2, sum(bb[2, ]/2))
  center
}

points <- apply(cities, 1, geocode)
points <- t(points)
colnames(points) <- c("lon", "lat")

cities <- cbind(cities, points)

library("geosphere")

## When arriving or departing include a progressive zoom with 100
## frames
zoomIn <- seq(.3, .1, length = 100)
zoomOut <- seq(.1, .3, length = 100)

## First point of the route
route <- data.frame(lon = cities[1, "lon"],
                    lat = points[1, "lat"],
                    zoom = zoomIn,
                    name = cities[1, "city"],
                    action = "arrive")

## This loop visits each location included in the "points" set
## generating the route.
for (i in 1:(nrow(cities) - 1)) {

  p1 <- cities[i,]
  p2 <- cities[i + 1,] 
  ## Initial location
  departure <- data.frame(lon = p1$lon,
                          lat = p1$lat,
                          zoom = zoomOut,
                          name = p1$city,
                          action = "depart")
  
  ## Travel between two points: Compute 100 points between the
  ## initial and the final locations.
  routePart <- gcIntermediate(p1[, c("lon", "lat")],
                              p2[, c("lon", "lat")],
                              n = 100)
  routePart <- data.frame(routePart)
  routePart$zoom <- 0.3
  routePart$name <- ""
  routePart$action <- "travel"
  
  ## Final location
  arrival <- data.frame(lon = p2$lon,
                        lat = p2$lat,
                        zoom = zoomIn,
                        name = p2$city,
                        action = "arrive")
  ## Complete route: initial, intermediate, and final locations.
  routePart <- rbind(departure, routePart, arrival)
  route <- rbind(route, routePart)
}

## Close the travel
route <- rbind(route,
               data.frame(lon = cities[i + 1, "lon"],
                          lat = cities[i + 1, "lat"],
                          zoom = zoomOut,
                          name = cities[i+1, "city"],
                          action = "depart"))

summary(route)

## Function to move the viewpoint in the RGL scene according to the
## information included in the route (position and zoom).
travel <- function(tt) {
  point <- route[tt,]
  view3d(theta = -90 + point$lon,
         phi = point$lat,
         zoom = point$zoom)
  par3d()
}

## Example of usage of travel
## Frame no.1200
travel(1200)
rgl.snapshot("figs/SpatioTime/rgl_travel1200.png")

movie3d(travel,
        duration = nrow(route),
        startTime = 1, fps = 1,
        type = "mp4", clean = FALSE,
        webshot = FALSE)

##################################################################
## Hill shading animation
##################################################################

library("rayshader")
library("parallel")
library("suntools")
library("raster")
library("gifski")

demCedeira <- raster('data/Spatial/demCedeira')
DEM <- raster_to_matrix(demCedeira)

water <- detect_water(DEM)

lonlat <- matrix(c((xmax(demCedeira) + xmin(demCedeira))/2,
                   (ymax(demCedeira) + ymin(demCedeira))/2),
                 nrow = 1)

tt <- seq(as.POSIXct("2024-06-01 07:00:00", tz = "Europe/Madrid"),
          as.POSIXct("2024-06-01 21:00:00", tz = "Europe/Madrid"),
          by = "15 min")
sun <- lapply(tt, function(x) solarpos(lonlat, x))

ncores <- detectCores()

hillshades <- mclapply(sun, function(ang)
{
    DEM %>%
        sphere_shade(texture = "imhof1", sunangle = ang[1]) %>%
        add_water(water, color = "imhof1") %>%
        add_shadow(ray_shade(DEM,
                             sunangle = ang[1],
                             sunaltitude = ang[2]),
                   0.75)
}, mc.cores = ncores)

old <- setwd(tempdir())

idx <- seq_along(hillshades)

for(i in idx)
{
    plot_3d(heightmap = DEM,
            hillshade = hillshades[[i]],
            zscale = 5,
            fov = 45,
            theta = 0,
            zoom = 0.75,
            phi = 45,
            windowsize = c(1000, 800))
    render_snapshot(filename = paste0(i, ".png"),
                    title_text = tt[i])
}

gifski(png_files = paste0(idx, ".png"),
       gif_file = "cedeira.gif",
       delay = 1/5)
  
setwd(old)
