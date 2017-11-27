library(rgl)
library(magick) ## needed to import the texture

lat <- seq(-90, 90, len = 100) * pi/180
long <- seq(-180, 180, len = 100) * pi/180

r <- 6378.1 # radius of Earth in km
x <- outer(long, lat, FUN=function(x, y) r*cos(y)*cos(x))
y <- outer(long, lat, FUN=function(x, y) r*cos(y)*sin(x))
z <- outer(long, lat, FUN=function(x, y) r*sin(y))

open3d()
bg3d('black')

## 'http://eoimages.gsfc.nasa.gov/images/imagerecords/79000/79765/dnb_land_ocean_ice.2012.3600x1800_geo.tif'
earth <- surface3d(-x, -z, y,
                   texture = "nightLightsHR.png", ## max 8192x8192; sólo PNG!
                   specular = "black", col = 'white')

writeWebGL('nightLights', width=1000)

library(XML)

geocode <- function(x){
    city <- x[1]
    country <- x[2]
    urlOSM <- paste0('http://nominatim.openstreetmap.org/search?',
                     'city=', city,
                     '&country=', country,
                     '&format=xml')
  xmlOSM <- xmlParse(urlOSM)
  cityOSM <- getNodeSet(xmlOSM, '//place')[[1]] ## use only the first result
  lon <- xmlGetAttr(cityOSM, 'lon')
  lat <- xmlGetAttr(cityOSM, 'lat')
  as.numeric(c(lon, lat))
  }

cities <- rbind(c('Madrid', 'Spain'),
                c('Tokyo', 'Japan'),
                c('Sidney', 'Australia'),
                c('Sao Paulo', 'Brazil'),
                c('New York', 'USA'))
cities <- as.data.frame(cities)
names(cities) <- c("city", "country")

points <- apply(cities, 1, geocode)
points <- t(points)
colnames(points) <- c("lon", "lat")

cities <- cbind(cities, points)

library(geosphere)

## When arriving or departing include a progressive zoom with 100
## frames
zoomIn <- seq(.3, .1, length = 100)
zoomOut <- seq(.1, .3, length = 100)

## First point of the route
route <- data.frame(lon = cities[1, "lon"],
                    lat = points[1, "lat"],
                    zoom = zoomIn,
                    name = cities[1, "city"],
                    action = 'arrive')

## This loop visits each location included in the 'points' set
## generating the route.
for (i in 1:(nrow(cities) - 1)) {

    p1 <- cities[i,]
    p2 <- cities[i + 1,] 
    ## Initial location
    departure <- data.frame(lon = p1$lon,
                            lat = p1$lat,
                            zoom = zoomOut,
                            name = p1$city,
                            action = 'depart')

    ## Travel between two points: Compute 100 points between the
    ## initial and the final locations.
    routePart <- gcIntermediate(p1[, c("lon", "lat")],
                                p2[, c("lon", "lat")],
                                n = 100)
    routePart <- data.frame(routePart)
    routePart$zoom <- 0.3
    routePart$name <- ''
    routePart$action <- 'travel'

    ## Final location
    arrival <- data.frame(lon = p2$lon,
                          lat = p2$lat,
                          zoom = zoomIn,
                          name = p2$city,
                          action = 'arrive')
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
                          action = 'depart'))

summary(route)

## Function to move the viewpoint in the RGL scene according to the
## information included in the route (position and zoom).
travel <- function(tt){
  point <- route[tt,]
  rgl.viewpoint(theta = -90 + point$lon,
                phi = point$lat,
                zoom = point$zoom)
}

travel(1)
rgl.snapshot('images/travel1.png')

travel(1200)
rgl.snapshot('images/travel2.png')

movie3d(travel,
        duration = nrow(route),
        startTime = 1, fps = 1,
        type = 'mp4', clean = FALSE)
