library("lattice")
library("ggplot2")
library("latticeExtra")
library("rnaturalearth")
library("sf")
library("sp")

sf_use_s2(FALSE) ## Needed for st_crop to work

## https://www.movebank.org/cms/webapp?gwt_fragment=page=studies,path=study2970193504+individual3034756875+deployment3034758018
bird <- read.csv("~/MX9216-13000006df.csv")
N <- nrow(bird)
bird$timestamp <- as.POSIXct(bird$timestamp)
bird$date <- as.Date(bird$timestamp)

boundaries <- ne_countries(scale = "large")
boundaries <- st_crop(boundaries,
                      xmin = min(bird$location.long),
                      xmax = max(bird$location.long),
                      ymin = min(bird$location.lat),
                      ymax = max(bird$location.lat))

boundariesSP <- as(boundaries, "Spatial")

xyplot(location.lat ~ location.long, data = bird,
       aspect = "iso",
       type = "l") +
    layer(sp.lines(boundariesSP))

p <- lapply(10000:12000, function(i) {
    xyplot(location.lat ~ location.long, data = bird[i, ],
           aspect = "iso",
           xlim = range(bird$location.long),
           ylim = range(bird$location.lat)) + 
        layer(sp.lines(boundariesSP))
})

delta <- 0.2

dateLabs <- function(x)format(as.Date(x, origin = "1970-01-01"), "%Y/%m")

ggplot() +
    ## geom_segment(aes(x = location.long, y = location.lat,
    ##                  xend = location.long + delta * sin(heading * pi/180), 
    ##                  yend = location.lat + delta * cos(heading * pi/180),
    ##                  color = date),
    ##              #arrow = arrow(length = unit(0.01,"cm")),
    ##              #alpha = 0.5,
    ##              data = bird) +
    geom_point(aes(x = location.long, y = location.lat, color = date),
               data = bird) + 
    scale_colour_viridis_c(labels = dateLabs) + 
    geom_sf(data = boundaries, fill = "transparent") +
    geom_point(data = bird[1,], aes(x = location.long, y = location.lat),
               size = 5, color = "red") +
    geom_point(data = bird[N,], aes(x = location.long, y = location.lat),
               size = 3, color = "black") +
    theme_bw()

library(gganimate)

p <- ggplot() +
    geom_point(aes(x = location.long, y = location.lat),
               data = bird) + 
    geom_sf(data = boundaries, fill = "transparent") +
    theme_bw() +
    xlab("Longitude") + ylab("Latitude") +
    transition_time(timestamp) +
    ggtitle("{format(frame_time, format = '%Y-%m-%d %H:%M:%S')}") +
    shadow_mark(0.5, color = "gray", size = 0.75)

animate(p, renderer = ffmpeg_renderer(), fps = 10, duration = 60)

