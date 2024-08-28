library(rayshader)
library(suntools)
library(raster)

tt <- seq(as.POSIXct("2024-06-01 07:00:00", tz = "Europe/Madrid"),
          as.POSIXct("2024-06-01 21:00:00", tz = "Europe/Madrid"),
          by = "15 min")

demCedeira <- raster('data/Spatial/demCedeira')
DEM <- raster_to_matrix(demCedeira)

lonlat <- matrix(c((xmax(demCedeira) + xmin(demCedeira))/2,
                   (ymax(demCedeira) + ymin(demCedeira))/2),
                 nrow = 1)

sun <- lapply(tt, function(x) solarpos(lonlat, x))

water <- detect_water(DEM)

hillshades <- lapply(sun, function(ang)
{
    DEM %>%
        sphere_shade(texture = "imhof1", sunangle = ang[1]) %>%
        add_water(water, color = "imhof1") %>%
        add_shadow(ray_shade(DEM,
                             sunangle = ang[1],
                             sunaltitude = ang[2]),
                   0.75)
})

old <- setwd("/tmp/rayshader")

idx <- seq_along(hillshades)

lapply(idx, function(i)
{
    plot_3d(heightmap = DEM,
            hillshade = hillshades[[i]],
            zscale = 5,
            fov = 45,
            theta = 0,
            zoom = 0.75,
            phi = 45,
            windowsize = c(1000, 800))
    render_snapshot(filename = i)
})

setwd(old)
