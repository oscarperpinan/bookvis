library("sf")
library("move2")
library("ggplot2")
library("gganimate")
library("rnaturalearth")
library("units")

sf_use_s2(FALSE) ## Needed for st_crop to work

birds0 <- movebank_download_study(2398637362,
                                 "license-md5"="74263192947ce529c335a0ae72d7ead7")

boundaries <- ne_countries(scale = "large")

boundaries <- st_crop(boundaries, birds0)

birds <- subset(birds0, ground_speed > set_units(2L, "m/s"))
birds$speed <- cut(birds$ground_speed, breaks = c(2, 5, 10, 15, 35))
birds$month <- format(mt_time(birds), "%m")

ggplot() +
    geom_sf(data = boundaries) +
    geom_sf(data = birds,
            aes(color = individual_local_identifier),
            alpha = 0.1) +
    guides(colour = guide_legend(override.aes = list(alpha = 1)))
    theme_linedraw()

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
    facet_wrap(~ month) +
    scale_fill_ordinal("Speed") +
    theme_linedraw()

p <- ggplot() +
    geom_sf(data = boundaries) +
    geom_sf(data = birds,
            aes(colour = individual_local_identifier),
            size = 3) +
    theme_bw() +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("{format(frame_time, format = '%Y-%m-%d %H:%M:%S')}") +
    transition_time(timestamp) +
    shadow_wake(.5)


animate(p, fps = 10, duration = 60)

## animate(p, fps = 10, duration = 60,
##         renderer = ffmpeg_renderer())
