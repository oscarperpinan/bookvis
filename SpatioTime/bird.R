library("sf")
library("move2")
library("ggplot2")
library("gganimate")
library("rnaturalearth")

sf_use_s2(FALSE) ## Needed for st_crop to work

birds <- movebank_download_study(2970193504,
                                 individual_local_identifier = c("MX8137", "MX9216"),
                                 "license-md5"="0dff0727e212af015c896ba138fe139f")

## https://www.movebank.org/cms/webapp?gwt_fragment=page=studies,path=study2970193504+individual3034756875+deployment3034758018

boundaries <- ne_countries(scale = "large")

boundaries <- st_crop(boundaries, birds)

ggplot() +
    geom_sf(data = boundaries) +
    geom_sf(data = birds,
            aes(color = individual_local_identifier),
            alpha = 0.05)


p <- ggplot() +
    geom_sf(data = boundaries) +
    geom_sf(data = birds,
            aes(colour = individual_local_identifier),
            size = 3) +
    theme_bw() +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("{format(frame_time, format = '%Y-%m-%d %H:%M:%S')}") +
    transition_time(timestamp) +
    shadow_wake(.05)


animate(p, renderer = ffmpeg_renderer(), fps = 10, duration = 60)
