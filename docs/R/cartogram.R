##################################################################
## Initial configuration
##################################################################
## Clone or download the repository and set the working directory
## with setwd to the folder where the repository is located.

##################################################################

library("ggplot2")
library("sf")
library("cartogram")

sfPopGDPSpain <- st_read("data/Spatial/sfPopGDPSpain.shp")

ggplot(sfPopGDPSpain) +
  geom_point(aes(y = Province, x = Population/1e6)) +
  xlab("Population (million)") + 
  theme_bw()

ggplot(sfPopGDPSpain) +
  geom_point(aes(y = Province, x = GDP/1e6)) +
  xlab("GDP (million euros)") + 
  theme_bw()

ggplot(sfPopGDPSpain, aes(y = Population/1e6, x = GDP/1e6)) +
  geom_point() +
  geom_smooth() + 
  xlab("GDP (million euros)") + ylab("Population (million)") +
  theme_bw()

cartPopNCont <- cartogram_ncont(sfPopGDPSpain, weight = "Population")

ggplot(cartPopNCont) +
  geom_sf(aes(fill = Population)) + 
  scale_fill_distiller(palette = "Blues", direction = 1) + 
  theme_bw()

cartGDPNCont <- cartogram_ncont(sfPopGDPSpain, weight = "GDP")

ggplot(cartGDPNCont) +
  geom_sf(aes(fill = GDP)) + 
  scale_fill_distiller(palette = "Blues", direction = 1) + 
  theme_bw()

cartPopCont <- cartogram_cont(sfPopGDPSpain,
                              weight = "Population")

ggplot(cartPopCont) +
  geom_sf(aes(fill = Population)) + 
  scale_fill_distiller(palette = "Blues", direction = 1) + 
  theme_bw()

cartGDPCont <- cartogram_cont(sfPopGDPSpain,
                              weight = "GDP")

ggplot(cartPopCont) +
  geom_sf(aes(fill = GDP)) + 
  scale_fill_distiller(palette = "Blues", direction = 1) + 
  theme_bw()

cartPopDorl <- cartogram_dorling(sfPopGDPSpain, weight = "Population")

ggplot(cartPopDorl) +
  geom_sf(aes(fill = Population)) + 
  scale_fill_distiller(palette = "Blues", direction = 1) + 
  theme_bw()

cartGDPDorl <- cartogram_dorling(sfPopGDPSpain, weight = "GDP")

ggplot(cartGDPDorl) +
  geom_sf(aes(fill = GDP)) + 
  scale_fill_distiller(palette = "Blues", direction = 1) + 
  theme_bw()
