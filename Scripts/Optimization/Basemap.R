# remotes::install_github("dkahle/ggmap")
library(ggmap)

register_stadiamaps(key = "e084c12a-a8c8-4e55-bbc5-a4d18c4d8099",write = TRUE)


# good basemaps
# stamen_watercolor
# stamen_terrain_background
# stamen_toner_background

atlCan <- get_stadiamap(bbox = c(left   = -140, 
                                 bottom = -60,
                                 right  = 160,
                                 top    = 60), 
                        zoom = 2, maptype = "stamen_terrain_background")
ggmap(atlCan)