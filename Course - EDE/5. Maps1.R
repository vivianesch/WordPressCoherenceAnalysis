
library(maps)
library(mapdata)
library(maptools)
library(RgoogleMaps)


Amaz <- read_delim("Focos.2019-08-31.2019-09-01.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)

head(Amaz)
tidy(map)

map("worldHires", "Brazil", col = "green", fill = TRUE)


devtools::install_github('abjur/abjData')

build_maps <- constroi_mapa_tematico <- function(dataset){
  dataset %>% 
    inner_join(abjData::br_uf_map) %>% {
      ggplot(.) +
        geom_map(aes(x = long, y = lat,
                     map_id = id, fill = variavel),
                 color = 'gray30', map = ., data = .) + 
        theme_void() +
        coord_equal()
    }
}

library("ggplot2")  
library("ggrepel")  
library("maptools")  
library("broom")  

map <- rgdal::readOGR("mapas/BRUFE250GC_SIR.shp")  
summary(map)
names(map)
head(map)
ggplot(map@data)

library(leaflet)







