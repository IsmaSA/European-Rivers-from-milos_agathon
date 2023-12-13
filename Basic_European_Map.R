
library(maps)
library(rworldmap)
library(rworldxtra)

newmap <- getMap(resolution="high")
par(mar=c(0.5,0.5,0.5,0.5))
plot(newmap, col="tan1", border="black",bg="lightblue", 
     xlim=c(23,48), ylim=c(35,43))

setwd("C:/Users/isma-/OneDrive/Escritorio/Crustaceans0/crustaceans/qgis")
riversData <- rgdal::readOGR("tur_watcrsl_rvr_1m.shp")
plot(riversData, col= "steelblue2", add=T)

points(df1$Longitude1 , df1$Latitude1, 
       col="black", pch=23, cex=0.6, bg="red2")

scalebar(x = 24.5, y = 36.2, which = "both", unit = "km", cex = 0.8, 
         length = 20, col = "black", bg = "white", border = "black")
maps::map.scale(x = 24.5, y = 36.2, ratio = F, relwidth=0.25)



###########################################################

library(ggplot2)
library(dplyr)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)

europe_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(continent == "Europe" | name == "Turkey" | 
           name %in% c("Morocco", "Algeria", "Tunisia", "Libya", "Egypt"))

europe_map$color <- ifelse(europe_map$name %in% c("Morocco", "Algeria", "Tunisia", "Libya", "Egypt", "Russia", "Turkey"),
                           "grey80", "Wheat")

setwd("C:/Users/isotoalmena/Downloads")
riversData <- rgdal::readOGR("rivers_europe_37253.shp")
riversData$line_width <- scales::rescale(riversData$Strahler, to = c(0.1, 1))
riversData_sf <- st_as_sf(riversData)
riversData1 <-riversData_sf %>% filter(line_width > 0.25)
  
ggplot() +
  geom_sf(data = europe_map, aes(fill = color))+   scale_fill_identity()+ 
  geom_sf(data = riversData1, aes(size = line_width), color = "steelblue2") + 
  coord_sf() + theme_bw()+ xlim(-11,30) + ylim(36,60)+
geom_point(data = df1, aes(x = Longitude, y = Latitude, color = year), size = 2) +
  scale_color_gradient(low = "yellow", high = "red", name = "Year") + 
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", 
                         which_north = "true", pad_x = unit(0.75, "in"),
                         pad_y = unit(0.5, "in"))
