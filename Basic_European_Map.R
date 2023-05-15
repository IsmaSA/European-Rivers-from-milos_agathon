
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
