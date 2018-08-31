library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)
library(grid)
library(RColorBrewer)

# define variables

read.csv2("EU11.csv", stringsAsFactors = FALSE)[,1]
countries <- c("Belgium","Luxembourg","Netherlands","France","Spain","Portugal","Austria","Italy","Malta","Bulgaria","Cyprus","Greece","Croatia","Hungary","Romania","Slovenia","Denmark","Finland","Faeroe Is.","Sweden","Germany","Czechia","Estonia","Lithuania","Latvia","Poland","Slovakia","United Kingdom","Guernsey","Gibraltar","Isle of Man","Ireland","Jersey","Iceland","Norway","Switzerland")
otherCountries <- c("Albania","Andorra","Bosnia and Herz.","Greenland","Liechtenstein","Vatican","Monaco","Montenegro","San Marino","Serbia","Kosovo","Turkey","Vatican")
countries <- c(countries, otherCountries)

download.file("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_countries.zip", "ne_10m_admin_0_countries.zip")
unzip("ne_10m_admin_0_countries.zip", exdir = "mapData")
file.remove("ne_10m_admin_0_countries.zip")

map <- shapefile("./mapData/ne_10m_admin_0_countries.shp")
map2 <- crop(map, extent(-35,60,30,72))
map3 <- map2[map2$NAME %in% countries,]
map4 <- map2[!map2$NAME %in% countries,]

# ggplot2 method with more functionalities
png("./vis/EU11.png", height = 1000, width = 1500)
gg <- ggplot() + geom_polygon(data=fortify(spTransform(map3, CRS("+init=epsg:3857"))), aes(long, lat, group = group),
                       colour = alpha("darkred", 1/2), size = 0.3, fill = 'khaki', alpha = 1) +
geom_polygon(data=fortify(spTransform(map4, CRS("+init=epsg:3857"))), aes(long, lat, group = group),
                        colour = alpha("darkred", 1/2), size = 0.3, fill = 'white', alpha = 1) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

gg
dev.off()
### extra code ###

# # basic plot method
# 
# png("./vis/EU11.png", height = 1000, width = 1000)
# plot(spTransform(map3, CRS("+init=epsg:3857")), col = "khaki", bg = "azure2", lwd = 0.5)
# dev.off()