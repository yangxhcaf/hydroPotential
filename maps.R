library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)
library(grid)
library(RColorBrewer)

countries <- c("Belgium","Luxembourg","Netherlands","France","Spain","Portugal","Austria","Italy","Malta","Bulgaria","Cyprus","Greece","Croatia","Hungary","Romania","Slovenia","Denmark","Finland","Faeroe Is.","Sweden","Germany","Czechia","Estonia","Lithuania","Latvia","Poland","Slovakia","United Kingdom","Guernsey","Gibraltar","Isle of Man","Ireland","Jersey","Iceland","Norway","Switzerland")
otherCountries <- c("Albania","Andorra","Bosnia and Herz.","Greenland","Liechtenstein","Vatican","Monaco","Montenegro","San Marino","Serbia","Kosovo","Turkey","Vatican")
countries <- c(countries, otherCountries)

read.csv2("EU11.csv", stringsAsFactors = FALSE)[,1]
map <- shapefile("../../../Downloads/ne_10m_admin_0_countries.shp")
map2 <- crop(map, extent(-35,60,30,72))
map3 <- map2[map2$NAME %in% countries,]

# basic plot method

png("./vis/EU11.png", height = 1000, width = 1000)
plot(spTransform(map3, CRS("+init=epsg:3857")), col = "khaki", bg = "azure2", lwd = 0.5)
dev.off()

# ggplot2 method with more functionalities

map4 <- spTransform(map3, CRS("+init=epsg:3857"))
map4f <- fortify(map4)
ggplot()+ geom_polygon(data=map4f, aes(long, lat, group = group), 
                       colour = alpha("darkred", 1/2), size = 0.7, fill = 'skyblue', alpha = .3)