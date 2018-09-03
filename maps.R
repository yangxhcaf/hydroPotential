library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)
library(grid)
library(RColorBrewer)

# download file if necessary

download.file("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_countries.zip", "ne_10m_admin_0_countries.zip")
unzip("ne_10m_admin_0_countries.zip", exdir = "mapData")
file.remove("ne_10m_admin_0_countries.zip")

# define variables

map <- shapefile("./mapData/ne_10m_admin_0_countries.shp")
iso3 <- read.csv2("EU11.csv", stringsAsFactors = FALSE)
map$SOV_A3[which(map$ADMIN == "Netherlands")] <- "NLD"
map$SOV_A3[which(map$ADMIN == "France")] <- "FRA"
map$SOV_A3[which(map$ADMIN == "Denmark")] <- "DNK"
map$SOV_A3[which(map$ADMIN == "Finland")] <- "FIN"
map$SOV_A3[which(map$ADMIN == "Faroe Islands")] <- "FRO"
map$SOV_A3[which(map$ADMIN == "United Kingdom")] <- "GBR"
map$SOV_A3[which(map$ADMIN == "Guernsey")] <- "GGY"
map$SOV_A3[which(map$ADMIN == "Gibraltar")] <- "GIB"
map$SOV_A3[which(map$ADMIN == "Isle of Man")] <- "IMN"
map$SOV_A3[which(map$ADMIN == "Jersey")] <- "JEY"
map$SOV_A3[which(map$ADMIN == "Greenland")] <- "GRL"


colors <- c("EUB" =brewer.pal(9,"YlOrRd")[6],"FRA"=brewer.pal(9,"YlOrRd")[7],                 
            "EUI"=brewer.pal(9,"YlOrRd")[1],"EUL"=brewer.pal(9,"YlOrRd")[3],"EUS"=brewer.pal(9,"YlOrRd")[2],  
            "EUN"=brewer.pal(9,"BuPu")[5],                                  
            "DEU"=brewer.pal(9,"Greys")[6],"EUP"=brewer.pal(9,"Greys")[5],                 
            "UKI"=brewer.pal(9,"Blues")[6],                                  
            "NOR"=brewer.pal(9,"YlGn")[6],"CHE"=brewer.pal(9,"YlGn")[5] ,"NEU"=brewer.pal(9,"YlGn")[3] #,
            #           "other"=NA
)

# perform operations

iso3[,3] <- sapply(1:nrow(iso3), function(i) return(colors[which(names(colors) == iso3[i,2])]))
map2 <- crop(map, extent(-35,60,30,72))
map3 <- map2[map2$SOV_A3 %in% iso3[,1],]
map4 <- map2[!map2$SOV_A3 %in% iso3[,1],]

map3 <- fortify(spTransform(map3, CRS("+init=epsg:3857")), region = "SOV_A3")
map3[,6] <- do.call("c", lapply(1:length(map3[,6]), function(i) return(unique(iso3[which(iso3[,1] == map3[i,6]),2]))))
map4 <- fortify(spTransform(map4, CRS("+init=epsg:3857")), region = "SOV_A3")

# ggplot2 method with more functionalities

png("./vis/EU11.png", height = 1000, width = 1500)

gg <- ggplot() + geom_polygon(data=map3, aes(long, lat, group = group, fill = id),
                       color = "black", size = 0.1) +
geom_polygon(data=map4, aes(long, lat, group = group),
                        color = "black", fill = "white", size = 0.1) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_manual(values = colors,name= "11 European regions")
gg

dev.off()

### extra code ###

# # basic plot method
# 
# png("./vis/EU11.png", height = 1000, width = 1000)
# plot(spTransform(map3, CRS("+init=epsg:3857")), col = "khaki", bg = "azure2", lwd = 0.5)
# dev.off()