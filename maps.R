library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)
library(grid)
library(RColorBrewer)
library(moinput)

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

# ggplot2 with eu11 map

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
        axis.ticks.y=element_blank(),
        legend.text=element_text(size=28),
        legend.title=element_text(size=30),
        legend.key.size=unit(2, "line"),
        legend.position = c(0.12,0.4),
        legend.background = element_rect(fill="transparent")) +
  scale_fill_manual(values = colors,name= "11 European\nregions")
gg

dev.off()

# ggplot2 with hydropotential visualizations

mapH <- data.frame(matrix(ncol = 5, nrow = 0))
names(mapH) <- c("long", "lat", "region", "WGBU", "Gernaat")
mapH <- map3[order(map3[,6]),][,c(1,2,6)]
mapAgg <- aggregate(mapH[,c(1,2)], list(mapH$id), mean)

eu11 <- read.csv2("EU11.csv", stringsAsFactors = FALSE)
eu11 <- eu11[-which(eu11[,1] == "KOS"),]
generic <- readSource("Gernaat", "Tech.Full")
generic <- generic[eu11[,1],,]
genericAgg <- toolAggregate(generic, eu11)

wgbu <- readSource("WGBU", convert = TRUE)
wgbu <- wgbu[eu11[,1],,2]
wgbuAgg <- toolAggregate(wgbu, eu11)

# convert to df

wgbuDf <- as.data.frame(wgbuAgg)
wgbuDf <- wgbuDf[order(wgbuDf[,2]),]

genericDf <- as.data.frame(genericAgg)
genericDf <- genericDf[order(genericDf[,2]),]

mapAgg$WGBU <- wgbuDf$Value
mapAgg$Gernaat <- genericDf$Value

mapAgg[12,2] <- -208456.2
mapAgg[12,3] <- 6810689
mapAgg[11,2] <- 892606.0 
mapAgg[11,3] <- 8342088
mapAgg[10,2] <- 3855244.9
mapAgg[10,3] <- 4586744
mapAgg[8,3] <- 2605244.9
mapAgg[8,3] <- 5586744 
mapAgg[7,2] <- 2103849.2
mapAgg[7,3] <- 6646070
mapAgg[6,2] <- 1608276.0
mapAgg[6,3] <- 8342088
mapAgg[5,2] <- 1302320.4
mapAgg[4,2] <- -400060.1
mapAgg[3,2] <- 511829.5
mapAgg[2,2] <- 1125500.3
mapAgg[2,3] <- 6646070
mapAgg[1,2] <- 893086.0
mapAgg[1,3] <- 5827015

png("./vis/EU11HydroPotential.png", height = 1000, width = 1500)
cols <- c("WGBU"="red","Gernaat et al. 2017"="blue")

gg <- ggplot() + geom_polygon(data=map3, aes(long, lat, group = group),
                              color = "black", fill = "khaki", size = 0.1) +
  geom_polygon(data=map4, aes(long, lat, group = group),
               color = "black", fill = "white", size = 0.1) +
  geom_errorbar(data = mapAgg, size = 6, alpha = 0.9, width = 0, aes(x = long, ymin = lat,
  ymax = lat+(2500*WGBU)+100000, colour = "WGBU")) +
  geom_errorbar(data = mapAgg, size = 6, alpha = 0.9, width = 0, aes(x = long+120000/1.2, ymin = lat, 
  ymax = lat+(2500*Gernaat)+100000, colour = "Gernaat et al. 2017")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = c(0.13,0.5),
        legend.background = element_rect(fill="transparent"),
        legend.text=element_text(size=21),
        legend.title=element_text(size=24),
        legend.key.size=unit(2, "line")) +
  scale_colour_manual(name="Legend", breaks = c("WGBU", "Gernaat et al. 2017"), values=cols)
gg

dev.off()

### issues ###

# can use generic for comparing technical and eocnomic potential differences, but not theoretical potentials
# for theoretical potentials, one option could be to use the plos one shapefile data, another option would be to wait for the data provided by nature authors
# aggregate to remind regions
# aggregate to europe eu11, important
# bars on countries for easier difference visualization
# percentage of difference below