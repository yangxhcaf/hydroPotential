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

### simple chart ###

# manipulate magpie objects with aggregationss, "KOS" removed from eu11 as no counterpart in weighted predictions

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

# plot basic comparison and precentage deviation

png("./vis/comparisonChart.png", width = 1200, height = 900)

# first plot with raw values

op <- par(mfrow = c(2,1), cex = 1, mgp= c(3,1,0), oma=c(1,1,1,1))
plot(1:12, wgbuDf$Value, "o", ylim = c(0,300), col = "red", lwd = 2, axes = F, xlab = "", ylab = "")
box()
par(new = TRUE)
plot(1:12, genericDf$Value, "o", ylim = c(0,300), col = "blue", lwd = 2, axes = F, xlab = "EU11", ylab = "Technical Potential (TWh/a @ <0.5 $/Kwh)")
box()
axis(side = 1, at = c(1:12), labels = genericDf$Region)
axis(side = 2, at = seq(0,500,50))
legend(0.9, 290, legend=c("WGBU", "Gernaat et al. 2017"),
       col=c("red", "blue"), lty=1:1, lwd = 2, cex=1.1)

# next plot with percentage differences

new <- sapply(1:nrow(wgbuDf), function(i) return(ifelse(min(wgbuDf[i,5], genericDf[i,5]) == 0, 100, (abs(wgbuDf[i,"Value"] - genericDf[i,"Value"])*100)/min(wgbuDf[i,"Value"], genericDf[i,"Value"]))))
plot(1:12, new, "o", ylim = c(0,300), col = "black", lwd = 2, axes = F, xlab = "EU11", ylab = "Absolute Percentage Deviation (%)")
abline(100, 0, col = "red", lty = 2, lwd = 0.8)
box()
axis(side = 1, at = c(1:12), labels = genericDf$Region)
axis(side = 2, at = seq(0,300,50))
legend(0.9, 290, legend="Absolute Percentage\nDeviation\n",
       col="black", lty=1:1, lwd = 2, cex=1.1)

# print
dev.off()

### eu11 map plot ###

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
        legend.title=element_text(size=30, face = "bold"),
        legend.key.size=unit(2, "line"),
        legend.position = c(0.1,0.4),
        legend.background = element_rect(fill="transparent")) +
  scale_fill_manual(values = colors,name= "EU11")
gg

dev.off()

### second map with bar plots ###

# ggplot2 with hydropotential visualizations

mapH <- data.frame(matrix(ncol = 5, nrow = 0))
names(mapH) <- c("long", "lat", "region", "WGBU", "Gernaat")
mapH <- map3[order(map3[,6]),][,c(1,2,6)]
mapAgg <- aggregate(mapH[,c(1,2)], list(mapH$id), mean)

mapAgg$WGBU <- wgbuDf$Value
mapAgg$Gernaat <- genericDf$Value

mapAgg[12,2] <- -208456.2
mapAgg[12,3] <- 6810689
mapAgg[11,2] <- 892606.0 
mapAgg[11,3] <- 8342088
mapAgg[10,2] <- 3855244.9
mapAgg[10,3] <- 4606744
mapAgg[8,3] <- 2605244.9
mapAgg[8,3] <- 5656744 
mapAgg[7,2] <- 2103849.2
mapAgg[7,3] <- 6686070
mapAgg[6,2] <- 1608276.0
mapAgg[6,3] <- 8342088
mapAgg[5,2] <- 1302320.4
mapAgg[4,2] <- -400060.1
mapAgg[3,2] <- 511829.5
mapAgg[2,2] <- 1125500.3
mapAgg[2,3] <- 6646070
mapAgg[1,2] <- 893086.0
mapAgg[1,3] <- 5827015
mapAgg[,3] <- mapAgg[,3] + 50000

png("./vis/EU11HydroPotential.png", height = 1000, width = 1500)
cols <- c("WGBU"="red","Gernaat et al. 2017"="blue")

gg <- ggplot() + geom_polygon(data=map3, aes(long, lat, group = group),
                              color = "black", fill = "khaki", size = 0.1) +
  geom_polygon(data=map4, aes(long, lat, group = group),
               color = "black", fill = "white", size = 0.1) +
  geom_errorbar(data = mapAgg, size = 6, alpha = 0.9, width = 0, aes(x = long, ymin = lat,
  ymax = ifelse(WGBU!=0, lat+(5000*WGBU), lat+0.5*5000*Gernaat), colour = "WGBU")) +
  geom_errorbar(data = mapAgg, size = 6, alpha = 0.9, width = 0, aes(x = long+120000/1.2, ymin = lat, 
  ymax = lat+(5000*Gernaat), colour = "Gernaat et al. 2017")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = c(0.135,0.5),
        legend.background = element_rect(fill="transparent"),
        legend.text=element_text(size=21),
        legend.title=element_text(size=24, face="bold"),
        legend.key.size=unit(2, "line")) +
  scale_colour_manual(name="Legend", breaks = c("WGBU", "Gernaat et al. 2017"), values=cols) +
  geom_text(data = mapAgg, aes(x=long+55000, y=lat-120000, label = Group.1), size = 6)

gg

dev.off()

### issues ###

# can use generic for comparing technical and eocnomic potential differences, but not theoretical potentials
# for theoretical potentials, one option could be to use the plos one shapefile data, another option would be to wait for the data provided by nature authors
# aggregate to remind regions
# aggregate to europe eu11, important
# bars on countries for easier difference visualization
# percentage of difference below