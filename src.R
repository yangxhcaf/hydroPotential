library(moinput)

generic <- readSource("Gernaat", "Tech.Full")
wgbu <- readSource("WGBU", convert = TRUE)

genericAgg <- toolAggregate(generic, paste0(getConfig()$mappingfolder, "/regional/regionmapping_22_EU11.csv"))
wgbuAgg <- toolAggregate(wgbu, paste0(getConfig()$mappingfolder, "/regional/regionmapping_22_EU11.csv"))
wgbuAgg <- wgbuAgg[,,2]

### extra code ###

# make comparisons between both datasets, try map and convulation to show differences/similarities
# need some world maps and boundaries to be able to conduct this
# or possibly charts from east to west and show curves for different distributions
# try different methods to show different aspects of data

# can use generic for comparing technical and eocnomic potential differences, but not theoretical potentials
# for theoretical potentials, one option could be to use the plos one shapefile data
# another option would be to wait for the data provided by nature authors
# check for existing plot functions available through local R packages, then experiment new methods

# wgbuDf <- as.data.frame(wgbuAgg[,,2])
# wgbuDf <- wgbuDf[order(wgbuDf[,2]),]
# 
# genericDf <- as.data.frame(generic[,,"Tech.Full"])
# genericDf <- genericDf[order(genericDf[,2]),]
# 
# op <- par(mfrow=c(2,1), cex = 0.8, mgp= c(5,3,0))
# plot(1:27, wgbuDf$Value, "o", ylim = c(0,2500), xlab = "WGBU", col = "red", lwd = 2, axes = F)
# box()
# axis(side = 1, at = c(1:27), labels = gsub(" ", "\n", genericDf$Region))
# axis(side = 2, at = seq(0,2500,500))
# plot(1:27, genericDf$Value, "o", ylim = c(0,2500), xlab = "Gernaat", col = "blue", lwd = 2, axes = F)
# box()
# axis(side = 1, at = c(1:27), labels = gsub(" ", "\n", genericDf$Region))
# axis(side = 2, at = seq(0,2500,500))
 
# aggregate to remind regions
# aggregate to europe eu11, important
# bars on countries for easier difference visualization
# percentage of difference below