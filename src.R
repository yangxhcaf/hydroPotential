### visualization scripts ###

# load dependencies

library(moinput)

# manipulate magpie objects with aggregationss
# "KOS" removed from eu11 as no counterpart in weighted predictions

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
 
# plot

png("./vis/comparisonChart.png", width = 1200, height = 750)
op <- par(cex = 1, mgp= c(3,1,0), oma=c(1,1,1,1))
plot(1:12, wgbuDf$Value, "o", ylim = c(0,300), col = "red", lwd = 2, axes = F, xlab = "", ylab = "")
box()
par(new = TRUE)
plot(1:12, genericDf$Value, "o", ylim = c(0,300), col = "blue", lwd = 2, axes = F, xlab = "EU11", ylab = "Technical Potential (TWh/a @ <0.5 $/Kwh)")
box()
axis(side = 1, at = c(1:12), labels = genericDf$Region)
axis(side = 2, at = seq(0,500,50))
legend(0.9, 290, legend=c("WGBU", "Gernaat et al. 2017"),
       col=c("red", "blue"), lty=1:1, lwd = 2, cex=1.1)
dev.off()

### issues ###

# can use generic for comparing technical and eocnomic potential differences, but not theoretical potentials
# for theoretical potentials, one option could be to use the plos one shapefile data
# another option would be to wait for the data provided by nature authors
# aggregate to remind regions
# aggregate to europe eu11, important
# bars on countries for easier difference visualization
# percentage of difference below