### visualization scripts ###

library(moinput)

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