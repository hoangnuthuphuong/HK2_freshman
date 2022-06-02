setwd("C:/Ngôn ngữ lập trình R/RBook")
Veg <- read.table(file="Vegetation2.txt", header = TRUE)

Me <- tapply(Veg$R, Veg$Transect, mean)
Sd <- tapply(Veg$R, Veg$Transect, sd)
Le <- tapply(Veg$R, Veg$Transect, length)
Se <- Sd / sqrt(Le)

cbind(Me, Sd, Le)

bp <- barplot(Me, xlab = "Transect",
              ylab = "Richness", ylim = c(0,20))

arrows(bp, Me, bp, Me + Sd, lwd = 1.5, angle=90, length=0.1)
box()


plot(x=Veg$Transect, y = jitter(Veg$R),
     pch=1,  xlab = "Transect",
     ylab = "Richness")

points(1:8,Me, pch = 16, cex = 1.5)
arrows(1:8, Me,
       1:8, Me + Se, lwd = 1.5,
       angle=90, length=0.1)

arrows(1:8, Me,
       1:8, Me - Se, lwd = 1.5,
       angle=90, length=0.1)

