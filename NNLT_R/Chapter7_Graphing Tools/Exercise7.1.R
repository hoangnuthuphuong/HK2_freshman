View(BirdFluDeaths)
Deaths  <- colSums(BirdFluDeaths[,2:15])
Deaths

#Piechart
par(mfrow = c(2,2), mar = c(3, 3, 2, 1))
pie(Deaths , main = "Ordinary pie chart")
pie(Deaths , col = gray(seq(0.4,1.0,length=6)),
    clockwise=TRUE, main = "Grey colours")
pie(Deaths , col = rainbow(6),clockwise = TRUE, main="Rainbow colours")
par(mfrow = c(1,1), mar = c(3, 3, 2, 1))
pie(rowSums(BirdFluDeaths[,2:15]) , col = gray(seq(0.4,1.0,length=6)),
    clockwise=TRUE, main = "Grey colours")
