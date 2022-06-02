setwd("C:/Ngôn ngữ lập trình R/RBook")
Owls <- read.table(file = "Owls.txt", header = TRUE, dec = ".")
names(Owls)
str(Owls)

ifelse(Owls$FoodTreatment == "Satiaded", Owls$NestNight <- paste(Owls$Nest, "1",sep = "_"),
       Owls$NestNight <- paste(Owls$Nest, "2",sep = "_"))
head(Owls)                             
#You can also do this with: Owls$NestNight <- paste(Owls$Nest, Owls$FoodTreatment,sep = "_") 

AllNestsNights <- unique(Owls$NestNight)
N <- length(AllNestsNights)
for (i in 1:N) {
  NestNight.i <- as.character(AllNestsNights[i])
  print(NestNight.i)
  Owlsi <- Owls[Owls$NestNight == NestNight.i ,]
  Rplot6.2 <- paste(NestNight.i, ".jpg", sep = "") 
  plot(x = Owlsi$ArrivalTime , y = Owlsi$NegPerChick, 
       xlab = "Arrival Time",
       ylab = "Sibling negotiation",
       main = NestNight.i)
  dev.off()
}

