library(readxl)
Temperature <- read_excel("Temperature.xls")
View(Temperature) 
names(Temperature)
str(Temperature)

AllStations <- unique(Temperature$Station)
N <- length(AllStations)
for (i in 1:30) {
  Station.i <- as.character(AllStations[i])
  print(Station.i)
  TPi <- Temperature[Temperature$Station == Station.i ,]
  Rplot6.1 <- paste(Station.i, ".jpg", sep = "") 
  plot(x = TPi$dDay2 , y = TPi$Temperature, 
       xlab = "Days since 1 January 1990",
       ylab = "Temperature",
       main = Station.i)}

