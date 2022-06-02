setwd("C:/Ngôn ngữ lập trình R/RBook")
CP <- read.table(file="CodParasite.txt", header = TRUE)
names(CP)
boxplot(Intensity ~ Area, data = CP)
boxplot(Intensity ~ Area * Sex, data = CP)



