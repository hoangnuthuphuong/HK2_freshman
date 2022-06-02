setwd("C:/Ngôn ngữ lập trình R/RBook")
CP <- read.table(file="CodParasite.txt", header = TRUE)
names(CP)

dotchart(CP$Intensity, groups = factor(CP$Area))
dotchart(CP$Depth, groups = factor(CP$Prevalence))
