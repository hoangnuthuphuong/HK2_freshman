setwd("C:/Ngôn ngữ lập trình R/RBook")
Owls <- read.table(file = "Owls.txt", header = TRUE, dec = ".")
names(Owls)
str(Owls)

# Thực hiện hai chấm của Cleveland về lượng thương mại và thời gian đến.

dotchart (Owls$NegPerChick,
          main = "Thương lượng",
          xlab = "Giá trị của biến",
          ylab = "Thứ tự của dữ liệu")
dotchart (Owls $ ArrivalTime,
          main = "Thời gian đến",
          xlab = "Giá trị của biến",
          ylab = "Thứ tự của dữ liệu")

