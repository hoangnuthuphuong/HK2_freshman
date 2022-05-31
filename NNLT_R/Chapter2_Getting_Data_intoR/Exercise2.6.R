# Vi tri thu muc lam viec
setwd("C:/Ngôn ngữ lập trình R/RBook")
# Xuat du lieu sang tep ASCII
Data1 = read.table(file="ISIT.txt", header = TRUE)
Data1
# Xuat thử 50 giá trị của tập dữ liệu
options(max.print = 50)
Data2 = scan("ISIT.txt", what = "character")
Data2
# Kiem tra xem đối số của nó có phải là ma trận hay dataframe hay không
# Data1
is.matrix(Data1) #FALSE
is.data.frame(Data1) #TRUE
# Data2
is.matrix(Data2) #FALSE
is.data.frame(Data2) #FALSE

