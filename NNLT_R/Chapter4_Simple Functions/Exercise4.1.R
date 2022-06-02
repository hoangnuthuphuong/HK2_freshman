library(readxl)
Temperature <- read_excel("Temperature.xls")
View(Temperature)
names(Temperature)
str(Temperature)



# Tính chuỗi giá trị trung bình hàng tháng - kết quả là một biến có kích thước 16x12
TB_tháng <- matrix(nrow = 16, ncol = 12)
unique(Temperature$Year)


TB_tháng <- matrix(nrow = 16, ncol = 12)
AllYears <- unique(Temperature$Year)
for (i in 1:16) {
  ThisYear <- AllYears[i]
  Temperaturei <- Temperature[Temperature$Year == ThisYear ,]
  TB_tháng[i,] <- tapply(Temperaturei$Temperature, FUN=mean, INDEX =Temperaturei$Month , na.rm = TRUE)
}
TB_tháng



Temperature$YeMo <- paste(Temperature$Year,Temperature$Month, sep = ".")
MeanYearMonth <- tapply(Temperature$Temperature, FUN = mean, INDEX = Temperature$YeMo, na.rm = TRUE)
MeanYearMonth
