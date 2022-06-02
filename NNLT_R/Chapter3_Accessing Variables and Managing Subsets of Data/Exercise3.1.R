# xuất thư viện cần sử dụng
# Đọc file BirdFlu.xls vào R - ta cần loại bỏ 9 hàng đầu tiên vì dữ liệu không dùng đến
library(readxl)
BirdFlu <- read_excel("RBook/BirdFlu.xls", skip = 9)
View(BirdFlu)
# Hiển thị tên và cấu trúc dữ liệu bên trong khung đữ liệu
names(BirdFlu)
str(BirdFlu)

# In ra số ca bệnh trong năm 2003
BirdFlu[,"cases2003"]
BirdFlu$cases2003
BirdFlu[,1]

# Tổng số ca bệnh trong năm 2003 và 2005
sum(BirdFlu[,"cases2003"])
sum(BirdFlu[,"cases2005"])


# Quốc gia có nhiều trường hợp mắc bệnh nhất
# Tạo cột Totalcases để tính tổng số ca mắc bệnh của từng quốc gia 
BirdFlu$TotalCases <- BirdFlu$cases2003 + BirdFlu$cases2004 + BirdFlu$cases2005 + BirdFlu$cases2006 + BirdFlu$cases2007 + BirdFlu$cases2008
BirdFlu$TotalCases   # => Indonesia là quốc gia có nhiều ca bệnh nhất = 135

# Quốc gia có nhiều trường hợp tử vong nhất
# Tạo cột TotalDeaths để tính tổng số ca tử vong của từng quốc gia
BirdFlu$TotalDeaths <- BirdFlu$deaths2003 + BirdFlu$deaths2004 + BirdFlu$deaths2005 + BirdFlu$deaths2006 + BirdFlu$deaths2007 + BirdFlu$deaths2008
BirdFlu$TotalDeaths  # => Indonesia là quốc gia có nhiều ca tử vong nhất = 110



