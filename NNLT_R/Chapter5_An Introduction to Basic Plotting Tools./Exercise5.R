# Đọc file Amphibian_road_Kills.xls
library(readxl)
RK <- read_excel("Amphibian_road_Kills.xls")
View(RK)
# Tên và dữ liệu các cột
names(RK)
str(RK)

# Biến, TOT_N, là số lượng động vật chết tại từng điểm lấy mẫu,
# OLIVE là số vườn ô liu tại địa điểm lấy mẫu
# D_Park là khoảng cách từ điểm lấy mẫu đến công viên gần đó.
# xây dựng plot với x=D.PARK và y=TOT.N
plot(x = RK$D.PARK, y = RK$TOT.N, 
     xlab = "Khoảng cách từ điểm lấy mẫu đến công viên gần đó",
     ylab = "Số lượng động vật chết tại điểm lấy mẫu")

# Thêm đường cong làm mịn
M.Loess <- loess(TOT.N ~ D.PARK, data = RK) 
Fit <- fitted(M.Loess)
Ord1 <- order(RK$D.PARK)
lines(RK$D.PARK[Ord1], Fit[Ord1],
      lwd = 5, lty = 3)

# Thực hiện lại plot nhưng sử dụng các điểm tỉ lệ với giá trị của OLIVE
plot(x = RK$D.PARK, y = RK$TOT.N, cex = 2 + 10 *RK$OLIVE / max(RK$OLIVE),
     xlab = "Khoảng cách từ điểm lấy mẫu đến công viên gần đó",
     ylab = "Số lượng động vật chết tại điểm lấy mẫu",
     pch = 20)
